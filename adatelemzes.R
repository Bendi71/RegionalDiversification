library(quantmod)
library(igraph)
library(plyr)
library(dplyr)
library(PerformanceAnalytics)
library(tseries)
library(rugarch)
library(rmgarch)
library(dtwclust)
library(onewaytests)

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# ---------- 0) Import and clean data ----------

startDate <- as.Date("2012-01-12")
endDate <- as.Date("2025-08-31")

read_local_series <- function(path, col_names, start_date, end_date) {
  df <- read.csv(path, header = TRUE, sep = ",")
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  df <- df[df$Date >= start_date & df$Date <= end_date, ]
  colnames(df) <- col_names
  df
}

BUX <- read_local_series(
  path = "bux_d.csv",
  col_names = c("Date", "Open", "BUX.High", "BUX.Low", "BUX.Close", "Volume"),
  start_date = startDate,
  end_date = endDate
)

WIG <- read_local_series(
  path = "wig20_d.csv",
  col_names = c("Date", "Open", "WIG.High", "WIG.Low", "WIG.Close", "Volume"),
  start_date = startDate,
  end_date = endDate
)

PX <- read_local_series(
  path = "PX_d.csv",
  col_names = c("Date", "Open", "PX.High", "PX.Low", "PX.Close", "Volume"),
  start_date = startDate,
  end_date = endDate
)

gold <- read_local_series(
  path = "xaueur_d.csv",
  col_names = c("Date", "Open", "XAU.High", "XAU.Low", "XAU.Close"),
  start_date = startDate,
  end_date = endDate
)


index_tickers <- c("^FCHI", "^FTSE", "^GDAXI", "^SSMI", "^OMX", "^IBEX", "URTH",
                   "^ATX", "^AEX", "FTSEMIB.MI", "CL=F")

currencies <- c("CHFEUR=X", "SEKEUR=X", "EUR=X", "GBPEUR=X", "HUFEUR=X", "PLNEUR=X", "CZKEUR=X"
)

index_hist <- new.env()
getSymbols(index_tickers, env = index_hist, src = "yahoo", from = startDate, to = endDate)

currency_hist <- new.env()
getSymbols(currencies, env = currency_hist, src = "yahoo", from = startDate, to = endDate)

# for each table in currency list, keep only close
currency_hist <- eapply(currency_hist, function(x) Cl(x))

# check the date range for each currency in currency_hist
for (currency_name in names(index_hist)) {
  currency_data <- index_hist[[currency_name]]
  date_range <- index(currency_data)
  cat(paste("Currency:", currency_name, "Date Range:", min(date_range), "to", max(date_range), "\n"))
}

index_hist$BUX <- xts(BUX[,-1], order.by = BUX$Date)
index_hist$WIG <- xts(WIG[,-1], order.by = WIG$Date)
index_hist$PX <- xts(PX[,-1], order.by = PX$Date)
index_hist$XAU <- xts(gold[,-1], order.by = gold$Date)

index_hist <- eapply(index_hist, function(x) Cl(x))

# convert to euro
currency_map <- list(
  "FTSE" = "GBPEUR=X",
  "SSMI" = "CHFEUR=X",
  "OMX" = "SEKEUR=X",
  "URTH" = "EUR=X",
  "BUX" = "HUFEUR=X",
  "WIG" = "PLNEUR=X",
  "PX" = "CZKEUR=X",
  "CL=F" = "EUR=X"
)

index_fx = list()
for (index_name in names(index_hist)) {
  if (index_name %in% names(currency_map)) {
    currency_name <- currency_map[[index_name]]
    
    # FX data
    fx_data <- currency_hist[[currency_name]]
    
    # Align FX data to index dates
    aligned_fx_data <- merge(index_hist[[index_name]], fx_data, join = "left")  # Align FX to index dates
    
    # Fill missing FX values with the previous day's value
    aligned_fx_data <- na.locf(aligned_fx_data, na.rm = FALSE)
    
    # Extract the aligned FX column (assuming it's named "fx_data.Close")
    fx_rates <- aligned_fx_data[, ncol(aligned_fx_data)]
    
    # Perform FX conversion
    index_fx[[index_name]] <- index_hist[[index_name]] * fx_rates
  } else {
    index_fx[[index_name]] <- index_hist[[index_name]]
  }
}

# Check for values in every index
missingValues <- lapply(index_fx, function(x) sum(is.na(x)))
missingValues

merger <- function(x,y) merge.xts(x,y)
index_df <- Reduce(merger, index_fx)
index_df <- na.omit(index_df)
colnames(index_df)[colnames(index_df) == "CL.F.Close"] <- "OIL.Close"
# calculate return on each index
returnFunc <- function(x) {
  diff(log(x))
}
index_return <- lapply(index_df, returnFunc)
index_return <- Reduce(merger, index_return)
index_return <- na.omit(index_return)

new_colnames <- sapply(colnames(index_return),
                       function(x) substr(x,1,nchar(x)-6))
colnames(index_return) <- new_colnames

#cumsum index returns for a time series plot
index_sum <- apply(index_return, 2, cumsum)

chart.TimeSeries(index_sum[, !colnames(index_sum) %in% c("XAU", "OIL")]*100,
                 auto.grid = FALSE,
                 lwd = 1.5,
                 date.format	= "%Y-%m-%d",
                 legend.loc = "topleft",
                 cex.legend = 0.8,
                 colorset = RColorBrewer::brewer.pal(n = ncol(index_sum), name = "Set3"),
                 element.color = "gray",
                 main = "",
                 major.ticks = "years",
                 minor.ticks = "months"
)

# everything minus XAU and OIL
rets_xts <- index_return[, !colnames(index_return) %in% c("XAU", "OIL")]
n_obs <- nrow(rets_xts)
n_assets <- ncol(rets_xts)
cat("Observations:", n_obs, "Assets:", n_assets, "\n")

ConnectednessApproach::SummaryStatistics(index_return)
#export descriptives
write.csv(ConnectednessApproach::SummaryStatistics(index_return*100), "descr_stats_pct.csv")
hist(index_return$OIL)

# ---------- 1) Stationarity tests ----------
# Jarque-Bera wrapper from tseries or stats
jarque_test <- function(x){
  if(all(is.na(x))) return(NA)
  tryCatch(tseries::jarque.bera.test(as.numeric(x))$p.value, error = function(e) NA)
}
adf_test_p <- function(x){
  tryCatch(tseries::adf.test(as.numeric(x))$p.value, error = function(e) NA)
}
kpss_test_p <- function(x){
  tryCatch(tseries::kpss.test(as.numeric(x))$p.value, error = function(e) NA)
}
arch_test_p <- function(x, lags = 12){
  tryCatch(FinTS::ArchTest(as.numeric(x), lags = lags)$p.value, error = function(e) NA)
}

stationarity_results <- data.frame(asset = colnames(rets_xts),
                                   ADF_p = NA,
                                   KPSS_p = NA,
                                   JB_p = NA,
                                   stringsAsFactors = FALSE)
for(i in seq_len(n_assets)){
  x <- na.omit(coredata(rets_xts[, i]))
  stationarity_results$ADF_p[i]  <- adf_test_p(x)
  stationarity_results$KPSS_p[i] <- kpss_test_p(x)
  stationarity_results$JB_p[i]   <- jarque_test(x)
}
print("Stationarity & Normality (p-values):")
print(stationarity_results)



# ---------- 2) ARCH/GARCH effect tests ----------
arch_results <- data.frame(asset = colnames(rets_xts), ARCH_p_5 = NA, ARCH_p_12 = NA, stringsAsFactors = FALSE)
for(i in seq_len(n_assets)){
  x <- na.omit(coredata(rets_xts[, i]))
  arch_results$ARCH_p_5[i]  <- arch_test_p(x, lags = 5)
  arch_results$ARCH_p_12[i] <- arch_test_p(x, lags = 12)
  arch_results$ARCH_p_20[i] <- arch_test_p(x, lags = 20)
}
print("ARCH test p-values:")
print(arch_results)


# ---------- Clustering ----------
# Convert returns to matrix or list of series
# dtwclust prefers a list or matrix where each row is a time series

series_list <- lapply(as.data.frame(rets_xts), as.numeric)
names(series_list) <- colnames(rets_xts)

# Normalize each series (optional but recommended)
series_list <- zscore(series_list)

hc_dtw <- tsclust(series_list,
                  type = "hierarchical",
                  k = 5,
                  distance = "dtw",
                  control = hierarchical_control(method = "ward.D2"))

plot(hc_dtw, labels = colnames(rets_xts), ann = FALSE)
title(ylab="Távolság")

clusters <- cutree(hc_dtw, k = 5)
print(clusters)

# Create groups based on clusters
grouped_assets <- split(names(clusters), clusters)

# Name groups
names(grouped_assets) <- c("KE", "Világpiac", "Magországok", "KKE", "NET")

aggregate_group_returns <- function(rets_xts, grouped_assets, weights = NULL){
  stopifnot(is.list(grouped_assets))
  asset_names <- colnames(rets_xts)
  
  group_returns <- list()
  
  for(g in names(grouped_assets)){
    members <- intersect(grouped_assets[[g]], asset_names)
    if(length(members) == 0){
      warning("Group ", g, " has no matching assets; skipping.")
      next
    }
    subret <- rets_xts[, members, drop = FALSE]
    # set equal weights if not provided
    if(is.null(weights) || is.null(weights[[g]])){
      w <- rep(1 / length(members), length(members))
    } else {
      w <- weights[[g]]
      if(length(w) != length(members)) stop("Weight length mismatch in group ", g)
      w <- w / sum(w)
    }
    group_returns[[g]] <- xts(as.numeric(subret %*% w),
                              order.by = index(subret))
  }
  
  do.call(merge, group_returns)
}

group_rets_xts <- aggregate_group_returns(rets_xts, grouped_assets)

# ---------- 3) Model grid: DCC variants ----------
# Symmetric GARCH for DCC
uspec_dcc_base <- function(dist, meanOpt) {
  ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
    mean.model     = list(armaOrder = meanOpt$armaOrder, include.mean = TRUE),
    distribution.model = dist
  )
}

# Asymmetric GARCH for ADCC
uspec_adcc_base <- function(dist, meanOpt) {
  ugarchspec(
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
    mean.model     = list(armaOrder = meanOpt$armaOrder, include.mean = TRUE),
    distribution.model = dist
  )
}

# Configurable model choices
#, "norm", "std"
uni_dist_choices <- c("ged")  # univariate error distributions to try
mv_dist_choices  <- c("mvnorm","mvt")             # multivariate distributions in dccspec
mean_options     <- list(noAR = list(armaOrder = c(0,0)), AR1 = list(armaOrder = c(1,0)))
dcc_type <- c("DCC","ADCC")
dcc_models <- list()  # will store specs keyed by name

# Build specs for each combination
for(ud in uni_dist_choices){
  for(md in mv_dist_choices){
    for(mn in names(mean_options)){
      # symmetric baseline for DCC
      uspec_dcc  <- multispec(replicate(n_assets, uspec_dcc_base(ud, mean_options[[mn]])))
      # asymmetric baseline for ADCC
      uspec_adcc <- multispec(replicate(n_assets, uspec_adcc_base(ud, mean_options[[mn]])))
      
      name_dcc  <- paste("u", ud, "mv", md, mn, "DCC", sep = "_")
      name_adcc <- paste("u", ud, "mv", md, mn, "ADCC", sep = "_")
      
      dcc_models[[name_dcc]]  <- list(uspec = uspec_dcc,  mvdist = md, model_type = "DCC",  desc = name_dcc)
      dcc_models[[name_adcc]] <- list(uspec = uspec_adcc, mvdist = md, model_type = "ADCC", desc = name_adcc)
    }
  }
}
cat("Built", length(dcc_models), "candidate specifications\n")


# ---------- Rolling 1-step forecast and loss calculation function ----------
# Realized covariance estimator: rolling window of last K days (use 5-day realized cov)
initial.window <- floor(0.7 * n_obs)     # first estimation window
forecast.length <- n_obs - initial.window
realized_window <- 5

compute_roll_losses <- function(dcc_spec_base, model_type = "DCC"){
  spec <- dccspec(uspec = dcc_spec_base$uspec,
                  dccOrder = c(1,1),
                  distribution = dcc_spec_base$mvdist,
                  model = ifelse(model_type == "ADCC", "aDCC", "DCC"))
  
  cat("Running dccroll for", dcc_spec_base$desc, "model", model_type, "\n")
  roll <- tryCatch(
    dccroll(spec, data = rets_xts, n.ahead = 1,
            forecast.length = n_obs - initial.window, refit.every = 20,
            refit.window = "moving",
            window.size = initial.window,
            fit.control = list(eval.se = FALSE)),
    error = function(e) { message("dccroll failed: ", e$message); return(NULL) }
  )
  if (is.null(roll)) return(NULL)
  
  rcov_arr <- rcov(roll)
  dims <- dim(rcov_arr)
  
  # unify shape to [N, N, nroll]
  if (length(dims) == 4) {
    # shape [N, N, 1, nroll]
    rcov_arr <- rcov_arr[, , 1, , drop = FALSE]
  }
  
  n_assets <- dim(rcov_arr)[1]
  n_rolls  <- dim(rcov_arr)[3]
  losses <- rep(NA_real_, n_rolls)
  
  for (r in seq_len(n_rolls)) {
    t_idx <- initial.window + r
    if (t_idx <= realized_window) next
    Hf <- rcov_arr[, , r]
    realized_idx_start <- t_idx - (realized_window - 1)
    realized_idx_end   <- t_idx
    if (realized_idx_start < 1) next
    realized_cov <- cov(coredata(rets_xts[realized_idx_start:realized_idx_end, ]),
                        use = "pairwise.complete.obs")
    losses[r] <- sqrt(sum((Hf - realized_cov)^2))
  }
  
  list(roll = roll, losses = losses)
}

# ---------- Fit all candidate models (both DCC and ADCC) with rolling forecasts ----------
results_list <- list()
for(name in names(dcc_models)){
  base <- dcc_models[[name]]
  out <- compute_roll_losses(base, model_type = base$model_type)
  results_list[[name]] <- out
}

# Remove failed fits and build loss matrix
valid_models <- names(results_list)[sapply(results_list, function(x) !is.null(x) && !is.null(x$losses))]
cat("Valid models:", length(valid_models), "\n")
loss_matrix <- do.call(cbind, lapply(valid_models, function(m) {
  L <- results_list[[m]]$losses
  return(L)
}))
colnames(loss_matrix) <- valid_models
rownames(loss_matrix) <- paste0("t+", seq_len(test_n))

# Remove rows that are all NA (edge alignment issues)
keep_rows <- apply(loss_matrix, 1, function(r) any(!is.na(r)))
loss_matrix <- loss_matrix[keep_rows, , drop = FALSE]

# ---------- 3) Model Confidence Set ----------
# Use MCS procedure on the loss matrix.
set.seed(123)
mcs_out <- tryCatch({
  MCS::MCSprocedure(as.matrix(loss_matrix), alpha = 0.10, B = 1000, statistic = "Tmax")
}, error = function(e){
  message("MCS failed: ", e$message)
  return(NULL)
})
if(!is.null(mcs_out)){
  cat("MCS selected models:\n")
  print(mcs_out@MCS)  # names of models in the final set
  best_models <- as.character(mcs_out@MCS)
} else {
  best_models <- valid_models[1]  # fallback
  cat("MCS failed; using fallback model:", best_models, "\n")
}

chosen_model <- "u_ged_mv_mvnorm_noAR_ADCC"
chosen_model_adcc2 <- "u_ged_mv_mvt_noAR_ADCC"
chosen_model_dcc <- "u_ged_mv_mvnorm_noAR_DCC"

# ---------- 4) One-step-ahead dynamic conditional correlations using rolling window approach ----------
# Re-run dccroll for the chosen model to extract correlations. Use refit.every = 20 as requested.
# The chosen_model string corresponds to results_list key; extract its roll object if present, else re-run.
chosen_roll <- NULL
if(!is.null(results_list[[chosen_model]]$roll)){
  chosen_roll <- results_list[[chosen_model]]$roll
} else {
  base <- dcc_models[[chosen_model]]
  if(!is.null(base)){
    spec <- dccspec(uspec = base$uspec, dccOrder = c(1,1), distribution = base$mvdist,
                    model = ifelse(model_type_ch == "ADCC", "aDCC", "DCC"))
    chosen_roll <- dccroll(spec, data = rets_xts, n.ahead = 1, forecast.length = n_obs - initial.window,
                           refit.every = 20, refit.window = "moving", fit.control = list(eval.se = FALSE))
  } else stop("Cannot reconstruct chosen model spec.")
}

# Extract dynamic correlations for each forecasted time
rcor_arr <- tryCatch(rcor(chosen_roll), error = function(e) NULL)
# rcor_arr dims: [nAssets, nAssets, n.ahead (=1), nroll] or [nAssets,nAssets,nroll]
if(is.null(rcor_arr)){
  message("Failed to extract rcor from roll object.")
} else {
  # collect 1-step ahead correlation matrices per rolling step
  dims <- dim(rcor_arr)
  # handle possible shapes
  if(length(dims) == 4){
    nrolls <- dims[4]
    corr_list <- lapply(seq_len(nrolls), function(i) rcor_arr[,,1,i])
  } else if(length(dims) == 3){
    nrolls <- dims[3]
    corr_list <- lapply(seq_len(nrolls), function(i) rcor_arr[,,i])
  } else stop("Unexpected rcor dimensions.")
  cat("Extracted", length(corr_list), "one-step-ahead correlation matrices from rolling forecasts.\n")
  # Example: show correlation between asset1 and asset2 over test period
  if(n_assets >= 2){
    r12 <- sapply(corr_list, function(M) M[1,2])
    names(r12) <- paste0("t+", seq_along(r12))
    print(head(r12, 10))
  }
}

# ---------- 5) Final full-sample DCC and ADCC estimation ----------
# Fit both on full sample with the chosen univariate distribution and mean structure from chosen_model
# parse chosen_model to get base
base <- dcc_models[[chosen_model]]
if(is.null(base)) stop("Error: chosen model base not found.")

# Get data for last window
last_window_start <- max(1, nrow(rets_xts) - initial.window - 20 + 1)
last_window_end <- nrow(rets_xts) - 20
last_window <- if (last_window_end > last_window_start) {
  rets_xts[last_window_start:last_window_end, ]
} else {
  rets_xts
}


spec_adcc_full <- dccspec(uspec = base$uspec, dccOrder = c(1,1), distribution = base$mvdist, model = "aDCC")
fit_adcc_full <- tryCatch(dccfit(spec_adcc_full, data = last_window, fit.control = list(eval.se = TRUE)), error = function(e) { message("adcc dccfit failed: ", e$message); NULL })

# order df columns by a specific order
rets_ordered <- rets_xts[, c(1,2,3,4,5,8,11,12,6,9,7,10,13)]
fdcc_spec <- dccspec(uspec = base$uspec, dccOrder = c(1,1), distribution = base$mvdist, model = "FDCC", groups=c(1,1,2,3,3,3,3,3,4,4,5,5,5))
fdcc_fit <- dccfit(fdcc_spec, data=rets_ordered,  fit.control = list(eval.se = TRUE))

# ---------- Plot pairwise one-step-ahead DCC vs ADCC dynamic correlations ----------

if (!is.null(rcor_arr)) {
  T_len <- dim(rcor_arr)[3]
  asset_names <- colnames(rets_xts)
  time_index <- index(rets_xts)[(nrow(rets_xts) - T_len + 1):nrow(rets_xts)]
  
  # Create directory for plots
  ensure_dir("dcc_plots")
  
  # Loop over all unique pairs i<j
  for (i in seq_len(n_assets - 1)) {
    for (j in (i + 1):n_assets) {
      corr_dcc  <- rcor_arr[i, j, ]
      png(sprintf("dcc_plots/ADCC_%s_%s.png",
                  asset_names[i], asset_names[j]), width = 1200, height = 700)
      plot(time_index, corr_dcc, type = "l", col = "darkblue", lwd = 2.5,
           main = sprintf("%s - %s", asset_names[i], asset_names[j]),
           xlab = "", ylab = ""
           #, ylim = range(df_plot[, 2:3], na.rm = TRUE)
      )
      dev.off()
    }
  }
  cat("Saved ADCC correlation plots to ./dcc_plots/\n")
} else {
  cat("Cannot plot DCC vs ADCC correlations: fits missing.\n")
}

# ---------- Create grouped DCC vs ADCC dynamic correlations ----------

aggregate_correlation_groups <- function(Sigma_array, groups, weights = NULL) {
  # Sigma_array: 3D array [N x N x T] of conditional covariances
  # groups: list of vectors (each either integer or character asset identifiers)
  # weights: optional list of numeric vectors matching groups (same length as each group)
  # Returns: 3D array [K x K x T] of group-level dynamic correlations
  
  N <- dim(Sigma_array)[1]
  if (dim(Sigma_array)[2] != N) stop("Sigma_array must be square in first two dimensions.")
  Tlen <- dim(Sigma_array)[3]
  K <- length(groups)
  
  asset_names <- dimnames(Sigma_array)[[1]]
  if (is.null(asset_names)) stop("Sigma_array must have row/column names if groups use character labels.")
  
  # Map group names to indices if character
  groups_idx <- lapply(groups, function(g) {
    if (is.character(g)) {
      idx <- match(g, asset_names)
      if (any(is.na(idx))) stop(paste("Some group names not found in Sigma_array dimnames:", paste(g[is.na(idx)], collapse=", ")))
      return(idx)
    } else if (is.numeric(g)) {
      return(g)
    } else {
      stop("Each group must be a character or numeric vector.")
    }
  })
  
  # Default equal weights per group
  if (is.null(weights)) {
    weights <- lapply(groups_idx, function(g) rep(1 / length(g), length(g)))
  }
  
  # Normalize weights per group and check lengths
  for (k in seq_along(groups_idx)) {
    if (length(weights[[k]]) != length(groups_idx[[k]])) {
      stop(paste("Length of weights[[", k, "]] does not match length of groups[[", k, "]]", sep=""))
    }
    weights[[k]] <- weights[[k]] / sum(weights[[k]])
  }
  
  # Initialize output array
  agg_cor <- array(NA, dim = c(K, K, Tlen),
                   dimnames = list(names(groups), names(groups), NULL))
  
  for (t in 1:Tlen) {
    Sigma_t <- Sigma_array[, , t]
    cov_mat <- matrix(NA, K, K)
    
    # Precompute group variances
    group_vars <- numeric(K)
    group_w <- list()
    for (a in seq_len(K)) {
      wa <- numeric(N)
      wa[groups_idx[[a]]] <- weights[[a]]
      group_w[[a]] <- wa
      group_vars[a] <- as.numeric(t(wa) %*% Sigma_t %*% wa)
    }
    
    # Compute correlations between groups
    for (a in seq_len(K)) {
      for (b in a:K) {
        cov_ab <- as.numeric(t(group_w[[a]]) %*% Sigma_t %*% group_w[[b]])
        rho_ab <- cov_ab / sqrt(group_vars[a] * group_vars[b])
        cov_mat[a, b] <- cov_mat[b, a] <- rho_ab
      }
    }
    diag(cov_mat) <- 1
    agg_cor[, , t] <- cov_mat
  }
  
  return(agg_cor)
}

aggregate_covariance_groups <- function(Sigma_array, groups, weights = NULL) {
  # Sigma_array: 3D array [N x N x T] of conditional covariances
  # groups: list of vectors (each either integer or character asset identifiers)
  # weights: optional list of numeric vectors matching groups (same length as each group)
  # Returns: 3D array [K x K x T] of group-level conditional covariances
  
  N <- dim(Sigma_array)[1]
  if (dim(Sigma_array)[2] != N) stop("Sigma_array must be square in first two dimensions.")
  Tlen <- dim(Sigma_array)[3]
  K <- length(groups)
  
  asset_names <- dimnames(Sigma_array)[[1]]
  if (is.null(asset_names)) stop("Sigma_array must have row/column names if groups use character labels.")
  
  # Map group names to indices if character
  groups_idx <- lapply(groups, function(g) {
    if (is.character(g)) {
      idx <- match(g, asset_names)
      if (any(is.na(idx))) stop(
        paste("Some group names not found in Sigma_array dimnames:",
              paste(g[is.na(idx)], collapse = ", "))
      )
      return(idx)
    } else if (is.numeric(g)) {
      return(g)
    } else stop("Each group must be a character or numeric vector.")
  })
  
  # Default equal weights per group
  if (is.null(weights)) {
    weights <- lapply(groups_idx, function(g) rep(1 / length(g), length(g)))
  }
  
  # Normalize weights per group and check lengths
  for (k in seq_along(groups_idx)) {
    if (length(weights[[k]]) != length(groups_idx[[k]]))
      stop(paste("Length of weights[[", k, "]] does not match length of groups[[", k, "]]", sep = ""))
    weights[[k]] <- weights[[k]] / sum(weights[[k]])
  }
  
  # Initialize output array
  agg_cov <- array(NA, dim = c(K, K, Tlen),
                   dimnames = list(names(groups), names(groups), NULL))
  
  for (t in seq_len(Tlen)) {
    Sigma_t <- Sigma_array[, , t]
    cov_mat <- matrix(NA, K, K)
    
    group_w <- vector("list", K)
    for (a in seq_len(K)) {
      wa <- numeric(N)
      wa[groups_idx[[a]]] <- weights[[a]]
      group_w[[a]] <- wa
    }
    
    for (a in seq_len(K)) {
      for (b in a:K) {
        cov_ab <- as.numeric(t(group_w[[a]]) %*% Sigma_t %*% group_w[[b]])
        cov_mat[a, b] <- cov_mat[b, a] <- cov_ab
      }
    }
    agg_cov[, , t] <- cov_mat
  }
  
  return(agg_cov)
}

group_rets_xts <- aggregate_group_returns(rets_xts, grouped_assets)
grouped_covs <- aggregate_covariance_groups(rcov(chosen_roll), grouped_assets)
grouped_corrs <- aggregate_correlation_groups(rcor(chosen_roll), grouped_assets)
dims <- dim(grouped_corrs)
K <- dims[1]
T_len <- dims[3]
group_names <- dimnames(grouped_corrs)[[1]]

ensure_dir("group_corr_plots")

for (i in seq_len(K - 1)) {
  for (j in (i + 1):K) {
    rho_ij <- grouped_corrs[i, j, ]
    if (all(!is.finite(rho_ij))) next
    df_plot <- data.frame(Date = time_index, Corr = as.numeric(rho_ij))
    ylims <- range(df_plot$Corr, finite = TRUE)
    if (!all(is.finite(ylims))) next
    
    png(sprintf("group_corr_plots/GroupCorr_%s_%s.png",
                group_names[i], group_names[j]), width = 1200, height = 700)
    plot(df_plot$Date, df_plot$Corr, type = "l", col = "darkblue", lwd = 2.5,
         main = sprintf("%s – %s",
                        group_names[i], group_names[j]),
         xlab = "", ylab = "", ylim = ylims, cex.main = 2, cex.axis = 1.8, cex.lab = 1.8)
    abline(h = 0, col = "gray70", lty = 3)
    dev.off()
  }
}

cat("Saved grouped dynamic correlation plots to ./group_corr_plots/\n")

# ---------- 6) Hedge Effectiveness ----------
rcov_adcc <- rcov(chosen_roll)
# if 4D, collapse 3rd dim = 1
if(length(dim(rcov_adcc)) == 4) rcov_adcc <- rcov_adcc[,,1,]

N <- dim(rcov_adcc)[1]
T_len <- dim(rcov_adcc)[3]
asset_names <- colnames(rets_xts)
# align returns to rcov time dimension (assume last T_len rows correspond)
rets_mat <- coredata(rets_xts)
rets_aligned <- rets_mat[(nrow(rets_mat)-T_len+1):nrow(rets_mat), , drop = FALSE]
time_index <- index(rets_xts)[(nrow(rets_xts)-T_len+1):nrow(rets_xts)]

# containers
rows <- list()
HedgeRatios_adcc <- list()

for(i in seq_len(N)){
  for(j in seq_len(N)){
    if(i == j) next
    
    h_ij_adcc <- sapply(seq_len(T_len), function(t) rcov_adcc[i,j,t])
    h_jj_adcc <- sapply(seq_len(T_len), function(t) rcov_adcc[j,j,t])
    beta_adcc <- h_ij_adcc / h_jj_adcc
    
    # clean NAs (synchronised)
    ok_idx <- which(!is.na(beta_adcc) &
                      !is.na(rets_aligned[, i]) & !is.na(rets_aligned[, j]))
    if(length(ok_idx) < 10){
      # store NAs if too short
      rows[[length(rows) + 1]] <- data.frame(
        asset_i = asset_names[i], asset_j = asset_names[j],
        mean_adcc = NA, sd_adcc = NA, he_adcc = NA,
        F_p = NA, BF_p = NA, T_test=NA, stringsAsFactors = FALSE
      )
      next
    }
    
    beta_adcc_ok <- beta_adcc[ok_idx]
    ri <- rets_aligned[ok_idx, i]
    rj <- rets_aligned[ok_idx, j]
    
    # hedged portfolio returns: r_i - beta_t * r_j
    hedged_adcc <- ri - beta_adcc_ok * rj
    
    # statistics
    mean_adcc <- mean(beta_adcc_ok, na.rm = TRUE)
    sd_adcc   <- sd(beta_adcc_ok, na.rm = TRUE)
    var_ri <- var(ri, na.rm = TRUE)
    var_hedged_adcc <- var(hedged_adcc, na.rm = TRUE)
    he_adcc <- as.numeric(1 - var_hedged_adcc / var_ri)
    
    # F-test for equality of variances of hedge-ratio series
    f_p_adcc <- tryCatch(var.test(ri, hedged_adcc, alternative = "greater")$p.value, error = function(e) NA_real_)
    # Brown-Forsythe via onewaytests::bf.test using combined vector + group
    z1 <- abs(ri - median(ri))
    z2 <- abs(hedged_adcc - median(hedged_adcc))
    group <- factor(c(rep("unhedged", length(z1)), rep("hedged", length(z2))))
    z_all <- c(z1, z2)
    BF_p_adcc <- anova(lm(z_all ~ group))$`Pr(>F)`[1]
    t_test <- t.test(z1, z2, alternative = "greater")
    
    rows[[length(rows) + 1]] <- data.frame(
      asset_i = asset_names[i], asset_j = asset_names[j],
      mean_adcc = mean_adcc, sd_adcc = sd_adcc, he_adcc = he_adcc,
      F_p_adcc = f_p_adcc, BF_p_adcc = BF_p_adcc, T_test = t_test$p.value,
      stringsAsFactors = FALSE
    )
    
    # save time series optionally
    HedgeRatios_adcc[[paste(asset_names[i], asset_names[j], sep = "_")]] <- zoo(beta_adcc, time_index)
  }
}

hedge_stats <- do.call(rbind, rows)

K <- dim(grouped_covs)[1]
T_len <- dim(grouped_covs)[3]
group_names <- dimnames(grouped_covs)[[1]]
time_index <- index(group_rets_xts)[(nrow(group_rets_xts) - T_len + 1):nrow(group_rets_xts)]
rets_mat <- coredata(group_rets_xts)[(nrow(group_rets_xts) - T_len + 1):nrow(group_rets_xts), ]

rows <- list()
HedgeRatios_grouped <- list()

for (i in seq_len(K)) {
  for (j in seq_len(K)) {
    if (i == j) next
    # Dynamic hedge ratios
    h_ij <- sapply(seq_len(T_len), function(t) grouped_covs[i, j, t])
    h_jj <- sapply(seq_len(T_len), function(t) grouped_covs[j, j, t])
    beta <- h_ij / h_jj
    
    ok_idx <- which(is.finite(beta) & is.finite(rets_mat[, i]) & is.finite(rets_mat[, j]))
    if (length(ok_idx) < 10) next
    
    beta_ok <- beta[ok_idx]
    ri <- rets_mat[ok_idx, i]
    rj <- rets_mat[ok_idx, j]
    hedged <- ri - beta_ok * rj
    
    # Summary statistics
    mean_beta <- mean(beta_ok)
    sd_beta   <- sd(beta_ok)
    var_ri    <- var(ri)
    var_hedged <- var(hedged)
    he <- 1 - var_hedged / var_ri
    
    # F-test (variance reduction)
    F_p <- tryCatch(var.test(ri, hedged, alternative = "greater")$p.value, error = function(e) NA_real_)
    
    # Brown–Forsythe one-sided test
    df_test <- data.frame(
      value = c(ri, hedged),
      group = factor(rep(c("unhedged", "hedged"), c(length(ri), length(hedged))))
    )
    z1 <- abs(ri - median(ri))
    z2 <- abs(hedged - median(hedged))
    group <- factor(c(rep("unhedged", length(z1)), rep("hedged", length(z2))))
    z_all <- c(z1, z2)
    BF_p_adcc <- anova(lm(z_all ~ group))$`Pr(>F)`[1]
    t_test <- t.test(z1, z2, alternative = "greater")
    
    rows[[length(rows) + 1]] <- data.frame(
      group_i = group_names[i],
      group_j = group_names[j],
      hedge_return = sum(hedged),
      mean_beta = mean_beta,
      sd_beta = sd_beta,
      hedging_effectiveness = he,
      F_p = F_p,
      BF_p = BF_p_adcc,
      T_test = t_test$p.value,
      stringsAsFactors = FALSE
    )
    
    HedgeRatios_grouped[[paste(group_names[i], group_names[j], sep = "_")]] <-
      zoo(beta, time_index)
  }
}

hedge_stats_grouped <- do.call(rbind, rows)

rows <- list()
Weights_grouped <- list()

for (i in seq_len(K)) {
  for (j in seq_len(K)) {
    if (i == j) next
    # Dynamic hedge ratios
    h_ii <- sapply(seq_len(T_len), function(t) grouped_covs[i, i, t])
    h_ij <- sapply(seq_len(T_len), function(t) grouped_covs[i, j, t])
    h_jj <- sapply(seq_len(T_len), function(t) grouped_covs[j, j, t])
    weight <- (h_jj-h_ij)/(h_ii-2*h_ij+h_jj)
    weight <- pmin(pmax(weight, 0), 1)
    
    ok_idx <- which(is.finite(weight) & is.finite(rets_mat[, i]) & is.finite(rets_mat[, j]))
    if (length(ok_idx) < 10) next
    
    weight_ok <- weight[ok_idx]
    ri <- rets_mat[ok_idx, i]
    rj <- rets_mat[ok_idx, j]
    hedged <- weight_ok*ri + (1-weight_ok) * rj
    
    # Summary statistics
    mean_beta <- mean(weight_ok)
    sd_beta   <- sd(weight_ok)
    var_ri    <- var(ri)
    var_hedged <- var(hedged)
    he <- 1 - var_hedged / var_ri
    
    # F-test (variance reduction)
    F_p <- tryCatch(var.test(ri, hedged, alternative = "greater")$p.value, error = function(e) NA_real_)
    
    # Brown–Forsythe one-sided test
    df_test <- data.frame(
      value = c(ri, hedged),
      group = factor(rep(c("unhedged", "hedged"), c(length(ri), length(hedged))))
    )
    z1 <- abs(ri - median(ri))
    z2 <- abs(hedged - median(hedged))
    group <- factor(c(rep("unhedged", length(z1)), rep("hedged", length(z2))))
    z_all <- c(z1, z2)
    BF_p_adcc <- anova(lm(z_all ~ group))$`Pr(>F)`[1]
    t_test <- t.test(z1, z2, alternative = "greater")
    
    rows[[length(rows) + 1]] <- data.frame(
      group_i = group_names[i],
      group_j = group_names[j],
      mean_weight = mean_beta,
      sd_weight = sd_beta,
      hedging_effectiveness = he,
      F_p = F_p,
      BF_p = BF_p_adcc,
      T_test = t_test$p.value,
      stringsAsFactors = FALSE
    )
    
    Weights_grouped[[paste(group_names[i], group_names[j], sep = "_")]] <-
      zoo(weight, time_index)
  }
}

weight_stats_grouped <- do.call(rbind, rows)

# ---------- Plot one-step-ahead dynamic hedge ratios: DCC vs ADCC ----------

if (!exists("HedgeRatios_dcc") || !exists("HedgeRatios_adcc"))
  stop("Hedge ratio lists not found")

ensure_dir("hedge_plots")

for (nm in names(HedgeRatios_adcc)) {
  hr_adcc <- HedgeRatios_adcc[[nm]]
  hr_adcc <- hr_adcc[!duplicated(index(hr_adcc))]
  df <- data.frame(
    Date = as.Date(index(hr_adcc)),
    ADCC = hr_adcc
  )
  
  # finite limits
  ylims <- range(df[, c("ADCC")], finite = TRUE)
  if (!all(is.finite(ylims))) next
  
  png(sprintf("hedge_plots/HedgeRatio_ADCC_%s.png", nm),
      width = 1200, height = 700)
  plot(df$Date, df$ADCC, type = "l", col = "darkblue", lwd = 2.5,
       main = sprintf("%s", gsub("_", " – ", nm)),
       xlab = "", ylab = "", ylim = ylims)
  dev.off()
}

cat("Saved cleaned DCC vs ADCC hedge-ratio plots to ./hedge_plots/\n")

ensure_dir("hedge_plots_grouped")

for (nm in names(HedgeRatios_grouped)) {
  hr_adcc <- HedgeRatios_grouped[[nm]]
  hr_adcc <- hr_adcc[!duplicated(index(hr_adcc))]
  df <- data.frame(
    Date = as.Date(index(hr_adcc)),
    ADCC = hr_adcc
  )
  
  # finite limits
  ylims <- range(df[, c("ADCC")], finite = TRUE)
  if (!all(is.finite(ylims))) next
  
  png(sprintf("hedge_plots_grouped/HedgeRatio_%s.png", nm),
      width = 1200, height = 700)
  plot(df$Date, df$ADCC, type = "l", col = "darkblue", lwd = 2.5,
       main = sprintf("%s", gsub("_", " – ", nm)),
       xlab = "", ylab = "", ylim = ylims)
  dev.off()
}

ensure_dir("weight_plots_grouped")

for (nm in names(Weights_grouped)) {
  hr_adcc <- Weights_grouped[[nm]]
  hr_adcc <- hr_adcc[!duplicated(index(hr_adcc))]
  df <- data.frame(
    Date = as.Date(index(hr_adcc)),
    ADCC = hr_adcc
  )
  
  # finite limits
  ylims <- range(df[, c("ADCC")], finite = TRUE)
  if (!all(is.finite(ylims))) next
  
  png(sprintf("weight_plots_grouped/HedgeRatio_%s.png", nm),
      width = 1200, height = 700)
  plot(df$Date, df$ADCC, type = "l", col = "darkblue", lwd = 2.5,
       main = sprintf("%s", gsub("_", " – ", nm)),
       xlab = "", ylab = "", ylim = ylims, cex.main = 2, cex.axis = 1.8, cex.lab = 1.8)
  dev.off()
}


# ---------- Commodity hedging ----------

# add commodities, columns []
rets_com <- index_return
n_assets <- ncol(rets_com)
uspec_adcc <- multispec(replicate(n_assets, uspec_adcc_base("ged", list(armaOrder = c(0,0)))))
spec <- dccspec(uspec = uspec_adcc, dccOrder = c(1,1), distribution = "mvnorm",
                model = "ADCC")
commod_roll <- dccroll(spec, data = rets_com, n.ahead = 1, forecast.length = n_obs - initial.window,
                       refit.every = 20, refit.window = "moving", fit.control = list(eval.se = FALSE))

# add OIL and GOLD to grouped_assets
grouped_commod <- grouped_assets
grouped_commod$OLAJ <- "OIL"
grouped_commod$ARANY <- "XAU"
group_rets_xts <- aggregate_group_returns(rets_com, grouped_commod)
grouped_covs <- aggregate_covariance_groups(rcov(commod_roll), grouped_commod)
grouped_corrs <- aggregate_correlation_groups(rcor(commod_roll), grouped_commod)

HedgeRatios_commod <- list()
rows_commod <- list()

K <- dim(grouped_covs)[1]
T_len <- dim(grouped_covs)[3]
group_names <- dimnames(grouped_covs)[[1]]
time_index <- index(group_rets_xts)[(nrow(group_rets_xts) - T_len + 1):nrow(group_rets_xts)]
rets_mat <- coredata(group_rets_xts)[(nrow(group_rets_xts) - T_len + 1):nrow(group_rets_xts), ]


for (i in seq_len(K)) {
  for (j in seq_len(K)) {
    if (i == j) next
    # Dynamic hedge ratios
    h_ij <- sapply(seq_len(T_len), function(t) grouped_covs[i, j, t])
    h_jj <- sapply(seq_len(T_len), function(t) grouped_covs[j, j, t])
    beta <- h_ij / h_jj
    
    ok_idx <- which(is.finite(beta) & is.finite(rets_mat[, i]) & is.finite(rets_mat[, j]))
    if (length(ok_idx) < 10) next
    
    beta_ok <- beta[ok_idx]
    ri <- rets_mat[ok_idx, i]
    rj <- rets_mat[ok_idx, j]
    hedged <- ri - beta_ok * rj
    
    # Summary statistics
    mean_beta <- mean(beta_ok)
    sd_beta   <- sd(beta_ok)
    var_ri    <- var(ri)
    var_hedged <- var(hedged)
    he <- 1 - var_hedged / var_ri
    
    # F-test (variance reduction)
    F_p <- tryCatch(var.test(ri, hedged, alternative = "greater")$p.value, error = function(e) NA_real_)
    
    # Brown–Forsythe one-sided test
    df_test <- data.frame(
      value = c(ri, hedged),
      group = factor(rep(c("unhedged", "hedged"), c(length(ri), length(hedged))))
    )
    z1 <- abs(ri - median(ri))
    z2 <- abs(hedged - median(hedged))
    group <- factor(c(rep("unhedged", length(z1)), rep("hedged", length(z2))))
    z_all <- c(z1, z2)
    BF_p_adcc <- anova(lm(z_all ~ group))$`Pr(>F)`[1]
    t_test <- t.test(z1, z2, alternative = "greater")
    
    rows_commod[[length(rows_commod) + 1]] <- data.frame(
      group_i = group_names[i],
      group_j = group_names[j],
      hedged_return = sum(hedged),
      mean_beta = mean_beta,
      sd_beta = sd_beta,
      hedging_effectiveness = he,
      F_p = F_p,
      BF_p = BF_p_adcc,
      T_test = t_test$p.value,
      stringsAsFactors = FALSE
    )
    
    HedgeRatios_commod[[paste(group_names[i], group_names[j], sep = "_")]] <-
      zoo(beta, time_index)
  }
}

hedge_stats_commod <- do.call(rbind, rows_commod)

rows <- list()
Weights_commod <- list()

for (i in seq_len(K)) {
  for (j in seq_len(K)) {
    if (i == j) next
    # Dynamic hedge ratios
    h_ii <- sapply(seq_len(T_len), function(t) grouped_covs[i, i, t])
    h_ij <- sapply(seq_len(T_len), function(t) grouped_covs[i, j, t])
    h_jj <- sapply(seq_len(T_len), function(t) grouped_covs[j, j, t])
    weight <- (h_jj-h_ij)/(h_ii-2*h_ij+h_jj)
    weight[weight < 0] <- 0
    weight[weight > 1] <- 1
    
    ok_idx <- which(is.finite(weight) & is.finite(rets_mat[, i]) & is.finite(rets_mat[, j]))
    if (length(ok_idx) < 10) next
    
    weight_ok <- weight[ok_idx]
    ri <- rets_mat[ok_idx, i]
    rj <- rets_mat[ok_idx, j]
    hedged <- weight_ok*ri + (1-weight_ok) * rj
    
    # Summary statistics
    mean_beta <- mean(weight_ok)
    sd_beta   <- sd(weight_ok)
    var_ri    <- var(ri)
    var_hedged <- var(hedged)
    he <- 1 - var_hedged / var_ri
    
    # F-test (variance reduction)
    F_p <- tryCatch(var.test(ri, hedged, alternative = "greater")$p.value, error = function(e) NA_real_)
    
    # Brown–Forsythe one-sided test
    df_test <- data.frame(
      value = c(ri, hedged),
      group = factor(rep(c("unhedged", "hedged"), c(length(ri), length(hedged))))
    )
    z1 <- abs(ri - median(ri))
    z2 <- abs(hedged - median(hedged))
    group <- factor(c(rep("unhedged", length(z1)), rep("hedged", length(z2))))
    z_all <- c(z1, z2)
    BF_p_adcc <- anova(lm(z_all ~ group))$`Pr(>F)`[1]
    t_test <- t.test(z1, z2, alternative = "greater")
    
    rows[[length(rows) + 1]] <- data.frame(
      group_i = group_names[i],
      group_j = group_names[j],
      hedged_return = sum(hedged),
      mean_weight = mean_beta,
      sd_weight = sd_beta,
      hedging_effectiveness = he,
      F_p = F_p,
      BF_p = BF_p_adcc,
      T_test = t_test$p.value,
      stringsAsFactors = FALSE
    )
    
    Weights_commod[[paste(group_names[i], group_names[j], sep = "_")]] <-
      zoo(weight, time_index)
  }
}

weight_stats_commod <- do.call(rbind, rows)

# get unhedged return over the time (sum of returns of assets in each group) as array
unhedged_returns <- matrix(NA, nrow = 1, ncol = K)
for (i in seq_len(K)) {
  unhedged_returns[1, i] <- sum(rets_mat[, i])
}

# ---------- Value at risk ----------

#plot var from grouped covariances
VaR_unhedged <- list()
for (i in seq_len(K)) {
  var_i <- sapply(seq_len(T_len), function(t) sqrt(grouped_covs[i, i, t]) * qnorm(0.99))
  VaR_unhedged[[group_names[i]]] <- zoo(var_i, time_index)
  plot(time_index, -var_i, type="l", main = paste("VaR unhedged", group_names[i]), ylab = "Return", xlab = "Time", col = "red", ylim = c(min(-var_i), max(rets_mat[,i])))
  lines(time_index, rets_mat[, i], col = "black")
  
}

VaR_weight <- list()
for (i in seq_len(K)) {
  
  var_i <- sapply(seq_len(T_len), function(t) sqrt(grouped_covs[i, i, t]) * qnorm(0.99))
  VaR_weight[[group_names[i]]] <- zoo(var_i, time_index)
  plot(time_index, -var_i, type="l", main = paste("VaR weighted", group_names[i]), ylab = "Return", xlab = "Time", col = "red", ylim = c(min(-var_i), max(rets_mat[,i])))
  lines(time_index, rets_mat[, i], col = "black")
  
}


