


# In this file we compute the realized volatility on non overlapping windows
# of width Window_Width. Check file config to see how windows are implemented
# with respect of PRESENT_DATE. We compute the HVR for the target stock
# (defined in config).



#___________________________REALIZED_VOLATILITY_________________________________

# This function removes the 9:30 returns as we are disregarding the overnight
# returns. After this initial cleaning, we compute volatility on
# non-overlapping windows of width Window_Width as the sum of squared returns.

RealizedNonOverlappingVolatility <- function(Window_Width) {
  
  # Remove observations at 9:30 for each day as they are set to be NAs
  is_0930 <- format(DT$datetime, "%H:%M") == "09:30"
  DT <- DT[!is_0930]
  sum(is_0930)
  any(format(DT$datetime, "%H:%M") == "09:30")
  
  # Compute indexes of start and end time of training and test volatility
  # buckets of width Window_Width.
  idx_p <- which(DT$datetime == PRESENT_DATETIME)[1]
  idx_train <- idx_p - Window_Width * seq(from = 0, to = floor((idx_p - 1) / Window_Width))
  idx_test  <- idx_p + Window_Width * seq(from = 1, to = floor((nrow(DT) - idx_p) / Window_Width))
  
  idx_raw <- sort(unique(c(idx_train, idx_test)))
  idx_raw <- idx_raw[idx_raw >= 1 & idx_raw <= nrow(DT)]
  n_intervals_before <- length(idx_raw) - 1
  
  idx <- sort(unique(c(idx_train, idx_test)))
  idx <- idx[idx >= 1 & idx <= nrow(DT)]
  idx <- sort(unique(idx))
  keep <- c(TRUE, diff(idx) == Window_Width)
  idx <- idx[keep]
  
  n_intervals_after <- length(idx) - 1
  
  stopifnot(all(idx >= 1 & idx <= nrow(DT)))
  stopifnot(all(diff(idx) == Window_Width))
  
  n_intervals_dropped <- n_intervals_before - n_intervals_after
  
  cols <- setdiff(colnames(DT), c("datetime", "date", "m"))
  X2 <- as.data.frame(DT[, ..cols] * DT[, ..cols])
  X2 <- rbind(rep(0, ncol(X2)), X2)
  X2_cum <- X2
  X2_cum[] <- lapply(X2, cumsum)
  rm(X2); gc()
  X2_cum <- X2_cum[idx, , drop = FALSE]
  X2_period <- X2_cum
  X2_period[-1, ] <- X2_cum[-1, ] - X2_cum[-nrow(X2_cum), ]
  X2_period[1, ]  <- X2_cum[1, ]
  rm(X2_cum); gc()
  X2_period <- X2_period %>%
    dplyr::mutate(datetime = DT$datetime[idx]) %>%
    dplyr::select(datetime, dplyr::everything())
  
  number_NA <- sum(is.na(X2_period))
  number_inf <- sum(is.infinite(
    as.matrix(X2_period[, sapply(X2_period, is.numeric), drop = FALSE])))
  number_of_0 <- sum(X2_period == 0, na.rm = TRUE)
  
  cat("Number of NAs: ", number_NA, "\n", sep = "")
  cat("Number of Inf: ", number_inf, "\n", sep = "")
  cat("Number of 0s: ", number_of_0, "\n", sep = "")
  
  rm(DT, envir = .GlobalEnv); gc()
  
  return(X2_period)
}



#_______________________HISTORICAL_VOLATILITY_RATIO_____________________________

# Here we generate the HVR for a given TARGET_STOCK (file config).
# HVR (https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5538758)

HistoricalVolatilityRatio <- function(df, TargetStock, datetimecol) {
  
  den = df[[TargetStock]]
  den[den == 0] <- NA_real_
  
  HVR <- data.frame(datetime = df[[datetimecol]],
         df[, setdiff(names(df), c(datetimecol, TargetStock)), drop = FALSE] / den,
         check.names = FALSE
  )
  
  rownames(HVR) <- NULL
  
  return(HVR)
}