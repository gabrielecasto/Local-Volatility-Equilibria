


#______________________________STABILITY_HVR____________________________________

# This section defines two functions.
# StabilityOneSeries evaluates the temporal stability of a time series.
# The training sample is partitioned into windows of length L.
# Each window defines a non-parametric empirical distribution of HVR.
# Local stability is measured by computing the average Wasserstein distance
# between the empirical distributions of consecutive windows, capturing
# short-term distributional changes.
# Global stability is measured by computing the average Wasserstein distance
# between the empirical distribution of each window and the empirical
# distribution of the full training sample, capturing long-term drift
# and distributional oscillations.
# ComputeStabilityAll applies this procedure to all columns of the HVR
# data frame, excluding the datetime column.
# The parameter `step` determines the stride between consecutive windows;
# values step < L generate overlapping windows, improving robustness to
# boundary effects and sensitivity to smooth distributional drift.

StabilityOneSeries <- function(x, L, p, step) {
  
  stopifnot(all(is.finite(as.matrix(x))))
  
  n <- length(x)
  if (n < 2 * L) return(c(S_local = NA_real_, S_global = NA_real_))
  
  starts <- seq.int(from = 1, to = n - L + 1, by = step)
  W <- length(starts)
  if (W < 2) return(c(S_local = NA_real_, S_global = NA_real_))
  
  windows <- lapply(starts, function(s) x[s:(s + L - 1)])
  x_used  <- x[starts[1]:(starts[W] + L - 1)]
  
  d_local <- vapply(
    2:W,
    function(w) transport::wasserstein1d(windows[[w]], windows[[w - 1]], p = p),
    numeric(1)
  )
  
  d_global <- vapply(
    1:W,
    function(w) transport::wasserstein1d(windows[[w]], x_used, p = p),
    numeric(1)
  )
  
  c(S_local = mean(d_local), S_global = mean(d_global))
}


ComputeStabilityAll <- function(df, L, step, p, exclude = c("datetime")) {
  
  # Keep only TRAIN
  df_train <- df[df[[exclude[1]]] <= PRESENT_DATETIME, , drop = FALSE]
  
  cols <- setdiff(names(df_train), exclude)
  
  res_mat <- t(vapply(
    cols,
    function(col) StabilityOneSeries(df_train[[col]], L = L,
                                     p = p, step = step),
    
    FUN.VALUE = c(S_local = 0, S_global = 0)
  ))
  
  res <- data.frame(
    ticker = cols,
    S_local = res_mat[, "S_local"],
    S_global = res_mat[, "S_global"],
    S_total = res_mat[, "S_local"] + res_mat[, "S_global"],
    row.names = NULL
  )
  
  res <- res[order(res$S_total), ]
  
  return(res)
  
}



#_________________STABILITY_OF_MULTIVARIATE_LEVEL_VOLATILITY____________________

# In this section we analyze the stability of multivariate distributions of
# realized volatility across assets. The realized volatility series is
# partitioned into windows of fixed length (Width) over the training sample,
# and the stability of the joint distribution is assessed via the
# 1-Wasserstein distance between consecutive windows.
# Windows are constructed in a rolling fashion, with the parameter `step`
# controlling the stride between consecutive windows. When step < Width,
# consecutive windows partially overlap, reducing sensitivity to arbitrary
# window boundaries and improving the detection of smooth distributional
# changes over time.
# A forward selection procedure is then used to identify the subset of assets
# that minimizes average distributional instability, measured as the mean
# 1-Wasserstein distance across consecutive windows, for each additional asset
# included in the multivariate distribution.



# This function prepares the data frame containing realized volatility
# estimations for the identification of the most stable multivariate
# distributions of level volatility. The stability is computed across
# consecutive windows in the train part of the data frame.
PrepareAnalysisMultivariate <- function(VolatilityDf, PresentDatetime, 
                                        Width, step, DatetimeCol) {
  
  rownames(VolatilityDf) = NULL
  idx_train <- which(VolatilityDf[[DatetimeCol]] == PresentDatetime)
  
  X <- VolatilityDf[1:idx_train, 
                          setdiff(colnames(VolatilityDf), DatetimeCol)]
  
  # Define parameters, divide in windows
  stopifnot(all(is.finite(as.matrix(X))))
  
  n <- nrow(X)
  if (n < 2 * Width) return(list())
  
  starts <- seq.int(from = 1, to = n - Width + 1, by = step)
  W <- length(starts)
  if (W < 2) return(list())
  
  windows <- lapply(starts, function(s) X[s:(s + Width - 1), , drop = FALSE])
  
  rm(X, idx_train); gc()
  
  return(windows)
}

# This function computes the average 1-Wasserstein distance (W1)
# between consecutive windows for a given subset of assets.
# Each window is standardized jointly with the next one, and the
# W1 distance is normalized by the square root of the dimension.
MeanW1 <- function(windows, stocks) {
  
  # Dimension of the multivariate distribution and number of windows
  d <- length(stocks)
  W <- length(windows)
  
  # At least two windows are required to compute stability
  if (W < 2) return(NA_real_)
  
  # Compute W1 distance between two consecutive windows
  W1Pair <- function(A, B) {
    
    # Extract selected assets and convert to matrices
    A <- as.matrix(A[, stocks, drop = FALSE])
    B <- as.matrix(B[, stocks, drop = FALSE])
    
    # Joint standardization using pooled mean and standard deviation
    XY <- rbind(A, B)
    mu  <- colMeans(XY)
    sdv <- apply(XY, 2, sd)
    sdv[sdv == 0] <- 1  # avoid division by zero
    
    A <- sweep(sweep(A, 2, mu, "-"), 2, sdv, "/")
    B <- sweep(sweep(B, 2, mu, "-"), 2, sdv, "/")
    
    # Compute pairwise Euclidean distance matrix between windows
    n <- nrow(A)
    m <- nrow(B)
    C <- as.matrix(dist(rbind(A, B)))[1:n, (n + 1):(n + m)]
    
    # Solve the optimal transport problem with uniform weights
    p <- local({
      p_out <- NULL
      invisible(capture.output({
        p_out <- suppressMessages(
          transport::transport(rep(1 / n, n),
                               rep(1 / m, m),
                               costm  = C,
                               method = "shortsimplex")
        )
      }))
      p_out
    })
    
    # Compute normalized 1-Wasserstein distance
    sum(p$mass * C[cbind(p$from, p$to)]) / sqrt(d)
  }
  
  # Average W1 distance across all consecutive window pairs
  mean(
    sapply(2:W, function(t) W1Pair(windows[[t - 1]], windows[[t]])),
    na.rm = TRUE
  )
  
}

# This function performs a forward selection procedure to identify
# the subset of assets that minimizes the average W1 distance across
# consecutive windows. Assets are added sequentially based on the
# marginal reduction in instability.
ForwardSelectionStability <- function(windows, start, K = 10) {
  
  # All available assets and initialization
  all  <- colnames(windows[[1]])
  sel  <- start
  cand <- setdiff(all, start)
  
  # Store selection history
  hist <- data.frame(
    step = 1,
    added = start,
    MeanW1= MeanW1(windows, sel)
  )
  
  # Sequentially add assets up to K selections
  for (k in 2:K) {
    
    # Evaluate contribution of each candidate asset in parallel
    sc <- future.apply::future_sapply(
      cand,
      function(s) MeanW1(windows, c(sel, s)),
      future.seed = TRUE
    )
    
    # Select asset minimizing the W1 distance
    best <- names(which.min(sc))
    sel  <- c(sel, best)
    cand <- setdiff(cand, best)
    
    # Update selection history
    hist <- rbind(
      hist,
      data.frame(
        step = k,
        added = best,
        MeanW1 = min(sc, na.rm = TRUE)
      )
    )
  }
  
  rm("WINDOWS", envir = .GlobalEnv);gc()
  
  # Return selected assets and full selection path
  list(selected = sel, history = hist)
}



#_________________STABILITY_OF_MULTIVARIATE_DEPENDENCE_COPULA___________________

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# This function prepares the data frame containing realized volatility
# estimations for the identification of the most stable multivariate
# distributions of level volatility. The stability is computed across
# consecutive windows in the train part of the data frame.
PrepareAnalysisMultivariateCopula <- function(VolatilityDf, PresentDatetime,
                                              Width, step, DatetimeCol) {
  
  rownames(VolatilityDf) = NULL
  idx_train <- which(VolatilityDf[[DatetimeCol]] == PresentDatetime)
  
  X <- VolatilityDf[1:idx_train, 
                    setdiff(colnames(VolatilityDf), DatetimeCol)]
  
  # Global copula transform on the whole train
  nX <- nrow(X)
  X <- as.data.frame(lapply(X, function(col) {
    rank(col, ties.method = "average") / (nX + 1)
  }))
  
  # Define parameters, divide in windows
  stopifnot(all(is.finite(as.matrix(X))))
  
  n <- nrow(X)
  if (n < 2 * Width) return(list())
  
  starts <- seq.int(from = 1, to = n - Width + 1, by = step)
  W <- length(starts)
  if (W < 2) return(list())
  
  windows <- lapply(starts, function(s) X[s:(s + Width - 1), , drop = FALSE])
  
  rm(X, idx_train); gc()
  
  return(windows)
}


# This function computes the average 1-Wasserstein distance (W1)
# between consecutive windows for a given subset of assets.
# Each window is standardized jointly with the next one, and the
# W1 distance is normalized by the square root of the dimension.
MeanW1Copula <- function(windows, stocks) {
  
  # Dimension of the multivariate distribution and number of windows
  d <- length(stocks)
  W <- length(windows)
  
  # At least two windows are required to compute stability
  if (W < 2) return(NA_real_)
  
  # Compute W1 distance between two consecutive windows
  W1Pair <- function(A, B) {
    
    # Extract selected assets and convert to matrices
    A <- as.matrix(A[, stocks, drop = FALSE])
    B <- as.matrix(B[, stocks, drop = FALSE])
    
    # Compute pairwise Euclidean distance matrix between windows
    n <- nrow(A)
    m <- nrow(B)
    C <- as.matrix(dist(rbind(A, B)))[1:n, (n + 1):(n + m)]
    
    # Solve the optimal transport problem with uniform weights
    p <- local({
      p_out <- NULL
      invisible(capture.output({
        p_out <- suppressMessages(
          transport::transport(rep(1 / n, n),
                               rep(1 / m, m),
                               costm  = C,
                               method = "shortsimplex")
        )
      }))
      p_out
    })
    
    # Compute normalized 1-Wasserstein distance
    sum(p$mass * C[cbind(p$from, p$to)]) / sqrt(d)
  }
  
  # Average W1 distance across all consecutive window pairs
  mean(
    sapply(2:W, function(t) W1Pair(windows[[t - 1]], windows[[t]])),
    na.rm = TRUE
  )
  
}

# This function performs a forward selection procedure to identify
# the subset of assets that minimizes the average W1 distance across
# consecutive windows. Assets are added sequentially based on the
# marginal reduction in instability.
ForwardSelectionDependence <- function(windows, start, K = 10) {
  
  # All available assets and initialization
  all  <- colnames(windows[[1]])
  sel  <- start
  cand <- setdiff(all, start)
  
  # Store selection history
  hist <- data.frame(
    step = 1,
    added = start,
    MeanW1Copula = MeanW1Copula(windows, sel)
  )
  
  # Sequentially add assets up to K selections
  for (k in 2:K) {
    
    # Evaluate contribution of each candidate asset in parallel
    sc <- future.apply::future_sapply(
      cand,
      function(s) MeanW1Copula(windows, c(sel, s)),
      future.seed = TRUE
    )
    
    # Select asset minimizing the W1 distance
    best <- names(which.min(sc))
    sel  <- c(sel, best)
    cand <- setdiff(cand, best)
    
    # Update selection history
    hist <- rbind(
      hist,
      data.frame(
        step = k,
        added = best,
        MeanW1Copula = min(sc, na.rm = TRUE)
      )
    )
  }
  
  rm("WINDOWS", envir = .GlobalEnv);gc()
  
  # Return selected assets and full selection path
  list(selected = sel, history = hist)
}






















#________________ITERATED_STABILITY_OF_RELATIVE_VOLATILITY_1____________________

# In this section we analyze the stability of relative realized volatility
# dynamics across assets through the distribution of the log Historical
# Volatility Ratios (HVRs). Given an initial subset of assets (seed), an
# aggregate volatility anchor is constructed as the cross-sectional mean of
# their realized volatilities.
# For each candidate asset, relative volatility dynamics are measured by the
# log-ratio between the candidate’s realized volatility and the anchor.
# Instability is quantified as the temporal dispersion of this log-HVR series
# over the training sample.
# A forward selection procedure is employed to iteratively expand the initial
# set of assets by adding, at each step, the candidate that minimizes the
# incremental instability of the joint relative volatility structure. The
# procedure continues until a target number of assets is selected, producing
# both the final subset and the complete selection path.

# This function considers only the training part of the data frame and
# computes, for the selected "seed" columns the row mean of their values (in
# this case realized volatility). Then calculates the standard deviation of
# the log(HVR) as a form of instability of the relative distribution of the
# candidate realized volatility. Denominator of HVR is anchor, row mean of
# realized volatilities.
ScoreRatioStabilityDf <- function(VolatilityDf, PresentDatetime, seed,
                                     DatetimeCol, candidate) {
  
  # Use only training part of data frame
  idx_train <- which(VolatilityDf[[DatetimeCol]] == PresentDatetime)
  if (length(idx_train) != 1) return(NA_real_)
  
  # Compute the mean of volatility on seed stocks (assets) for each row
  anchor <- rowMeans(as.matrix(VolatilityDf[1:idx_train, seed, drop = FALSE]))
  y <- as.numeric(VolatilityDf[1:idx_train, candidate])
  
  # Compute the standard deviation of the log of HVR, for the candidate.
  # HVR has as denominator anchor and as numerator candidate.
  eps <- 1e-12
  r <- log(pmax(y, eps)) - log(pmax(anchor, eps))
  sd(r, na.rm = TRUE)
  
}

# This function applies ScoreStabilityDf to all the candidates excluding
# the seed. Then updates the seed by adding the best candidate (the one
# with more stable HVR) to the seed and repeats the process until
# (K - seed) number of stocks are selected.
ForwardSelectRatioDf <- function(VolatilityDf, PresentDatetime, DatetimeCol,
                                 seed, K) {
  
  # Identify seed stocks (assets), and candidates.
  all  <- setdiff(colnames(VolatilityDf), DatetimeCol)
  sel  <- seed
  cand <- setdiff(all, sel)
  
  # Creates the first row of the dat frame to store results
  hist <- data.frame(step = 1, added = paste(seed, collapse=","),
                     score = NA_real_)
  
  # Compute the volatility score for each candidate and choose the lowest one.
  # Update the list of candidates removing the chosen ones.
  # report the result adding a row to the data frame created above.
  for (k in 1:(K - length(seed))) {
    sc <- future.apply::future_sapply(
      cand,
      function(s) ScoreRatioStabilityDf(
        VolatilityDf = VolatilityDf,
        PresentDatetime = PresentDatetime,
        seed = sel,
        DatetimeCol = DatetimeCol,
        candidate = s
      )
    )
    
    best <- names(which.min(sc))
    sel  <- c(sel, best)
    cand <- setdiff(cand, best)
    
    hist <- rbind(hist, data.frame(step = k + 1, added = best,
                                   score = min(sc, na.rm = TRUE)))
  }
  
  return(list(selected = sel, history = hist))
  
}



#________________ITERATED_STABILITY_OF_RELATIVE_VOLATILITY_2____________________

# This section analyzes the stability of relative realized volatility dynamics
# across assets using the distribution of log Historical Volatility Ratios
# (HVRs). Given an initial set of assets (seed), an aggregate volatility
# anchor is defined as the cross-sectional mean of their realized volatilities.
# For each candidate asset, instability is measured on the log-HVR series
# using rolling windows of length L and stride `step`, combining global and
# local distributional stability measures. A forward selection procedure is
# then used to iteratively select assets that minimize relative volatility
# instability with respect to the anchor.

# Computes the distributional instability of a candidate asset’s relative
# realized volatility with respect to the current anchor using rolling
# windows of length L and stride `step`.
ScoreDistributionStabilityDf <- function(VolatilityDf, PresentDatetime, seed,
                                         DatetimeCol, candidate, L, p, step) {
  
  # Use only training part of data frame
  idx_train <- which(VolatilityDf[[DatetimeCol]] == PresentDatetime)
  if (length(idx_train) != 1) return(NA_real_)
  
  # Compute the mean of volatility on seed stocks (assets) for each row
  anchor <- rowMeans(as.matrix(VolatilityDf[1:idx_train, seed, drop = FALSE]))
  y <- as.numeric(VolatilityDf[1:idx_train, candidate])
  eps <- 1e-12
  r <- log(pmax(y, eps)) - log(pmax(anchor, eps))
  stab  <- StabilityOneSeries(r, L = L, p = p, step = step)
  global <- stab[["S_global"]]
  local  <- stab[["S_local"]]
  total_score <- global + local
  
  return(total_score)
  
}

# Implements a forward selection procedure based on distributional stability
# of relative realized volatility, returning both the selected assets and
# the full selection path.
ForwardSelectDistDf <- function(VolatilityDf, PresentDatetime, DatetimeCol,
                                 seed, K, L, p, step) {
  
  # Identify seed stocks (assets), and candidates.
  all  <- setdiff(colnames(VolatilityDf), DatetimeCol)
  sel  <- seed
  cand <- setdiff(all, sel)
  
  # Creates the first row of the dat frame to store results
  hist <- data.frame(step = 1, added = paste(seed, collapse=","),
                     score = NA_real_)
  
  # Compute the distributional instability score for each candidate asset
  # relative to the current anchor, select the candidate minimizing this
  # score, and update the selection history accordingly.
  for (k in 1:(K - length(seed))) {
    
    sc <- future.apply::future_sapply(
      cand,
      function(s) ScoreDistributionStabilityDf(
        VolatilityDf = VolatilityDf,
        PresentDatetime = PresentDatetime,
        seed = sel,
        DatetimeCol = DatetimeCol,
        candidate = s,
        L = L,
        p = p,
        step = step
      ),
      future.packages = "transport",
      future.seed = TRUE
    )
    
    best <- names(which.min(sc))
    sel  <- c(sel, best)
    cand <- setdiff(cand, best)
    
    hist <- rbind(hist, data.frame(step = k + 1, added = best, 
                                   score = min(sc, na.rm = TRUE)))
  }
  
  return(list(selected = sel, history = hist))
  
}



#_____________________________LOCAL_EQUILIBRIUM_________________________________

# In this section we introduce the concept of local volatility equilibrium.
# Unlike previous approaches that focus solely on distributional stability,
# this framework combines two complementary dimensions:
#
#   1. Relative distributional stability of log-volatility ratios,
#      measured through rolling Wasserstein distances.
#
#   2. Dynamic cohesion of the selected assets, measured via the
#      persistence of cross-sectional log-volatility differences
#      (AR(1) mean-reversion structure).
#
# The objective is to identify a subset of assets whose relative volatility
# dynamics are both statistically stable and dynamically cohesive,
# forming a locally self-consistent volatility structure.
#
# A forward selection procedure iteratively expands the seed set by
# selecting assets that jointly minimize distributional instability
# and maximize dynamic cohesion.
#
# The stopping rule determines the point at which marginal improvements
# become negligible, signaling the emergence of a local equilibrium.

# This function measures the dynamic cohesion of a candidate asset
# relative to a set of seed assets over the training sample.
# Cohesion is defined as the cross-sectional mean of log-differences
# between the candidate’s realized volatility and each seed’s volatility.
# An AR(1) model is estimated on this series, and the autoregressive
# coefficient (phi) is returned as a measure of persistence of the
# relative volatility structure.
ScoreCohesionDf <- function(VolatilityDf, PresentDatetime, seed,
                            DatetimeCol, candidate) {
  
  # Use only training part of data frame
  idx_train <- which(VolatilityDf[[DatetimeCol]] == PresentDatetime)
  if (length(idx_train) != 1) return(NA_real_)
  train <- VolatilityDf[1:idx_train, , drop = FALSE]
  
  # Compute log of each value of train data (log-volatility)
  eps <- 1e-12
  log_candidate <- log(pmax(train[[candidate]], eps))
  
  # Compute the series of cohesion of the distribution
  r_mat <- sapply(seed, function(j){
    log_j <- log(pmax(train[[j]], eps))
    log_candidate - log_j
  })
  
  # Cohesion serie
  g <- rowMeans(r_mat)
  
  # AR1 process
  g1 <- g[-length(g)]
  g2 <- g[-1]
  
  fit <- try(lm(g2 ~ g1), silent = TRUE)
  if (inherits(fit, "try-error")) return(NA_real_)
  
  phi <- as.numeric(coef(fit)[2])
  sd_g <- sd(g, na.rm = TRUE)
  return(phi * sd_g)

}

# Implements a forward selection procedure based on distributional stability
# of relative realized volatility and cohesion score (tendency to mean revert
# to equilibrium of ratios). It returns both the selected assets and
# the full selection path.
ForwardSelectDistCohesionDf <- function(VolatilityDf, PresentDatetime,
                                        DatetimeCol, seed, K, L, p, step) {
  
  # Identify seed stocks (assets), and candidates.
  all  <- setdiff(colnames(VolatilityDf), DatetimeCol)
  sel  <- seed
  cand <- setdiff(all, sel)
  
  # Creates the first row of the dat frame to store results
  hist <- data.frame(
    step = 1,
    added = paste(seed, collapse=","),
    dist_score = NA_real_,
    cohesion_phi = NA_real_,
    score = NA_real_
  )
  
  # Compute the distributional instability score for each candidate asset
  # relative to the current anchor as defined in ScoreDistributionStabilityDf,
  # then compute the cohesion score for each candidate as defined in
  # ScoreCohesionDf, select the best candidate.
  for (k in 1:(K - length(seed))) {
    
    # Compute distance score
    distance_score <- future.apply::future_sapply(
      cand,
      function(s) ScoreDistributionStabilityDf(
        VolatilityDf = VolatilityDf,
        PresentDatetime = PresentDatetime,
        seed = sel,
        DatetimeCol = DatetimeCol,
        candidate = s,
        L = L,
        p = p,
        step = step
      ),
      future.packages = "transport",
      future.seed = TRUE
    )
    
    # Compute cohesion score
    cohesion_score <- future.apply::future_sapply(
      cand,
      function(j) ScoreCohesionDf(
        VolatilityDf = VolatilityDf,
        PresentDatetime = PresentDatetime,
        DatetimeCol = DatetimeCol,
        seed = sel,
        candidate = j
      ),
      future.seed = TRUE
    )
    
    distance_score[!is.finite(distance_score)] <- 
      max(distance_score[is.finite(distance_score)], na.rm = TRUE) + 1
    cohesion_score[!is.finite(cohesion_score)] <- 
      max(cohesion_score[is.finite(cohesion_score)], na.rm = TRUE) + 1
    
    # Normalize both scores cross-sectionally (z-score)
    z_dist <- as.numeric(scale(distance_score))
    z_coh  <- as.numeric(scale(cohesion_score))
    
    # Compute the total score as a sum of the two scores
    total_score <- as.numeric(z_dist) + as.numeric(z_coh)
    names(total_score) <- cand
    
    # Change sign of total score to be positive
    total_score <- - total_score
    
    # Select the best candidate as the one with maximum score (after sign flip)
    best <- names(which.max(total_score))
    sel  <- c(sel, best)
    
    # Update candidates list
    cand <- setdiff(cand, best)
    
    # Update the history of selection
    hist <- rbind(hist, data.frame(
      step = k + 1,
      added = best,
      dist_score = distance_score[best],
      cohesion_phi = cohesion_score[best],
      score = total_score[best]
    ))
  }
  
  return(list(selected = sel, history = hist))
  
}



#_________________________STOP_RULE_LOCAL_EQUILIBRIUM___________________________

# This function defines a stopping rule for the forward selection procedure.
# Under a minimization framework, it stops at step k when no sufficient
# relative improvement (at least x) is observed within the next 'lookahead'
# steps. If the condition is never met, the maximum available step is returned.
# The rule identifies the point at which marginal gains become negligible,
# signaling the emergence of a local volatility equilibrium.
StopByImprovement <- function(score_vector, x,
                              lookahead, min_k) {
  
  # Remove NA values (e.g., seed step) and keep index mapping
  valid <- !is.na(score_vector)
  scores <- as.numeric(score_vector[valid])
  index_map <- which(valid)
  
  n <- length(scores)
  if (n < lookahead + 1) stop("Not enough scores.")
  
  # Iterate through candidate stopping points
  for (k in seq(from = min_k, to = n - lookahead)) {
    
    # Define required relative improvement threshold
    target <- (1 - x) * scores[k]
    
    # Compute the best (minimum) score within the lookahead window
    future_min <- min(scores[(k + 1):(k + lookahead)])
    
    # Stop if no sufficient improvement is observed
    if (future_min > target) {
      return(index_map[k])
    }
  }
  
  # If condition is never triggered, return the largest available step
  index_map[n]
}