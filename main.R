
# In this section we can run the whole script, using functions from other
# files in the same working directory


#________________________________PREPARATION____________________________________

# Clean the environment
#rm(list = ls()); gc()

# Connect other scripts of the same working directory
sources <- c(
  "setup.R",
  "config.R",
  "import.R",
  "cleaning.R",
  "Volatility_Window.R",
  "Volatility_Equilibria.R",
  "Map.R",
  "Graph.R"
)

stopifnot(all(file.exists(sources)))
invisible(lapply(sources, source))



#_______________________________IMPORT_DATA_____________________________________

# In this section we import the tickers that compose the SP500 index and the
# minute-level closing prices for each stock.

# Import SP500 tickers
TICKERS <- c(GetSp500Tickers(), "SPY")

# Import minute level data
INTRADAY_WIDE_DF <- BuildWideIntradayDf(
  tickers = TICKERS,
  from_date = STARTING_DATE,
  to_date = TO_DATE,
  multiplier = 1,
  timespan = "minute",
  sleep_sec = 0.1,
  verbose = TRUE
)



#______________________________CLEANING_DATA____________________________________

# In this section we clean the data. Start by building a master grid composed of
# all the expected minutes for each day that we are considering.
# Output is master grid and trading days.

out <- MasterGridCompleteData()
MASTER_GRID <- out$MASTER_GRID
TRADING_DAYS <- out$TRADING_DAYS

rm(out);invisible(gc())

# We perform the first stage of cleaning in which we drop all the tickers that
# have less than x% (defined in config as COVERAGE_TRESHOLD)
# of expected observations

PRICES <- FilterA(Coverage_Threshold = COVERAGE_TRESHOLD)

# We perform the second stage of cleaning in which we drop all the tickers that
# i) A day is classified as having a large gap if the maximum number of
# consecutive missing minutes is greater than or equal to Minutes_Big_Gap.
# ii) A ticker is removed if the fraction of days with a large gap exceeds
#     Maximum_N_Big_Gaps.
# iii) A ticker is removed if the maximum intraday gap observed on any single
#      day exceeds Max_Gap_Allowed.

PRICES <- FilterB(Minutes_Big_Gap = GAP_THR_DAY,
                  Maximum_N_Big_Gaps = BIG_GAP_THR,
                  Max_Gap_Allowed = MAX_GAP_ANY)

# Fill missing prices per ticker by carrying the last observation forward (LOCF)
# and then backward (NOCB) to eliminate internal and edge NAs; store the filled
# panel as PRICES_FILLED and run basic sanity checks for NA/Inf/zero values.

PRICES_FILLED <- FillMissingPrices()

# In this section we remove the rows corresponding to minutes in which
# at least x% of returns across all stocks is 0. This arises from locf and nocb
# filling minutes with no data. In early close days this leads to a 0 return
# for minutes in which market is closed early.

out1 <- EarlyClose(ZeroShareThreshold = ZERO_SHARE_THR)
DT <- out1$DT
tickers <- out1$tickers
rm(out1);invisible(gc())



#______________________________REALIZED_VOL_HVR_________________________________

# In this section we compute the realized volatility on non overlapping windows
# of width Window_Width. Check file config to see how windows are implemented
# with respect of PRESENT_DATETIME. We compute the HVR for the target stock
# (defined in config).

# Compute intraday realized volatility on non-overlapping window of width
# WINDOW (in file config)
REALIZED_VOLATILITY <- RealizedNonOverlappingVolatility(Window_Width = WINDOW)

# Here we generate the HVR for a given TARGET_STOCK (file config).
HVR <- HistoricalVolatilityRatio(df = REALIZED_VOLATILITY,
                                 TargetStock = TARGET_STOCK,
                                 datetimecol = "datetime")



#_____________________________STABILITY_OF_HVR__________________________________

# In this section we compute the local and global stability of each HVR with
# the Wasserstein distance. Local stability is intended as the average
# difference between each empirical train distribution and the previous
# empirical distribution of HVR (average measure in shift of HVR). Global
# stability is intended as the average dstance between each observed
# distribution of HVR and the overall distribution of HVR in the train sample.

STABILITY_INDIVIDUAL_HVR <- ComputeStabilityAll(df = HVR, L = N_EMPIRICAL_DIST,
                                                p = p_chosen, step = 20)


# Considerations: this method is able to deliver relevant results in terms
# of distribution stability of HVRs, producing a "ranking" of the most stable
# distributions for each HVR throught time.
# As we are comparing HVRs individually, given a target asset's volatility
# (common denominator of HVRs) we will obtain a ranking of the most 
# stable HVRs in a bivariate setting (as HVR requires two asset's
# volatilities to be computed). This method cannot properly identify local
# stable structures of volatilities among groups of assets by properly
# accounting for comovements of multiple assets (it is a "greedy method" to
# select, given a target asset, the asset whose volatility shows greater
# stability in comovements with the volatility of the target asset).
 


#_________________STABILITY_OF_MULTIVARIATE_LEVEL_VOLATILITY____________________

# In this section we analyze the stability of multivariate distributions of
# level realized volatility across assets. The realized volatility series is
# partitioned into consecutive non-overlapping windows in the train sample,
# and the stability of the joint distribution is assessed via the
# 1-Wasserstein distance between consecutive windows. A forward selection
# procedure is then used to identify the subset of assets that minimizes
# distributional instability over time starting from a list of assets
# and minimizing average 1-Wasserstein distance across consecutive windows for
# each additional asset in the distribution.
WINDOWS <- PrepareAnalysisMultivariate(VolatilityDf = REALIZED_VOLATILITY,
                                       PresentDatetime = PRESENT_DATETIME,
                                       Width = 100,
                                       DatetimeCol = "datetime",
                                       step = 20)

STABILITY_MULTIVARIATE_VOL <- ForwardSelectionStability(windows = WINDOWS,
                                                          start = "NCLH",
                                                          K = 10)
STABILITY_MULTIVARIATE_VOL$history

# Considerations: this method is a "greedy" algorithm that identifies, given
# one or more target assets, the best candidates to produce a multivariate
# stable distribution of volatility when added to the target assets.
# By its own nature, this method will tend to select assets that show
# greater stability by themselves to reduce the level of instability of
# the joint distribution with target assets. This method overcomes
# the limitations of the previous attempt, but reveals a structural (logical)
# weakness in finding local volatility equilibria.



#_________________STABILITY_OF_MULTIVARIATE_DEPENDENCE_COPULA___________________

WINDOWS <- PrepareAnalysisMultivariateCopula(VolatilityDf = REALIZED_VOLATILITY,
                                             PresentDatetime = PRESENT_DATETIME,
                                             Width = 100,
                                             DatetimeCol = "datetime",
                                             step = 20)

STABILITY_MULTIVARIATE_DEP_VOL <- ForwardSelectionDependence(windows = WINDOWS,
                                                             start = "NCLH",
                                                             K = 10)
STABILITY_MULTIVARIATE_DEP_VOL$history




















#________________ITERATED_STABILITY_OF_RELATIVE_VOLATILITY_1____________________

# In this section we analyze the stability of relative realized volatility
# dynamics across assets, measured through the distribution of the
# log Historical Volatility Ratios (HVRs). For a given subset of assets
# (seed), an aggregate volatility anchor is constructed as the cross-sectional
# mean of their realized volatilities. Candidate assets are then evaluated
# based on the temporal stability of their log-volatility ratios with respect
# to this anchor.
# Instability is quantified as the standard deviation of the log(HVR) over
# the training sample. A forward selection procedure is employed to iteratively
# expand the initial set of assets by adding, at each step, the candidate that
# minimizes the incremental instability of the joint HVR structure. The
# procedure continues until a target number of assets is selected, producing
# both the final subset and the full selection path.

STABILITY_AVG_HVR <- ForwardSelectRatioDf(VolatilityDf = REALIZED_VOLATILITY,
                                          PresentDatetime = PRESENT_DATETIME,
                                          DatetimeCol = "datetime",
                                          seed = c("AAPL"),
                                          K = 10)

STABILITY_AVG_HVR$history

# Considerations: This method improves upon previous approaches by using an
# iterative (greedy) forward selection procedure that evaluates candidate
# assets based on the stability of their relative volatility dynamics with
# respect to a jointly defined volatility anchor, rather than treating assets
# in isolation.
# Nonetheless, several limitations remain. First, the volatility anchor is
# constructed as the simple cross-sectional average of realized volatilities,
# which represents a convenient but arbitrary proxy for joint volatility.
# Second, the selection criterion relies on the time-series standard deviation
# of log HVRs, rather than on a fully distributional measure of temporal
# stability. Finally, the method does not explicitly investigate the latent
# risk components underlying observed volatility dynamics, which could be
# explored in future work using dimensionality reduction techniques such as PCA.


                                          
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
STABILITY_DIST_HVR <- ForwardSelectDistDf(VolatilityDf = REALIZED_VOLATILITY,
                                          PresentDatetime = PRESENT_DATETIME,
                                          DatetimeCol = "datetime",
                                          seed = c("NCLH"),
                                          K = 10, L = 150, p = 2, step = 5)
STABILITY_DIST_HVR$history

# Consideration: performances of this algorithm show a greater efficiency in
# identifying the assets whose volatilities are stable compared to the
# basket of stocks (assets) chosen as seed.
# Nonetheless, this method can be implemented by adding an additional score
# for the assets selection, that accounts not only for stability of
# distribution but also to the cohesion of the volatilities chosen (tendency
# to mean revert).



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
RESULTS <- ForwardSelectDistCohesionDf(VolatilityDf = REALIZED_VOLATILITY,
                                       PresentDatetime = PRESENT_DATETIME,
                                       DatetimeCol = "datetime",
                                       seed = c("AAPL"),
                                       K = 10, L = 100, p = 1, step = 10)
RESULTS

# This part is used to plot the score for stocks
scores <- RESULTS$history$score
stocks <- RESULTS$history$added
valid <- !is.na(scores)
seed_name <- RESULTS$history$added[1]

plot(x = seq_along(scores[valid]), y = scores[valid], type = "b", xaxt = "n",
  xlab = "Stock", ylab = "Score", 
  main = paste("Score by Selected Stock -", seed_name), col = "blue", pch = 19)

axis(1, at = seq_along(stocks[valid]), labels = stocks[valid], las = 2)



#_________________________STOP_RULE_LOCAL_EQUILIBRIUM___________________________

# This function defines a stopping rule for the forward selection procedure.
# Under a minimization framework, it stops at step k when no sufficient
# relative improvement (at least x) is observed within the next 'lookahead'
# steps. If the condition is never met, the maximum available step is returned.
# The rule identifies the point at which marginal gains become negligible,
# signaling the emergence of a local volatility equilibrium.
k_star <- StopByImprovement(
  score_vector = RESULTS$history$score,
  x = 0.1,
  lookahead = 3,
  min_k = 2
)

LOCAL_EQUILIBRIUM <- RESULTS$selected[1:k_star]

print(RESULTS$history)
cat("\nStop step:", k_star, "\n")
cat("Local equilibrium:", paste(LOCAL_EQUILIBRIUM, collapse = ", "), "\n")



#__________________________MAP_ALL_LOCAL_CONNECTIONS____________________________

# This section takes each stock (assets included in column names of
# VolatilityDf) as a seed and computes the score fore each of the other
# stocks (assets) in the column names of VolatilityDf data frame.
# In other terms, it performs the first step of the forward greedy selection
# algorithm in section LOCAL_EQUILIBRIUM and records all the scores. It is
# performed gives as seed each stock (asset) at a time and generates the
# n x n matrix of all scores (where n is the number of stocks (assets) in the
# VolatilityDf). Performed only on train sample, identified as sample going
# from initial observation to PresentDatetime.
# The goal of this section is to generate a "map" of the volatility structure
# of the stock universe analysed.
# After computing te matrix with BuildDistCohesionMatrix, we compute the
# average volatility of all the stocks (assets) included in the data frame
# VolatilityDf from "FROM" to "TO".
W <- BuildDistCohesionMatrix(VolatilityDf = REALIZED_VOLATILITY,
                             PresentDatetime = PRESENT_DATETIME,
                             DatetimeCol = "datetime",
                             L = 100, p = 2, step = 20)

W_dist <- W$W_dist
W_coh  <- W$W_coh

W_total <- BuildWTotal(W_dist, W_coh, lambda = 0.5)

#FROM = as.POSIXct("2025-04-01 10:00:00", tz = "America/New_York")
#TO = as.POSIXct("2025-08-12 14:30:00", tz = "America/New_York")

#AVERAGE_VOL <- AverageVolatility(VolatilityDf = REALIZED_VOLATILITY,
#                                 DatetimeCol = "datetime",
#                                 From = FROM, To = TO)

TICKERS <- setdiff(colnames(REALIZED_VOLATILITY), "datetime")

MARKET_CAP <- GetMarketCapPolygon(tickers = TICKERS,
                                      api_key = POLYGON_KEY)
SECTORS <- GetSectorsWikipedia(tickers = TICKERS)

