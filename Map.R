


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



# Given a seed, this function computes the local equilibria (distribution
# stability + cohesion) score for each of the other stocks (assets) contained
# in the VolatilityDf data frame.
ScoreRowDistCohesion <- function(VolatilityDf, PresentDatetime,
                                 DatetimeCol, seed, L, p, step) {
  
  # Select candidates
  all <- setdiff(colnames(VolatilityDf), DatetimeCol)
  sel <- seed
  cand <- setdiff(all, sel)
  
  # Calculate score of distribution stability
  distance_score <- future.apply::future_sapply(
    cand,
    function(s) ScoreDistributionStabilityDf(VolatilityDf = VolatilityDf,
                                             PresentDatetime = PresentDatetime,
                                             DatetimeCol = DatetimeCol,
                                             seed = sel, candidate = s,
                                             L = L, p = p, step = step),
    future.packages = "transport",
    future.seed = TRUE
  )
  
  # Calculate the score for cohesion
  cohesion_score <- future.apply::future_sapply(
    cand,
    function(s) ScoreCohesionDf(VolatilityDf = VolatilityDf,
                                PresentDatetime = PresentDatetime,
                                seed = sel,
                                DatetimeCol = DatetimeCol,
                                candidate = s),
    future.seed = TRUE
  )
  
  # Handle non-finite
  distance_score[!is.finite(distance_score)] <- 
    max(distance_score[is.finite(distance_score)], na.rm = TRUE) + 1
  cohesion_score[!is.finite(cohesion_score)] <- 
    max(cohesion_score[is.finite(cohesion_score)], na.rm = TRUE) + 1
  
  # Safe z-score (avoid sd=0 issues)
  z_safe <- function(x) {
    if (length(x) < 2) return(rep(0, length(x)))
    sx <- sd(x, na.rm = TRUE)
    if (!is.finite(sx) || sx == 0) return(rep(0, length(x)))
    as.numeric((x - mean(x, na.rm = TRUE)) / sx)
  }
  
  z_dist <- z_safe(distance_score)
  z_coh  <- z_safe(cohesion_score)
  
  total_score <- -(z_dist + z_coh)
  names(total_score) <- cand
  
  list(
    dist  = distance_score,
    coh   = cohesion_score
  )
}

# Given the cohesion scores produced by the previous function, this function
# generates the matrix of cohesion scores for each seed and each of the
# stocks contained in VolatilityDf.
BuildDistCohesionMatrix <- function(VolatilityDf, PresentDatetime, DatetimeCol,
                                    L, p, step, keep_components = FALSE) {
  
  # Find tickers (no datetime)
  tickers <- setdiff(colnames(VolatilityDf), DatetimeCol)
  n <- length(tickers)
  
  # Build the empty matrix to store scores
  W_dist <- matrix(NA_real_, nrow = n, ncol = n,
                   dimnames = list(tickers, tickers))
  W_coh  <- matrix(NA_real_, nrow = n, ncol = n,
                   dimnames = list(tickers, tickers))
  
  
  # Building scores for each ticker i as seed
  res_list <- progressr::with_progress({
    
    pbar <- progressr::progressor(along = tickers)
    
    future.apply::future_lapply(tickers, function(i) {
      row_res <- ScoreRowDistCohesion(VolatilityDf = VolatilityDf,
                                      PresentDatetime = PresentDatetime,
                                      DatetimeCol = DatetimeCol, seed = i,
                                      L = L, p = p, step = step)
      
      pbar()
      
      list(seed = i, dist = row_res$dist, coh = row_res$coh)
    },
    future.seed = TRUE)
  })
  
  for (k in seq_along(res_list)) {
    i <- res_list[[k]]$seed
    d <- res_list[[k]]$dist
    c <- res_list[[k]]$coh
    
    W_dist[i, names(d)] <- d
    W_coh[i,  names(c)] <- c
    
    W_dist[i, i] <- NA_real_
    W_coh[i,  i] <- NA_real_
  }
  
  if (!keep_components) return(list(W_dist = W_dist, W_coh = W_coh))
  
  list(W_dist = W_dist, W_coh = W_coh, res_list = res_list)
  
}

# This section generates a data frame of average realized volatility for each
# stock given the realized volatility data frame. It is useful to leave it as a
# separate function for clarity of algorithm.
AverageVolatility <- function(VolatilityDf, DatetimeCol, From, To) {
  
  idx_from <- which(VolatilityDf[[DatetimeCol]] == From)
  if(length(idx_from) != 1) return(NA_real_)
  idx_to <- which(VolatilityDf[[DatetimeCol]] == To)
  if(length(idx_to) != 1) return(NA_real_)
  
  stocks <- setdiff(colnames(VolatilityDf), DatetimeCol)
  df <- VolatilityDf[idx_from:idx_to, stocks]
  
  avg_vol_j <- sapply(stocks, function(j) {mean(df[[j]], na.rm = TRUE)})
  
  average_volatility <- matrix(avg_vol_j, nrow = 1,
                               dimnames = list("Avg_Vol", stocks)
  )
  
  return(average_volatility)

}
  
  
  
  














BuildWTotal <- function(W_dist, W_coh, lambda = 0.5) {
  vd <- as.numeric(W_dist); vd <- vd[is.finite(vd)]
  vc <- as.numeric(W_coh);  vc <- vc[is.finite(vc)]
  
  md <- median(vd); sd_ <- mad(vd, constant = 1); if (!is.finite(sd_) || sd_ == 0) sd_ <- 1
  mc <- median(vc); sc_ <- mad(vc, constant = 1); if (!is.finite(sc_) || sc_ == 0) sc_ <- 1
  
  W_total <- -((W_dist - md) / sd_ + lambda * (W_coh - mc) / sc_)
  diag(W_total) <- NA_real_
  W_total
}











GetMarketCapPolygon <- function(tickers, api_key) {
  
  caps <- future_sapply(tickers, function(tk) {
    
    url <- paste0(
      "https://api.polygon.io/v3/reference/tickers/",
      tk,
      "?apiKey=", api_key
    )
    
    res <- try(GET(url), silent = TRUE)
    if (inherits(res, "try-error") || status_code(res) != 200)
      return(NA_real_)
    
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    mc <- data$results$market_cap
    
    if (is.null(mc) || length(mc) == 0) NA_real_ else as.numeric(mc)
  })
  
  as.data.frame(t(caps))
}









GetSectorsWikipedia <- function(tickers) {
  
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  
  # Read tables from Wikipedia page
  tables <- try(rvest::read_html(url) |> rvest::html_table(fill = TRUE), silent = TRUE)
  if (inherits(tables, "try-error") || length(tables) == 0) {
    return(data.frame(ticker = tickers, sector = NA_character_, stringsAsFactors = FALSE))
  }
  
  sp500_tbl <- tables[[1]]
  
  # Standardize column names a bit
  # Expected: "Symbol" and "GICS Sector"
  if (!all(c("Symbol", "GICS Sector") %in% names(sp500_tbl))) {
    return(data.frame(ticker = tickers, sector = NA_character_, stringsAsFactors = FALSE))
  }
  
  sector_map <- sp500_tbl %>%
    transmute(
      ticker = str_trim(Symbol),
      sector = str_trim(`GICS Sector`)
    )
  
  # Some tickers in datasets use "." instead of "-" (e.g., BRK.B vs BRK-B on Wikipedia)
  tickers_clean <- str_replace_all(str_trim(tickers), "\\.", "-")
  
  out <- data.frame(
    ticker = tickers,
    ticker_wiki = tickers_clean,
    stringsAsFactors = FALSE
  ) %>%
    left_join(sector_map, by = c("ticker_wiki" = "ticker")) %>%
    select(ticker, sector)
  
  out
}