# =======================
# RUN THIS WHOLE BLOCK
# =======================

# Packages
suppressPackageStartupMessages({
  library(igraph)
  library(ggraph)
  library(ggplot2)
})

# -----------------------
# 0) SETTINGS
# -----------------------
top_x  <- 10
lambda <- 0.5

# -----------------------
# 1) Helpers
# -----------------------
MakeTopXEdges <- function(W_total, top_x = 10) {
  tickers <- rownames(W_total)
  out <- vector("list", length(tickers))
  
  for (i in seq_along(tickers)) {
    from <- tickers[i]
    v <- W_total[from, ]
    v[from] <- NA_real_
    
    v <- v[is.finite(v)]
    v <- v[v > 0]
    
    if (length(v) == 0) next
    
    ord <- order(v, decreasing = TRUE)
    take <- ord[seq_len(min(top_x, length(ord)))]
    
    out[[i]] <- data.frame(
      from   = from,
      to     = names(v)[take],
      weight = as.numeric(v[take]),
      stringsAsFactors = FALSE
    )
  }
  
  edges <- do.call(rbind, out)
  if (is.null(edges) || nrow(edges) == 0) stop("No edges produced (maybe all weights <= 0).")
  edges
}

SymmetrizeEdges <- function(edges) {
  edges$from <- trimws(as.character(edges$from))
  edges$to   <- trimws(as.character(edges$to))
  
  edges <- edges[is.finite(edges$weight), , drop = FALSE]
  edges <- edges[!is.na(edges$from) & !is.na(edges$to) & edges$from != "" & edges$to != "", , drop = FALSE]
  edges <- edges[edges$from != edges$to, , drop = FALSE]
  
  key <- paste(pmin(edges$from, edges$to), pmax(edges$from, edges$to), sep="__")
  edges$key <- key
  
  agg <- aggregate(weight ~ key, edges, mean)
  
  parts <- strsplit(agg$key, "__", fixed = TRUE)
  agg$from <- vapply(parts, `[[`, character(1), 1)
  agg$to   <- vapply(parts, `[[`, character(1), 2)
  agg$key <- NULL
  
  agg
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

# -----------------------
# 2) Ensure W_total exists
# -----------------------
if (!exists("W_total")) {
  if (!exists("W_dist") || !exists("W_coh")) stop("Missing W_total and also missing W_dist/W_coh.")
  W_total <- BuildWTotal(W_dist, W_coh, lambda = lambda)
}

stopifnot(is.matrix(W_total))

# -----------------------
# 3) Build edges (top-x) + symmetrize
# -----------------------
edges_dir <- MakeTopXEdges(W_total, top_x = top_x)
edges <- SymmetrizeEdges(edges_dir)

# IMPORTANT: guarantee columns exactly: from, to, weight (in this order doesn't matter)
edges <- edges[, c("from", "to", "weight")]
edges$from <- as.character(edges$from)
edges$to   <- as.character(edges$to)
edges$weight <- as.numeric(edges$weight)

# -----------------------
# 4) Build graph (NO vertices_df to avoid name mismatch errors)
# -----------------------
g <- igraph::graph_from_data_frame(d = edges, directed = FALSE)
deg <- igraph::degree(g)
g <- igraph::induced_subgraph(g, vids = names(deg[deg >= 2]))

# -----------------------
# 5) Edge scaling (robust) — ALWAYS length == ecount(g)
# -----------------------
Ew <- as.numeric(igraph::E(g)$weight)
if (length(Ew) != igraph::ecount(g)) Ew <- rep(1, igraph::ecount(g))

w_med <- median(Ew, na.rm = TRUE)
w_mad <- mad(Ew, constant = 1, na.rm = TRUE)
if (!is.finite(w_mad) || w_mad == 0) w_mad <- 1

igraph::E(g)$w_scaled <- pmax(0, (Ew - w_med) / w_mad)

# -----------------------
# 6) Node size = Market Cap
# -----------------------

# extract first row as numeric vector
mc_map <- as.numeric(MARKET_CAP[1, ])
names(mc_map) <- colnames(MARKET_CAP)

# assign to graph nodes
igraph::V(g)$size_scaled <- (log(mc_map[igraph::V(g)$name]))^20

# -----------------------
# 7) Plot — DO NOT use any mutate(direction=...) or .data$to/.data$from anywhere
# -----------------------
set.seed(1)
p <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(alpha = w_scaled,
                     width = w_scaled),
                 color = "red",
                 show.legend = FALSE) +
  geom_node_point(aes(size = size_scaled), show.legend = FALSE, color = "lightblue") +
  geom_node_text(aes(label = name), size = 2.5, repel = FALSE, color = "black") +
  scale_edge_width(range = c(0.2, 2.5)) +
  scale_edge_alpha(range = c(0.01, 1.2)) +
  scale_size(range = c(0.4, 20)) +
  theme_void()

print(p)