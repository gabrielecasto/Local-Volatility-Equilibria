# In this file we generate a network graph of local volatility connections.
# The graph is built starting from the total score matrix W_total, where higher
# values identify stronger local volatility connections between pairs of
# stocks. Edges are selected by keeping, for each stock, the top_x strongest
# connections. The graph is then symmetrized and plotted using igraph and
# ggraph. Node size is scaled according to market capitalization.



#_______________________________GRAPH_PACKAGES__________________________________

# Load the packages required to build and plot the network graph.
# igraph is used to create and manipulate the network structure.
# ggraph and ggplot2 are used to produce the final visualization.

suppressPackageStartupMessages({
  library(igraph)
  library(ggraph)
  library(ggplot2)
})



#______________________________GRAPH_SETTINGS___________________________________

# top_x defines how many strongest connections are retained for each stock.
# For each row of W_total, only the top_x positive scores are transformed into
# candidate edges.

top_x  <- 10

# lambda controls the relative importance of the cohesion component when
# W_total is rebuilt from W_dist and W_coh inside this file. In the main script,
# W_total is already computed before sourcing Graph.R, but this parameter is
# kept here as a fallback in case W_total does not yet exist.

lambda <- 0.5



#_____________________________MAKE_TOP_X_EDGES__________________________________

# This function converts the total score matrix W_total into a directed edge
# list. For each ticker, the function selects the top_x strongest positive
# connections with the other tickers. Self-connections are excluded by setting
# the diagonal element of each row to NA.
#
# The output is a data frame with three columns:
# i)   from:   ticker from which the connection starts;
# ii)  to:     ticker to which the connection points;
# iii) weight: strength of the connection.

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
  if (is.null(edges) || nrow(edges) == 0) {
    stop("No edges produced (maybe all weights <= 0).")
  }
  edges
}



#______________________________SYMMETRIZE_EDGES_________________________________

# The edge list produced by MakeTopXEdges is directed because each stock selects
# its own top_x connections. This function transforms the edge list into an
# undirected structure by collapsing reciprocal pairs into a single edge.
#
# For example, AAPL -> MSFT and MSFT -> AAPL are converted into one undirected
# edge between AAPL and MSFT. If the same pair appears more than once, the edge
# weight is averaged.

SymmetrizeEdges <- function(edges) {
  edges$from <- trimws(as.character(edges$from))
  edges$to   <- trimws(as.character(edges$to))
  
  edges <- edges[is.finite(edges$weight), , drop = FALSE]
  edges <- edges[
    !is.na(edges$from) & !is.na(edges$to) &
      edges$from != "" & edges$to != "",
    , drop = FALSE
  ]
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



#_______________________________BUILD_W_TOTAL___________________________________

# This function combines two score matrices into one total connection score:
# i)   W_dist: distributional instability score;
# ii)  W_coh:  cohesion score.
#
# Both components are robustly standardized using the median and MAD. The final
# score is multiplied by -1 so that stronger and more desirable connections are
# represented by higher values in W_total. The diagonal is set to NA to avoid
# self-connections.

BuildWTotal <- function(W_dist, W_coh, lambda = 0.5) {
  vd <- as.numeric(W_dist); vd <- vd[is.finite(vd)]
  vc <- as.numeric(W_coh);  vc <- vc[is.finite(vc)]
  
  md <- median(vd)
  sd_ <- mad(vd, constant = 1)
  if (!is.finite(sd_) || sd_ == 0) {
    sd_ <- 1
  }
  
  mc <- median(vc)
  sc_ <- mad(vc, constant = 1)
  if (!is.finite(sc_) || sc_ == 0) {
    sc_ <- 1
  }
  
  W_total <- -((W_dist - md) / sd_ + lambda * (W_coh - mc) / sc_)
  diag(W_total) <- NA_real_
  W_total
}



#______________________________CHECK_W_TOTAL____________________________________

# Graph.R is sourced at the end of the main script, after W_total has already
# been created. This block is a safety check: if W_total does not exist but
# W_dist and W_coh are available, the file rebuilds W_total using BuildWTotal.
# If neither W_total nor its components are available, the script stops.

if (!exists("W_total")) {
  if (!exists("W_dist") || !exists("W_coh")) {
    stop("Missing W_total and also missing W_dist/W_coh.")
  }
  W_total <- BuildWTotal(W_dist, W_coh, lambda = lambda)
}

stopifnot(is.matrix(W_total))



#_______________________________BUILD_EDGES_____________________________________

# This section builds the graph edge list. First, each stock is connected to
# its top_x strongest positive links. Then the directed edge list is converted
# into an undirected edge list. Finally, the columns are forced to have the
# exact names and types required by igraph.

edges_dir <- MakeTopXEdges(W_total, top_x = top_x)
edges <- SymmetrizeEdges(edges_dir)

# IMPORTANT: guarantee columns exactly: from, to, weight (in this order doesn't
# matter)
edges <- edges[, c("from", "to", "weight")]
edges$from <- as.character(edges$from)
edges$to   <- as.character(edges$to)
edges$weight <- as.numeric(edges$weight)



#________________________________BUILD_GRAPH____________________________________

# This section converts the edge list into an undirected igraph object.
# Nodes with degree lower than 2 are removed to keep the final visualization
# focused on stocks that are meaningfully connected to the local volatility
# network.

g <- igraph::graph_from_data_frame(d = edges, directed = FALSE)
deg <- igraph::degree(g)
g <- igraph::induced_subgraph(g, vids = names(deg[deg >= 2]))



#_______________________________SCALE_EDGES_____________________________________

# Edge weights are scaled robustly using the median and MAD of the edge weights.
# The scaled value is stored as an edge attribute called w_scaled and is used
# later to control both edge width and edge transparency in the graph.

Ew <- as.numeric(igraph::E(g)$weight)
if (length(Ew) != igraph::ecount(g)) Ew <- rep(1, igraph::ecount(g))

w_med <- median(Ew, na.rm = TRUE)
w_mad <- mad(Ew, constant = 1, na.rm = TRUE)
if (!is.finite(w_mad) || w_mad == 0) w_mad <- 1

igraph::E(g)$w_scaled <- pmax(0, (Ew - w_med) / w_mad)



#_______________________________SCALE_NODES_____________________________________

# Node size is based on market capitalization. MARKET_CAP is generated in the
# main script before Graph.R is sourced. Here, the first row of MARKET_CAP is
# converted into a named numeric vector and matched to graph nodes by ticker.

# extract first row as numeric vector
mc_map <- as.numeric(MARKET_CAP[1, ])
names(mc_map) <- colnames(MARKET_CAP)

# assign to graph nodes
igraph::V(g)$size_scaled <- (log(mc_map[igraph::V(g)$name]))^20



#_________________________________PLOT_GRAPH____________________________________

# This section produces the final network plot.
# The Fruchterman-Reingold layout is used to place strongly connected nodes
# closer together. Edge width and transparency are proportional to the scaled
# edge weight. Node size is proportional to scaled market capitalization.
# Ticker labels are added directly on the nodes.

set.seed(1)
p <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(alpha = w_scaled,
                     width = w_scaled),
                 color = "red",
                 show.legend = FALSE) +
  geom_node_point(aes(size = size_scaled), show.legend = FALSE,
                  color = "lightblue") +
  geom_node_text(aes(label = name), size = 2.5, repel = FALSE,
                 color = "black") +
  scale_edge_width(range = c(0.2, 2.5)) +
  scale_edge_alpha(range = c(0.01, 1.2)) +
  scale_size(range = c(0.4, 20)) +
  theme_void()

print(p)