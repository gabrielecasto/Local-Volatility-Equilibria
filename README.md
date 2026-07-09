# Local Volatility Equilibria

This repository contains an empirical R project that explores the **localization of local volatility equilibria** across S&P 500 stocks using high-frequency intraday data.

The code is designed as an experimental research pipeline. Its goal is to investigate whether groups of stocks can be identified whose realized volatility dynamics display relatively stable and cohesive relationships over time.

The project starts from minute-level stock prices, computes realized volatility, studies relative volatility structures, and then applies several stability-based and cohesion-based selection methods to search for local volatility equilibria.

## Important

This repository represents a first research draft of the project. The current pipeline is mainly exploratory and is used to empirically test different methods for localizing local volatility equilibria. The most promising approaches will be further developed in a new and more complete pipeline, with stronger code robustness, improved methodological validation, additional statistical tests, and out-of-sample evaluation of the methods that appear to work empirically.

## Research Idea

The central idea of the project is that volatility should not only be studied at the level of individual stocks. Instead, the code investigates whether there are **local structures of volatility**: groups of assets whose realized volatilities move in a relatively stable relationship with one another.

In this framework, a local volatility equilibrium is interpreted as a subset of assets where relative volatility dynamics are:

1. **Distributionally stable**, meaning that the empirical distribution of relative volatility ratios does not change excessively over time;

2. **Dynamically cohesive**, meaning that the relative log-volatility differences among selected assets show some persistence and tendency to remain within a coherent local structure;

3. **Locally self-consistent**, meaning that adding further assets provides only limited marginal improvement according to the selected scoring rule.

The project therefore experiments with different ways of identifying these structures empirically.

## Project Structure

```text
.
├── main.R
├── setup.R
├── config.R
├── import.R
├── cleaning.R
├── Volatility_Window.R
├── Volatility_Equilibria.R
├── Map.R
└── Graph.R
```

## Files Description

### `main.R`

Runs the full empirical pipeline.

The script sources all other files, imports the data, cleans the intraday price panel, computes realized volatility, constructs Historical Volatility Ratios, applies different stability and equilibrium localization methods, builds a score matrix of local volatility connections, and finally produces a network graph.

### `setup.R`

Installs and loads the required R packages.

It also defines the parallel computation plan and activates progress bars.

### `config.R`

Contains the main global parameters used in the project, including:

* sample start and end dates;
* Polygon API key loading;
* cleaning thresholds;
* realized volatility window size;
* target stock for Historical Volatility Ratios;
* training/test split timestamp;
* Wasserstein distance parameters;
* empirical distribution window length.

The Polygon API key is read from the environment variable:

```r
POLYGON_API_KEY
```

### `import.R`

Imports the stock universe and downloads minute-level price data.

The import procedure also includes a patching mechanism for ticker-day combinations with extreme missingness, which can occur because of temporary API failures during large downloads.

### `cleaning.R`

Cleans and aligns the intraday data.

The cleaning procedure:

1. Builds a complete minute-level master grid for all trading days;
2. Aligns all stock prices to this common grid;
3. Drops stocks with insufficient overall data coverage;
4. Drops stocks with excessive intraday missing-data gaps;
5. Fills remaining missing prices using LOCF and NOCB;
6. Computes intraday log returns;
7. Removes early-close periods where most stocks display zero returns because the market was already closed.

The output of this stage is a cleaned intraday return panel suitable for realized volatility estimation.

### `Volatility_Window.R`

Computes realized volatility and Historical Volatility Ratios.

Realized volatility is computed over non-overlapping intraday windows as the sum of squared log returns.

For each stock, the code also computes a Historical Volatility Ratio relative to a selected target stock:

```text
HVR_i,t = RV_i,t / RV_target,t
```

where:

* `RV_i,t` is the realized volatility of stock `i`;
* `RV_target,t` is the realized volatility of the target stock.

This transforms individual realized volatility series into relative volatility series.

### `Volatility_Equilibria.R`

Contains the main empirical methods used to search for local volatility equilibria.

The file implements several approaches:

* univariate stability analysis of Historical Volatility Ratios;
* multivariate stability analysis of realized volatility levels;
* copula-based dependence stability;
* forward selection based on relative volatility ratios;
* forward selection based on distributional stability;
* forward selection based on both distributional stability and dynamic cohesion;
* a stopping rule used to identify the size of a local equilibrium.

The most important part of the project is the local equilibrium procedure, which combines two components:

#### Distributional Stability

For a candidate stock, the code constructs log-relative volatility ratios with respect to a volatility anchor. The empirical distribution of these ratios is then evaluated through rolling windows.

Stability is measured using Wasserstein distances:

* local stability compares consecutive rolling empirical distributions;
* global stability compares each rolling distribution with the overall training distribution.

Lower instability indicates that the candidate has more stable relative volatility dynamics with respect to the selected seed assets.

#### Dynamic Cohesion

The cohesion component studies the log-volatility differences between a candidate stock and the already selected assets.

An AR(1)-type regression is estimated on the resulting relative log-volatility series. The cohesion score is used to capture whether the candidate contributes to a persistent and coherent local volatility structure.

#### Forward Selection

Starting from a seed stock or a seed group, the algorithm evaluates all remaining stocks and selects the candidate that optimizes the combined stability/cohesion score.

The procedure is repeated until a maximum number of assets is reached.

#### Stopping Rule

A stopping rule is applied to determine when the marginal improvement becomes too small. This step is used to define the final local volatility equilibrium.

### `Map.R`

Builds a full map of local volatility connections.

For each stock used as a seed, the script evaluates all other stocks as possible candidates. It stores:

* a distributional instability score;
* a cohesion score.

These scores are collected into two matrices:

```r
W_dist
W_coh
```

The two components are then standardized and combined into a total score matrix:

```r
W_total
```

This matrix represents the strength of local volatility connections across the stock universe.

### `Graph.R`

Generates the final network graph of local volatility connections.

Starting from `W_total`, the script keeps the strongest positive connections for each stock, symmetrizes the resulting edge list, and builds an undirected graph.

In the graph:

* nodes represent stocks;
* edges represent strong local volatility connections;
* edge width and transparency depend on connection strength;
* node size is scaled using market capitalization.

The graph provides a visual representation of the empirical local volatility structure identified by the model.

## How to Run the Code

### 1. Clone the repository

```bash
git clone <repository-url>
cd <repository-name>
```

### 2. Open the project in R or RStudio

Make sure all `.R` files are in the same working directory.

### 3. Add your Polygon API key

Create or edit your `.Renviron` file and add:

```r
POLYGON_API_KEY=your_polygon_api_key_here
```

Restart R after saving the file.

### 4. Check the configuration file

Before running the project, check the parameters in `config.R`

Important: `PRESENT_DATETIME` must correspond to a valid timestamp in the realized volatility sample, because it is used to split the observations into training and testing periods.

### 5. Run the full script

```r
source("main.R")
```

## Requirements

The project is written in R and uses the following main packages:

```r
jsonlite
dplyr
purrr
data.table
tidyquant
lubridate
httr
hms
ggplot2
future.apply
stringr
here
transport
future
progressr
rvest
tidyr
tibble
MASS
glmnet
igraph
ggraph
```

The file `setup.R` installs missing packages automatically.
