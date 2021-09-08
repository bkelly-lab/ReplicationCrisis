library(directlabels)
library(xtable)
library(zeallot)
library(dendextend)
library(RColorBrewer)
library(rsample)
library(lubridate)
library(tidyverse)
library(data.table)
options(dplyr.summarise.inform = FALSE)

# How To --------------------------------------
# Paths
# - portfolio_path:           Folder that contains market_returns.csv, hml.csv and cmp.csv generated from portfolio.R                
# - object_path:              Folder to save objects too. Retrived when update_*==F
# - output_path:              Folder to save figures in. Not neccesary if save_figures==F
# Save
# - save_figures:             Should figures be saved in output_path?
# Update
# - update_sim:               Simulations for figure 2 (Simulation Comparison of False Discovery Rates)
# - update_post_over_time:    Posterior calculations for figure 8 (US Factor Alpha Posterior Distribution over Time)
# - update_post_is:           Data for regression in table E.1 (The Economic Benefit of More Powerful Tests)
# - update_harvey_baseline:   Data for figure 9
# - update_harvey_worstcase:  Data for figure F.1

# User Input -----------------------
# Paths
portfolio_path <- "Factors" 
object_path <- "Objects"
output_path <- "Figures"
# Save
save_figures <- T
# Update
update_sim <- T              
update_post_over_time <- T
update_post_is <- T
update_harvey_baseline <- T  
update_harvey_worstcase <- T 
# Settings
settings <- list(
  seed = 1,
  start_date = as.Date("1925-12-31"),     
  end_date = as.Date("2019-12-31"),       # Important that end_date <= Last_CRSP_UPDATE
  country_excl = c("ZWE", "VEN"),         # Countries are excluded due to data issues
  weighting =  list(                      # Which weighting scheme to use? In c("ew", "vw", "vw_cap")
    us = 'vw_cap',
    global_ex_us = 'vw_cap'
  ),                   
  n_stocks_min = 5,                       # Minimum amount of stocks in each side of the portfolios
  months_min = 5 * 12,                    # Minimum amount of observations a factor needs to be included  
  country_weighting = "market_cap",       # How to weight countries? In ("market_cap", "stocks", "ew")
  countries_min = 3,                      # Minimum number of countries necessary in a regional portfolio
  clusters = "hcl",                       # Which cluster method to use? In c("manual", "hcl")
  hcl = list(
    ret_type = "alpha",                   # Which return to use in clustering: In c("raw", "alpha")
    cor_method = "pearson",               # Which cor method to base distance upon
    linkage    = "ward.D",                # Which linkage method to use
    k          = 13,                      # How many clusters to colour
    region     = "us",                    # Region to use for clusters
    start_year = 1975                     # Start year cluster data   
  ),
  eb = list(
    scale_alpha = T,
    overlapping = F,
    min_obs = 5 * 12,
    fix_alpha = T, 
    bs_cov = T, 
    shrinkage = 0, 
    cor_type = "block_clusters", 
    bs_samples = 10000                    # Set to 10000 for paper
  ),
  tpf = list(
    start = list(
      world = as.Date("1952-01-01"),
      us = as.Date("1952-01-01"),
      developed = as.Date("1987-01-01"),
      emerging = as.Date("1994-01-01"),
      size_grps = as.Date("1963-01-01")   # Dictated by start of nano-caps
    ),
    bs_samples = 10000,                   # Number of bootstrap samples [10.000 for paper]
    shorting = F                          # Should shorting be allowed?
  ),
  tpf_factors = list(
    region = "us",
    orig_sig = T,                         # Only include originally significant factor: T, include all: c(T,F)
    start = as.Date("1972-01-31"),
    scale = T,                            # Scale to ex-post volatility of 10%?
    k = 5                                 # Number of Folds for cross-validation exercise
  )
)

# Layout Settings ---------------
theme_set(theme_classic())
colours_theme <- c("#0C6291", "#A63446", RColorBrewer::brewer.pal(8, "Dark2"), 
                   "darkslategrey", "blue3", "red3", "purple2", "yellow2", "aquamarine",
                   "grey", "salmon", "antiquewhite", "chartreuse") 
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = colours_theme)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = colours_theme)
}

# Run Scripts ----------
source("0 - Functions.R")
source("1 - Prepare Data.R")
source("2 - Determine Clusters.R")
source("3 - Analysis.R")
source("4 - Output.R")
