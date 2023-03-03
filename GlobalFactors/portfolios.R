library(lubridate)
library(tidyverse)
library(data.table)

# How To --------------------
# Paths
# - data_path:    Set to path with global characteristics data and if daily_pf==T should also contain a folder with daily stock returns.
# - output_path:  Set to desired output folder.
# - legacy_path:  Set to folder if you want to maintain legacy version. If not, set to NULL.
# Countries
# - countries:    Choose the countries where portfolio returns are created. Default: All countries in data_path/Characteristics
# Characteristics
# - chars:        Characteristics to create portfolios from. Can be any column from the global characteristics dataset.
# Portfolio Settings
# - settings:     Choose how to create portfolios. For more information, see description for the portfolios() function

# User Input -----------------------
# Paths
data_path <- "Data"
output_path <- "PaperFactors"
legacy_path <- "Legacy"
# Countries
countries <- list.files(path = paste0(data_path, "/Characteristics")) %>% str_remove(".csv")
# Chars 
chars <- c(
  "age",                 "aliq_at",             "aliq_mat",            "ami_126d",           
  "at_be",               "at_gr1",              "at_me",               "at_turnover",        
  "be_gr1a",             "be_me",               "beta_60m",            "beta_dimson_21d",    
  "betabab_1260d",       "betadown_252d",       "bev_mev",             "bidaskhl_21d",       
  "capex_abn",           "capx_gr1",            "capx_gr2",            "capx_gr3",           
  "cash_at",             "chcsho_12m",          "coa_gr1a",            "col_gr1a",           
  "cop_at",              "cop_atl1",            "corr_1260d",          "coskew_21d",         
  "cowc_gr1a",           "dbnetis_at",          "debt_gr3",            "debt_me",            
  "dgp_dsale",           "div12m_me",           "dolvol_126d",         "dolvol_var_126d",    
  "dsale_dinv",          "dsale_drec",          "dsale_dsga",          "earnings_variability",
  "ebit_bev",            "ebit_sale",           "ebitda_mev",          "emp_gr1",            
  "eq_dur",              "eqnetis_at",          "eqnpo_12m",           "eqnpo_me",           
  "eqpo_me",             "f_score",             "fcf_me",              "fnl_gr1a",           
  "gp_at",               "gp_atl1",             "ival_me",             "inv_gr1",            
  "inv_gr1a",            "iskew_capm_21d",      "iskew_ff3_21d",       "iskew_hxz4_21d",     
  "ivol_capm_21d",       "ivol_capm_252d",      "ivol_ff3_21d",        "ivol_hxz4_21d",      
  "kz_index",            "lnoa_gr1a",           "lti_gr1a",            "market_equity",      
  "mispricing_mgmt",     "mispricing_perf",     "ncoa_gr1a",           "ncol_gr1a",          
  "netdebt_me",          "netis_at",            "nfna_gr1a",           "ni_ar1",             
  "ni_be",               "ni_inc8q",            "ni_ivol",             "ni_me",              
  "niq_at",              "niq_at_chg1",         "niq_be",              "niq_be_chg1",        
  "niq_su",              "nncoa_gr1a",          "noa_at",              "noa_gr1a",           
  "o_score",             "oaccruals_at",        "oaccruals_ni",        "ocf_at",             
  "ocf_at_chg1",         "ocf_me",              "ocfq_saleq_std",      "op_at",              
  "op_atl1",             "ope_be",              "ope_bel1",            "opex_at",            
  "pi_nix",              "ppeinv_gr1a",         "prc",                 "prc_highprc_252d",   
  "qmj",                 "qmj_growth",          "qmj_prof",            "qmj_safety",         
  "rd_me",               "rd_sale",             "rd5_at",              "resff3_12_1",        
  "resff3_6_1",          "ret_1_0",             "ret_12_1",            "ret_12_7",           
  "ret_3_1",             "ret_6_1",             "ret_60_12",           "ret_9_1",            
  "rmax1_21d",           "rmax5_21d",           "rmax5_rvol_21d",      "rskew_21d",          
  "rvol_21d",            "sale_bev",            "sale_emp_gr1",        "sale_gr1",           
  "sale_gr3",            "sale_me",             "saleq_gr1",           "saleq_su",           
  "seas_1_1an",          "seas_1_1na",          "seas_11_15an",        "seas_11_15na",       
  "seas_16_20an",        "seas_16_20na",        "seas_2_5an",          "seas_2_5na",         
  "seas_6_10an",         "seas_6_10na",         "sti_gr1a",            "taccruals_at",       
  "taccruals_ni",        "tangibility",         "tax_gr1a",            "turnover_126d",      
  "turnover_var_126d",   "z_score",             "zero_trades_126d",    "zero_trades_21d",    
  "zero_trades_252d"
)
# Portfolio settings
settings <- list(
  end_date = as.Date("2022-12-31"),
  pfs = 3,
  source = c("CRSP", "COMPUSTAT"),                           
  wins_ret = T,
  bps = "non_mc",
  bp_min_n = 10,
  cmp = list(
    us = T,
    int = F
  ),
  signals = list(
    us = F,
    int = F,
    standardize = T,
    weight = "vw_cap"
  ),
  regional_pfs = list(
    ret_type = "vw_cap",                    # Type of return to use for regional factors
    country_excl = c("ZWE", "VEN"),         # Countries are excluded due to data issues
    country_weights = "market_cap",         # How to weight countries? In ("market_cap", "stocks", "ew")
    stocks_min = 5,                         # Minimum amount of stocks in each side of the portfolios
    months_min = 5 * 12,                    # Minimum amount of observations a factor needs to be included  
    countries_min = 3                       # Minimum number of countries necessary in a regional portfolio
  ), 
  daily_pf = T,
  ind_pf = T
)

# Portfolio Function -------------
portfolios <- function(
  data_path,
  excntry,
  chars, 
  source = c("CRSP", "COMPUSTAT"),       # Use data from "CRSP", "Compustat" or both: c("CRSP", "COMPUSTAT")
  wins_ret = T,                          # Should Compustat returns be winsorized at the 0.1% and 99.9% of CRSP returns? 
  pfs,                                   # Number of portfolios 
  bps,                                   # What should breakpoints be based on? Non-Microcap stocks ("non_mc") or NYSE stocks "nyse"
  bp_min_n,                              # Minimum number of stocks used for breakpoints
  cmp = F,                               # Create characteristics managed size portfolios?
  signals = F,                           # Create portfolio signals?
  signals_standardize = F,               # Map chars to [-0.5, +0.5]?,
  signals_w = "vw_cap",                  # Weighting for signals: in c("ew", "vw", "vw_cap")
  nyse_size_cutoffs,                     # Data frame with NYSE size breakpoints
  daily_pf= F,                           # Should daily return be estimated
  ind_pf = F,                            # Should industry portfolio returns be estimated
  ret_cutoffs = NULL,                    # Data frame for monthly winsorization. Neccesary when wins_ret=T
  ret_cutoffs_daily = NULL               # Data frame for daily winsorization. Neccesary when wins_ret=T and daily_pf=T
) {
  # Characteristic Data
  data <- fread(paste0(data_path, "/Characteristics/", excntry, ".csv"), select = c("id", "eom", "source_crsp", "comp_exchg", "crsp_exchcd", "size_grp", "ret_exc", "ret_exc_lead1m", "me", "gics", "ff49", chars), colClasses = c("eom"="character"))
  data[, eom := eom %>% lubridate::fast_strptime(format = "%Y%m%d") %>% as.Date()]
  # ME CAP
  data <- nyse_size_cutoffs[, .(eom, nyse_p80)][data, on = "eom"]
  data[, me_cap := pmin(me, nyse_p80)][, nyse_p80 := NULL]
  # Screens
  if (length(source) == 1) {
    if (source == "CRSP") {
      data <- data[source_crsp == 1]
    }
    if (source == "COMPUSTAT") {
      data <- data[source_crsp == 0]
    }
  }
  data <- data[!is.na(size_grp) & !is.na(me) & !is.na(ret_exc_lead1m)] # The ret_exc_lead1m screen assumes that investor knew at the beginning of the month that the security would delist. 
  # Daily Returns
  if (daily_pf) {
    daily <- fread(paste0(data_path, "/Daily Returns/", excntry, ".csv"), colClasses = c("date"="character"), select = c("id", "date", "ret_exc")); gc()
    daily[, date := date %>% lubridate::fast_strptime(format = "%Y%m%d") %>% as.Date()]
    daily[, eom_lag1 := floor_date(date, unit="month")-1]
  }
  # Winsorize Returns?
  if (wins_ret) {
    data <- ret_cutoffs[, .("eom" = eom_lag1, "p001"=ret_exc_0_1, "p999"=ret_exc_99_9)][data, on = "eom"]
    data[source_crsp == 0 & ret_exc_lead1m > p999, ret_exc_lead1m := p999]
    data[source_crsp == 0 & ret_exc_lead1m < p001, ret_exc_lead1m := p001]
    data[, c("source_crsp", "p001", "p999") := NULL]
    if (daily_pf) {
      daily[, year := year(date)]
      daily[, month := month(date)]
      daily <- ret_cutoffs_daily[, .(year, month, "p001"=ret_exc_0_1, "p999"=ret_exc_99_9)][daily, on = .(year, month)]
      daily[id>99999 & ret_exc > p999, ret_exc := p999] # Only winsorize Compustat data, id for CRSP is 5 digits, Compustat is 9: source_crsp == 0 
      daily[id>99999 & ret_exc < p001, ret_exc := p001]
      daily[, c("p001", "p999", "year", "month") := NULL]
    }
  }
  # Standardize to [-0.5, +0.5] interval (for signals)
  if (signals_standardize & signals) {
    data[, (chars) := lapply(.SD, function(x) frank(x, ties.method = "min", na.last = "keep")), .SDcols = chars, by = eom]
    data[, (chars) := lapply(.SD, as.numeric), .SDcols = chars]
    data[, (chars) := lapply(.SD, function(x) x / max(x, na.rm=T) - 0.5), .SDcols = chars, by = eom]
  }
  # Industry Portfolios 
  if (ind_pf) {
    ind_data <- data[!is.na(gics), .(eom, gics, excntry, ret_exc_lead1m, me, me_cap)]
    # Get first 2 digits of GICS code for industry groups
    ind_data[, gics := as.numeric(substr(ind_data$gics, 1, 2))]
    ind_gics <- ind_data[, .(
      n = .N,
      ret_ew = mean(ret_exc_lead1m),
      ret_vw = sum(ret_exc_lead1m * me) / sum(me),
      ret_vw_cap = sum(ret_exc_lead1m * me_cap) / sum(me_cap) 
    ), by = .(gics, eom)][, excntry := str_to_upper(excntry)]
    # Lead month to match using leaded returns
    ind_gics[, eom := ceiling_date(eom+1, unit = "month")-1]
    ind_gics <- ind_gics[n >= bp_min_n]
    # Estimate industry portfolios by Fama-French portfolios for US data
    if (excntry == "usa"){
      ind_data <- data[!is.na(ff49), .(eom, ff49, ret_exc_lead1m, me, me_cap)]
      ind_ff49 <- ind_data[, .(
        n = .N,
        ret_ew = mean(ret_exc_lead1m),
        ret_vw = sum(ret_exc_lead1m * me) / sum(me),
        ret_vw_cap = sum(ret_exc_lead1m * me_cap) / sum(me_cap) 
      ), by = .(ff49, eom)][, excntry := str_to_upper(excntry)]
      ind_ff49[, eom := ceiling_date(eom+1, unit = "month")-1]
      ind_ff49 <- ind_ff49[n >= bp_min_n]
    }
  }
  # Prepare output list
  output <- list()
  # Apply Portfolio Function to Each Characteristic
  char_pfs <- chars %>% lapply(function(x) {
    op <- list()
    print(paste0("   " , x, ": ", match(x, chars), " out of ", length(chars)))
    data[, var := as.double(get(x))]
    # Unless we need to compute signals, limit size of data
    if(!signals) {
      sub <- data[!is.na(var), .(id, eom, var, size_grp, ret_exc_lead1m, me, me_cap, crsp_exchcd, comp_exchg)]
    } else {
      sub <- data[!is.na(var)]
    }
    # Portfolio Assignment
    if (bps == "nyse") {
      sub[, bp_stock := (crsp_exchcd == 1 & is.na(comp_exchg)) | (comp_exchg == 11 & is.na(crsp_exchcd))]
    }
    if (bps == "non_mc") {
      sub[, bp_stock := (size_grp %in% c("mega", "large", "small"))]
    }
    sub[, bp_n := sum(bp_stock), by = eom]
    sub <- sub[bp_n >= bp_min_n] # require at least 10 stocks for break points
    if (nrow(sub) != 0) {
      sub[, cdf := ecdf(var[bp_stock == T])(var), by = eom]
      sub[, min_cdf := min(cdf), by = eom]
      sub[cdf == min_cdf, cdf := 0.00000001] # To ensure that the lowest value is in portfolio 1   
      sub[, pf := ceiling(cdf*pfs), by = eom]  
      sub[pf == 0, pf := 1]  # Happens when non-bp stocks extend beyond bp stock range
      # Returns 
      op$pf_returns <- sub[, .(
        characteristic = x,
        n = .N,
        signal = median(var),
        ret_ew = mean(ret_exc_lead1m),
        ret_vw = sum(ret_exc_lead1m * me) / sum(me),
        ret_vw_cap = sum(ret_exc_lead1m * me_cap) / sum(me_cap)
      ), by = .(pf, eom)]
      op$pf_returns[, eom := ceiling_date(eom+1, unit = "month")-1]  # Reflect the fact that returns are leaded
      # Signals
      if (signals) {
        if (signals_w == "ew") {
          sub[, w := 1/.N, by = .(pf, eom)]
        }
        if (signals_w == "vw") {
          sub[, w := me / sum(me), .(pf, eom)]
        }
        if (signals_w == "vw_cap") {
          sub[, w := me_cap / sum(me_cap), .(pf, eom)]
        }
        sub[, (chars) := lapply(.SD, function(x) if_else(is.na(x), 0, x)), .SDcols = chars]  # Set missing to median of 0
        pf_signals <- sub[, lapply(.SD, function(x) sum(w * x)), .SDcols = chars, by = .(pf, eom)]
        pf_signals[, characteristic := x]
        pf_signals[, eom := ceiling_date(eom+1, unit = "month")-1]  # Reflect the fact that returns are leaded
        op$signals <- pf_signals
      }
      # Daily Portfolios
      if (daily_pf) {
        # Keep weights constant throughout month
        weights <- sub[, .(id, w_ew = 1/.N, w_vw = me/sum(me), w_vw_cap = me_cap/sum(me_cap)), by = .(eom, pf)]
        daily_sub <- weights[daily, on = .(id, eom=eom_lag1)][!is.na(pf) & !is.na(ret_exc)]
        op$pf_daily <- daily_sub[, .(
          n = .N,
          ret_ew = sum(w_ew*ret_exc),
          ret_vw = sum(w_vw*ret_exc),
          ret_vw_cap = sum(w_vw_cap*ret_exc)
        ), by = .(pf, date)][, characteristic := x]
        op$pf_daily <- op$pf_daily[n >= bp_min_n]
      }
      # Output
      return(op)  
    } 
  })
  output$pf_returns <- char_pfs %>% lapply(function(x) x$pf_returns) %>% rbindlist()
  if (daily_pf) {
    output$pf_daily <- char_pfs %>% lapply(function(x) x$pf_daily) %>% rbindlist()
  }
  if (ind_pf) {
    output$gics_returns <- ind_gics
    if (excntry == "usa") {
      output$ff49_returns <- copy(ind_ff49)
    }
  }
  if (nrow(output$pf_returns) != 0) {
    output$pf_returns[, excntry := str_to_upper(excntry)]
    if (daily_pf) {
      output$pf_daily[, excntry := str_to_upper(unique(output$pf_returns[, excntry]))]
    }
    if (signals) {
      output$signals <- char_pfs %>% lapply(function(x) x$signals) %>% rbindlist()
      output$signals[, excntry := str_to_upper(excntry)]
    }
  }
  # Characteristic Managed Portfolios 
  if (cmp) {
    output$cmp <- chars %>% lapply(function(x) {
      print(paste0("   CMP - " , x, ": ", match(x, chars), " out of ", length(chars)))
      data[, var := get(x)]
      sub <- data[!is.na(var), .(eom, var, size_grp, ret_exc_lead1m)]
      sub[, p_rank := frank(var, na.last=NA, ties.method = "average") / (.N + 1), by = .(size_grp, eom)] # Notice tie method
      sub[, p_rank_dev := p_rank - mean(p_rank), by = .(size_grp, eom)]
      sub[, weight := p_rank_dev / (sum(abs(p_rank_dev)) / 2), by = .(size_grp, eom)]  # 1 unit invested in each of the long and short pf
      cmp <- sub[, .(
        excntry = excntry,
        characteristic = x,
        n_stocks = .N,
        ret_weighted = sum(ret_exc_lead1m * weight),
        signal_weighted = sum(var * weight),
        sd_var = sd(var)
      ), by = .(size_grp, eom)]
      cmp <- cmp[sd_var != 0][, sd_var := NULL]
      cmp[, eom := ceiling_date(eom+1, unit = "month")-1]  # Reflect the fact that returns are leaded
      return(cmp)
    }) %>% rbindlist()
    output$cmp[, excntry := str_to_upper(excntry)]
  }
  # Output
  return(output)
}

# Extract Neccesary Information --------------------
# Factor Details
char_info <- readxl::read_xlsx("Factor Details.xlsx", sheet = "details", range = "A1:N300") %>%
  select("characteristic"=abr_jkp, direction) %>%
  filter(!is.na(characteristic)) %>%
  mutate(direction = direction %>% as.integer) %>%
  setDT()
# Country Classification
country_classification <- readxl::read_xlsx("Country Classification.xlsx", 
                                            sheet = "countries", range = "A1:I200") %>%
  select(excntry, msci_development, region) %>%
  filter(!is.na(excntry) & !(excntry %in% settings$regional_pfs$country_excl)) %>%
  setDT()
regions <- tibble(
  name = c("developed", "emerging", "frontier", "world", "world_ex_us"),
  country_codes = list(
    country_classification[msci_development == "developed" & excntry != "USA"]$excntry,
    country_classification[msci_development == "emerging"]$excntry,
    country_classification[msci_development == "frontier"]$excntry,
    country_classification$excntry,
    country_classification[excntry != "USA"]$excntry
  ),
  countries_min = c(rep(settings$regional_pfs$countries_min, 3), 1, 3)
)
# Cluster Labels
cluster_labels <- fread("Cluster Labels.csv")
# NYSE Cutoff 
nyse_size_cutoffs <- fread(paste0(data_path, "/nyse_cutoffs.csv"), colClasses = c("eom"="character"))
nyse_size_cutoffs[, eom := as.Date(eom, format = "%Y%m%d")]
# CRSP Return Cutoffs
ret_cutoffs <- fread(paste0(data_path, "/return_cutoffs.csv"), colClasses = c("eom"="character"))
ret_cutoffs[, eom := as.Date(eom, format = "%Y%m%d")]
ret_cutoffs[, eom_lag1 := floor_date(eom, unit = "month") - 1]  # Because we use ret_exc_lead1m
if (settings$daily_pf) {
  ret_cutoffs_daily <- fread(paste0(data_path, "/return_cutoffs_daily.csv")) 
}
# Market 
market <- fread(paste0(data_path, "/market_returns.csv"), colClasses = c("eom"="character"))
market[, eom := eom %>% as.Date("%Y%m%d")]
if (settings$daily_pf) {
  market_daily <- fread(paste0(data_path, "/market_returns_daily.csv"), colClasses = c("date"="character"))
  market_daily[, date := date %>% as.Date("%Y%m%d")]
}

# Create Portfolios -----------------------
portfolio_data <- countries %>% lapply(function(ex) {
  print(paste0(ex, ": ", match(ex, countries), " out of ", length(countries)))
  portfolios(
    data_path = data_path,
    excntry = ex, 
    chars = chars, 
    source = settings$source, 
    wins_ret = settings$wins_ret, 
    pfs=settings$pfs, 
    bps=settings$bps, 
    bp_min_n=settings$bp_min_n, 
    cmp = if_else(ex == "usa", settings$cmp$us, settings$cmp$int),
    signals=if_else(ex == "usa", settings$signals$us, settings$signals$int),
    signals_standardize=settings$signals$standardize, 
    signals_w=settings$signals$weight, 
    nyse_size_cutoffs = nyse_size_cutoffs, 
    daily_pf = settings$daily_pf,
    ind_pf = settings$ind_pf, 
    ret_cutoffs = ret_cutoffs,
    ret_cutoffs_daily = ret_cutoffs_daily
  )
})

# Daily Data
if (settings$daily_pf) {
  # Daily Portfolio Returns
  pf_daily <- portfolio_data %>% lapply(function(x) x$pf_daily) %>% rbindlist() 
  pf_daily %>% setorder(excntry, characteristic, pf, date)
  # Daily Long-Short Factors
  hml_daily <- pf_daily[, .(
    pfs = sum(pf == settings$pfs) + sum(pf == 1),
    n_stocks = n[pf==settings$pfs] + n[pf==1],
    n_stocks_min = as.integer(min(n[pf==settings$pfs], n[pf==1])),
    ret_ew = ret_ew[pf==settings$pfs] - ret_ew[pf==1],
    ret_vw = ret_vw[pf==settings$pfs] - ret_vw[pf==1],
    ret_vw_cap = ret_vw_cap[pf==settings$pfs] - ret_vw_cap[pf==1]
  ), .(excntry, characteristic, date)]
  hml_daily <- hml_daily[pfs == 2][, pfs := NULL]
  hml_daily %>% setorder(excntry, characteristic, date)
  lms_daily <- char_info[hml_daily, on = "characteristic"]
  resign_cols <- c("ret_ew", "ret_vw", "ret_vw_cap")
  lms_daily[, (resign_cols) := lapply(.SD, function(x) x*direction), .SDcols=resign_cols]
}

# Monthly Portfolio Returns
pf_returns <- portfolio_data %>% lapply(function(x) x$pf_returns) %>% rbindlist() 
pf_returns <- pf_returns %>% select(excntry, characteristic, pf, eom, n, signal, ret_ew, ret_vw, ret_vw_cap)
pf_returns %>% setorder(excntry, characteristic, pf, eom)

# GICS Returns 
if (settings$ind_pf) {
  gics_returns <- portfolio_data %>% lapply(function(x) x$gics_returns) %>% rbindlist()
  gics_returns %>% setorder(excntry, gics, eom)
  ff49_returns <- portfolio_data[[which(countries == "usa")]]$ff49_returns
  ff49_returns %>% setorder(excntry, ff49, eom)
}

# Create HML Returns
hml_returns <- pf_returns[, .(
  pfs = sum(pf == settings$pfs) + sum(pf == 1),
  signal = signal[pf==settings$pfs] - signal[pf==1],
  n_stocks = n[pf==settings$pfs] + n[pf==1],
  n_stocks_min = min(n[pf==settings$pfs], n[pf==1]),
  ret_ew = ret_ew[pf==settings$pfs] - ret_ew[pf==1],
  ret_vw = ret_vw[pf==settings$pfs] - ret_vw[pf==1],
  ret_vw_cap = ret_vw_cap[pf==settings$pfs] - ret_vw_cap[pf==1]
), .(excntry, characteristic, eom)]
hml_returns <- hml_returns[pfs == 2][, pfs := NULL]
hml_returns %>% setorder(excntry, characteristic, eom)

# Create Long-Short Factors [Sign Returns to be consistent with original paper]
lms_returns <- char_info[hml_returns, on = "characteristic"]
resign_cols <- c("signal", "ret_ew", "ret_vw", "ret_vw_cap")
lms_returns[, (resign_cols) := lapply(.SD, function(x) x*direction), .SDcols=resign_cols]

# Extract Signals (TBD)

# Extract CMP returns
cmp_returns <- portfolio_data %>% lapply(function(x) x$cmp) %>% rbindlist() 
cmp_returns <- cmp_returns %>% select(excntry, characteristic, size_grp, eom, n_stocks, signal_weighted, ret_weighted)

# Cluster portfolios ---------------
cluster_pfs <- cluster_labels[lms_returns, on = .(characteristic)][, .(
  n_factors = .N,
  ret_ew = mean(ret_ew),
  ret_vw = mean(ret_vw),
  ret_vw_cap = mean(ret_vw_cap)
), by = .(excntry, cluster, eom)]
if (settings$daily_pf) {
  cluster_pfs_daily <- cluster_labels[lms_daily, on = .(characteristic)][, .(
    n_factors = .N,
    ret_ew = mean(ret_ew),
    ret_vw = mean(ret_vw),
    ret_vw_cap = mean(ret_vw_cap)
  ), by = .(excntry, cluster, date)]
}

# Regional Portfolios ------------------------------------------------
regional_data <- function(data, mkt, date_col, char_col, countries, weighting, countries_min, periods_min, stocks_min) {
  # Determine Country Weights
  weights <- mkt[, .(excntry, get(date_col), mkt_vw_exc, "country_weight" = case_when(
    weighting == "market_cap" ~ me_lag1,
    weighting == "stocks" ~ as.double(stocks),
    weighting == "ew" ~ 1)
  )]
  weights %>% setnames(old="V2", new="date_col")
  # Portfolio Return 
  pf <- data[excntry %in% countries & n_stocks_min >= stocks_min] 
  pf %>% setnames(old=c(date_col, char_col), new = c("date_col", "char_col"))
  pf <- weights[pf, on = .(excntry, date_col)]
  pf <- pf[!is.na(mkt_vw_exc), .(  
    n_countries = .N,
    direction = unique(direction),
    ret_ew = sum(ret_ew*country_weight) / sum(country_weight),
    ret_vw = sum(ret_vw*country_weight) / sum(country_weight),
    ret_vw_cap = sum(ret_vw_cap*country_weight) / sum(country_weight),
    mkt_vw_exc = sum(mkt_vw_exc * country_weight) / sum(country_weight) 
  ), by = .(char_col, date_col)]
  # Minimum Requirement: Countries
  pf <- pf[n_countries >= countries_min]
  # Minimum Requirement: Months
  pf[, periods := .N, by = .(char_col)]
  pf <- pf[periods >= periods_min][, periods := NULL]
  pf %>% setorder(char_col, date_col)
  pf %>% setnames(old = c("date_col", "char_col"), new = c(date_col, char_col))
  return(pf)
}
# Regional Factors
regional_pfs <- 1:nrow(regions) %>% lapply(function(i) {
  info <- regions[i, ]
  reg_pf <- lms_returns %>% regional_data(mkt=market, countries = unlist(info$country_codes), date_col = "eom", char_col = "characteristic", 
                                          weighting = settings$regional_pfs$country_weights,
                                          countries_min = info$countries_min, periods_min = settings$regional_pfs$months_min, 
                                          stocks_min = settings$regional_pfs$stocks_min)
  reg_pf %>% mutate(region = info$name) %>% select(region, characteristic, direction, eom, n_countries, ret_ew, ret_vw, ret_vw_cap, mkt_vw_exc)
}) %>% rbindlist() 
if (settings$daily_pf) {
  regional_pfs_daily <- 1:nrow(regions) %>% lapply(function(i) {
    info <- regions[i, ]
    reg_pf <- lms_daily %>% regional_data(mkt=market_daily, countries = unlist(info$country_codes), date_col = "date", char_col = "characteristic", 
                                          weighting = settings$regional_pfs$country_weights,
                                          countries_min = info$countries_min, periods_min = settings$regional_pfs$months_min*21, 
                                          stocks_min = settings$regional_pfs$stocks_min)
    reg_pf %>% mutate(region = info$name) %>% select(region, characteristic, direction, date, n_countries, ret_ew, ret_vw, ret_vw_cap, mkt_vw_exc)
  }) %>% rbindlist() 
}
# Regional Cluster Portfolios
regional_clusters <- 1:nrow(regions) %>% lapply(function(i) {
  info <- regions[i, ]
  reg_pf <- cluster_pfs %>% rename("n_stocks_min"=n_factors) %>% mutate(direction = NA_real_) %>% # Hack to make the function applicable
    regional_data(mkt=market, countries = unlist(info$country_codes), date_col = "eom", char_col = "cluster",
                  weighting = settings$regional_pfs$country_weights,
                  countries_min = info$countries_min, periods_min = settings$regional_pfs$months_min, 
                  stocks_min = 1)
  reg_pf %>% mutate(region = info$name) %>% select(region, cluster, eom, n_countries, ret_ew, ret_vw, ret_vw_cap, mkt_vw_exc)
}) %>% rbindlist() 
if (settings$daily_pf) {
  regional_clusters_daily <- 1:nrow(regions) %>% lapply(function(i) {
    info <- regions[i, ]
    reg_pf <- cluster_pfs_daily %>% rename("n_stocks_min"=n_factors) %>% mutate(direction = NA_real_) %>% # Hack to make the function applicable
      regional_data(mkt=market_daily, countries = unlist(info$country_codes), date_col = "date", char_col = "cluster", 
                    weighting = settings$regional_pfs$country_weights,
                    countries_min = info$countries_min, periods_min = settings$regional_pfs$months_min*21, 
                    stocks_min = 1)
    reg_pf %>% mutate(region = info$name) %>% select(region, cluster, date, n_countries, ret_ew, ret_vw, ret_vw_cap, mkt_vw_exc)
  }) %>% rbindlist() 
}

# Save ----------------
if(!is.null(legacy_path)) {
  # Save Time Stamped Files
  folder <- paste0(legacy_path, "/Past Portfolios/", Sys.Date())
  dir.create(folder)
  settings %>% saveRDS(file = paste0(folder, "/settings.RDS"))
  market[eom <= settings$end_date] %>% fwrite(file = paste0(folder, "/market_returns.csv"))
  market_daily[date <= settings$end_date] %>% fwrite(file = paste0(folder, "/market_returns_daily.csv"))
  hml_returns[eom <= settings$end_date] %>% fwrite(file = paste0(folder, "/hml.csv"))
  cmp_returns[eom <= settings$end_date] %>% fwrite(file = paste0(folder, "/cmp.csv"))
  if (settings$daily_pf) {
    lms_daily[date <= settings$end_date] %>% fwrite(file = paste0(folder, "/lms_daily.csv"))
  }
  if (settings$ind_pf) {
    gics_returns[eom <= settings$end_date] %>% fwrite(file = paste0(folder, "/industry_gics.csv"))
    if (nrow(ff49_returns) != 0) {
      ff49_returns[eom <= settings$end_date] %>% fwrite(file = paste0(folder, "/industry_ff49.csv"))
    }
  }
}
# Save Most Recent Files
market[eom <= settings$end_date] %>% fwrite(file = paste0(output_path, "/market_returns.csv"))
pf_returns[eom <= settings$end_date] %>% fwrite(file = paste0(output_path, "/pfs.csv"))
hml_returns[eom <= settings$end_date] %>% fwrite(file = paste0(output_path, "/hml.csv"))
lms_returns[eom <= settings$end_date] %>% fwrite(file = paste0(output_path, "/lms.csv"))
cmp_returns[eom <= settings$end_date] %>% fwrite(file = paste0(output_path, "/cmp.csv"))
cluster_pfs[eom <= settings$end_date] %>% fwrite(file = paste0(output_path, "/clusters.csv"))
if (settings$daily_pf) {
  market_daily[date <= settings$end_date] %>% fwrite(file = paste0(output_path, "/market_returns_daily.csv"))
  pf_daily[date <= settings$end_date] %>% fwrite(file = paste0(output_path, "/pfs_daily.csv"))
  hml_daily[date <= settings$end_date] %>% fwrite(file = paste0(output_path, "/hml_daily.csv"))
  lms_daily[date <= settings$end_date] %>% fwrite(file = paste0(output_path, "/lms_daily.csv"))
  cluster_pfs_daily[date <= settings$end_date] %>% fwrite(file = paste0(output_path, "/clusters_daily.csv"))
}
if (settings$ind_pf) {
  gics_returns[eom <= settings$end_date] %>% fwrite(file = paste0(output_path, "/industry_gics.csv"))
  if (nrow(ff49_returns) != 0) {
    ff49_returns[eom <= settings$end_date] %>% fwrite(file = paste0(output_path, "/industry_ff49.csv"))
  }
}

# Regional Factors
reg_folder <- paste0(output_path, "/Regional Factors")
if (!dir.exists(reg_folder)) {
  dir.create(reg_folder)
}
for (reg in unique(regional_pfs$region)) {
  regional_pfs[eom <= settings$end_date & region %in% reg] %>% fwrite(file = paste0(reg_folder, "/", str_to_sentence(reg), ".csv"))
}
if (settings$daily_pf) {
  reg_folder_daily <- paste0(output_path, "/Regional Factors Daily")
  if (!dir.exists(reg_folder_daily)) {
    dir.create(reg_folder_daily)
  }
  for (reg in unique(regional_pfs_daily$region)) {
    regional_pfs_daily[date <= settings$end_date & region %in% reg] %>% fwrite(file = paste0(reg_folder_daily, "/", str_to_sentence(reg), ".csv"))
  }
}
# Regional Clusters
reg_folder <- paste0(output_path, "/Regional Clusters")
if (!dir.exists(reg_folder)) {
  dir.create(reg_folder)
}
for (reg in unique(regional_clusters$region)) {
  regional_clusters[eom <= settings$end_date & region %in% reg] %>% fwrite(file = paste0(reg_folder, "/", str_to_sentence(reg), ".csv"))
}
if (settings$daily_pf) {
  reg_folder_daily <- paste0(output_path, "/Regional Clusters Daily")
  if (!dir.exists(reg_folder_daily)) {
    dir.create(reg_folder_daily)
  }
  for (reg in unique(regional_clusters_daily$region)) {
    regional_clusters_daily[date <= settings$end_date & region %in% reg] %>% fwrite(file = paste0(reg_folder_daily, "/", str_to_sentence(reg), ".csv"))
  }
}

# Save Long/Short Factors by Country
cnt_folder <- paste0(output_path, "/Country Factors")
if (!dir.exists(cnt_folder)) {
  dir.create(cnt_folder)
}
for (exc in unique(lms_returns$excntry)) {
  lms_returns[eom <= settings$end_date & excntry==exc] %>% fwrite(file = paste0(cnt_folder, "/", exc, ".csv"))
}
if (settings$daily_pf) {
  cnt_folder_daily <- paste0(output_path, "/Country Factors Daily")
  if (!dir.exists(cnt_folder_daily)) {
    dir.create(cnt_folder_daily)
  }
  for (exc in unique(lms_daily$excntry)) {
    lms_daily[date <= settings$end_date & excntry==exc] %>% fwrite(file = paste0(cnt_folder_daily, "/", exc, ".csv"))
  }
}

# Save supplementary information
nyse_size_cutoffs %>% fwrite(file = paste0(output_path, "/nyse_cutoffs.csv"))
ret_cutoffs %>% fwrite(file = paste0(output_path, "/return_cutoffs.csv"))
if (settings$daily_pf) {
  ret_cutoffs_daily %>% fwrite(file = paste0(output_path, "/return_cutoffs_daily.csv"))
}
