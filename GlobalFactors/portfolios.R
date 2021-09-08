library(lubridate)
library(tidyverse)
library(data.table)

# How To --------------------
# Paths
# - data_path:    Set to path with global characteristics data.
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
data_path <- "../Public/Data"
output_path <- "../Public/PaperFactors"
legacy_path <- "../Pre-Public/Data"
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
  "gp_at",               "gp_atl1",             "intrinsic_value",     "inv_gr1",            
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
  end_date = as.Date("2020-12-31"),
  pfs = 3,
  data_source = c("CRSP", "COMPUSTAT"),
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
  )
)

# Portfolio Function -------------
portfolios <- function(
  data_path,
  excntry,
  chars,
  data_source = c("CRSP", "COMPUSTAT"),  # Use data from "CRSP", "COMPUSTAT", or both c("CRSP", "COMPUSTAT")
  wins_ret = T,                          # Should Compustat returns be winsorized at the 0.1% and 99.9% of CRSP returns? 
  pfs,                                   # Number of portfolios 
  bps,                                   # What should breakpoints be based on? Non-Microcap stocks ("non_mc") or NYSE stocks "nyse"
  bp_min_n,                              # Minimum number of stocks used for breakpoints
  cmp = F,                               # Create characteristics managed size portfolios?
  signals = F,                           # Create portfolio signals?
  signals_standardize = F,               # Map chars to [-0.5, +0.5]?,
  signals_w = "vw_cap",                  # Weighting for signals: in c("ew", "vw", "vw_cap")
  nyse_size_cutoffs,                     # Data frame with NYSE size breakpoints
  crsp_ret_cutoffs = NULL                # Data frame with CRSP return information. Neccesary when wins_ret=T
) {
  # Characteristic Data
  data <- fread(paste0(data_path, "/Characteristics/", excntry, ".csv"), select = c("excntry", "id", "eom", "source", "comp_exchg", "crsp_exchcd", "size_grp", "ret_exc", "ret_exc_lead1m", "me", chars), colClasses = c("eom"="character"))
  data[, eom := as.Date(lubridate::fast_strptime(eom, format = "%Y%m%d"))]
  # ME CAP
  data <- nyse_size_cutoffs[, .(eom, nyse_p80)][data, on = "eom"]
  data[, me_cap := pmin(me, nyse_p80)][, nyse_p80 := NULL]
  # Screens
  data <- data[source %in% data_source]
  data <- data[!is.na(size_grp) & !is.na(me) & !is.na(ret_exc_lead1m)]
  # Winsorize Returns?
  if (wins_ret) {
    data <- crsp_ret_cutoffs[, .("eom" = eom_lag1, "p001"=ret_exc_0_1, "p999"=ret_exc_99_9)][data, on = "eom"]
    data[source == "COMPUSTAT" & ret_exc_lead1m > p999, ret_exc_lead1m := p999]
    data[source == "COMPUSTAT" & ret_exc_lead1m < p001, ret_exc_lead1m := p001]
    data[, c("source", "p001", "p999") := NULL]
  }
  # Standardize to [-0.5, +0.5] interval (for signals)
  if (signals_standardize & signals) {
    data[, (chars) := lapply(.SD, function(x) frank(x, ties.method = "min", na.last = "keep")), .SDcols = chars, by = eom]
    data[, (chars) := lapply(.SD, as.numeric), .SDcols = chars]
    data[, (chars) := lapply(.SD, function(x) x / max(x, na.rm=T) - 0.5), .SDcols = chars, by = eom]
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
      sub <- data[!is.na(var), .(eom, var, size_grp, ret_exc_lead1m, me, me_cap, crsp_exchcd, comp_exchg)]
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
      # Output
      return(op)  
    } 
  })
  output$pf_returns <- char_pfs %>% lapply(function(x) x$pf_returns) %>% rbindlist()
  if (nrow(output$pf_returns) != 0) {
    output$pf_returns[, excntry := str_to_upper(excntry)]
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
# NYSE Cutoff 
nyse_size_cutoffs <- fread(paste0(data_path, "/nyse_cutoffs.csv"), colClasses = c("eom"="character"))
nyse_size_cutoffs[, eom := as.Date(eom, format = "%Y%m%d")]

# CRSP Return Cutoffs
crsp_ret_cutoffs <- fread(paste0(data_path, "/crsp_return_cutoffs.csv"), colClasses = c("eom"="character"))
crsp_ret_cutoffs[, eom := as.Date(eom, format = "%Y%m%d")]
crsp_ret_cutoffs[, eom_lag1 := floor_date(eom, unit = "month") - 1]  # Because we use ret_exc_lead1m

# Market 
market <- fread(paste0(data_path, "/market_returns.csv"), colClasses = c("eom"="character"))
market[, eom := eom %>% as.Date("%Y%m%d")]

# Create Portfolios -----------------------
portfolio_data <- countries %>% lapply(function(ex) {
  print(paste0(ex, ": ", match(ex, countries), " out of ", length(countries)))
  portfolios(
    data_path = data_path,
    excntry = ex, 
    chars = chars, 
    data_source = settings$data_source, 
    wins_ret = settings$wins_ret, 
    pfs=settings$pfs, 
    bps=settings$bps, 
    bp_min_n=settings$bp_min_n, 
    cmp = if_else(ex == "usa", settings$cmp$us, settings$cmp$int),
    signals=if_else(ex == "usa", settings$signals$us, settings$signals$int),
    signals_standardize=settings$signals$standardize, 
    signals_w=settings$signals$weight, 
    nyse_size_cutoffs = nyse_size_cutoffs, 
    crsp_ret_cutoffs = crsp_ret_cutoffs
  )
})

# Extract Portfolio Returns
pf_returns <- portfolio_data %>% lapply(function(x) x$pf_returns) %>% rbindlist() 

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

# Extract Signals (TBD)

# Extract CMP returns
cmp_returns <- portfolio_data %>% lapply(function(x) x$cmp) %>% rbindlist() 

# Save ----------------
if(!is.null(legacy_path)) {
  # Save Time Stamped Files
  folder <- paste0(legacy_path, "/Past Portfolios/", Sys.Date())
  dir.create(folder)
  settings %>% saveRDS(file = paste0(folder, "/settings.RDS"))
  market[eom <= settings$end_date] %>% fwrite(file = paste0(folder, "/market_returns.csv"))
  pf_returns[eom <= settings$end_date] %>% fwrite(file = paste0(folder, "/pfs.csv"))
  hml_returns[eom <= settings$end_date] %>% fwrite(file = paste0(folder, "/hml.csv"))
  cmp_returns[eom <= settings$end_date] %>% fwrite(file = paste0(folder, "/cmp.csv"))
}
# Save Most Recent Files
settings %>% saveRDS(file = paste0(output_path, "/settings.RDS"))
market[eom <= settings$end_date] %>% fwrite(file = paste0(output_path, "/market_returns.csv"))
pf_returns[eom <= settings$end_date] %>% fwrite(file = paste0(output_path, "/pfs.csv"))
hml_returns[eom <= settings$end_date] %>% fwrite(file = paste0(output_path, "/hml.csv"))
cmp_returns[eom <= settings$end_date] %>% fwrite(file = paste0(output_path, "/cmp.csv"))
