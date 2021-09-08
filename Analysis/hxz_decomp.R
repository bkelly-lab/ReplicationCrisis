library(lubridate)
library(tidyverse)
library(data.table)
options(dplyr.summarise.inform = FALSE)

# User Input -----------------------
# Paths
data_path <- "C:/Users/tij.fi/Dropbox (CBS)/International Stock Data/Public/Data" # Set to path with global characteristics data.
# Start Date
start <- as.Date("2019-12-31")

# Data -----------------------------
# Characteristics
char_info <- readxl::read_xlsx("Factor Details.xlsx",
                               sheet = "details", range = "A1:N300") %>%
  filter(!is.na(abr_jkp)) %>%
  select("characteristic"=abr_jkp, direction, significance) %>%
  mutate(direction = direction %>% as.numeric()) %>%
  setDT()
chars <- char_info$characteristic

# NYSE Cutoff 
nyse_size_cutoffs <- fread(paste0(data_path, "/nyse_cutoffs.csv"), colClasses = c("eom"="character"))
nyse_size_cutoffs[, eom := as.Date(eom, format = "%Y%m%d")]

# CRSP Return Cutoffs
crsp_ret_cutoffs <- fread(paste0(data_path, "/crsp_return_cutoffs.csv"), colClasses = c("eom"="character"))
crsp_ret_cutoffs[, eom := as.Date(eom, format = "%Y%m%d")]
crsp_ret_cutoffs[, eom_lag1 := floor_date(eom, unit = "month") - 1]  # Because we use ret_exc_lead1m

# Data
data <- fread(paste0(data_path, "/Characteristics/usa.csv"), 
              select = c("excntry", "id", "eom", "source", "comp_exchg", "crsp_exchcd", "size_grp", "ret_exc", "ret_exc_lead1m", "me", chars), colClasses = c("eom"="character"))
data[, eom := as.Date(lubridate::fast_strptime(eom, format = "%Y%m%d"))]
# ME CAP
data <- nyse_size_cutoffs[, .(eom, nyse_p80)][data, on = "eom"]
data[, me_cap := pmin(me, nyse_p80)][, nyse_p80 := NULL]
# Screens
data <- data[!is.na(size_grp) & !is.na(me) & !is.na(ret_exc_lead1m)]

# Winsorize Compustat Returns
data <- crsp_ret_cutoffs[, .(eom, "p001"=ret_exc_0_1, "p999"=ret_exc_99_9)][data, on = "eom"]
data[source == "COMPUSTAT" & ret_exc > p999, ret_exc := p999]
data[source == "COMPUSTAT" & ret_exc < p001, ret_exc := p001]
data[, c("p001", "p999") := NULL]
data <- crsp_ret_cutoffs[, .("eom" = eom_lag1, "p001"=ret_exc_0_1, "p999"=ret_exc_99_9)][data, on = "eom"]
data[source == "COMPUSTAT" & ret_exc_lead1m > p999, ret_exc_lead1m := p999]
data[source == "COMPUSTAT" & ret_exc_lead1m < p001, ret_exc_lead1m := p001]
data[, c("source", "p001", "p999") := NULL]

# Create 1 month separated observations
returns <- tidyr::crossing("id" = unique(data$id), "eom" = unique(data$eom)) %>% setDT()
returns <- data[, .(id, eom, ret_exc)][returns, on = .(id, eom)]
returns[, start := min(eom[!is.na(ret_exc)]), by = id]
returns <- returns[eom >= start][, start := NULL]
returns[, last := floor_date(max(eom[!is.na(ret_exc)]), unit = "month") + months(12) - 1, by = id]  # To avoid lookahead bias, _horizon_ months after last obs
returns <- returns[eom <= last][, last := NULL]
returns %>% setorder(id, eom)

pf_func <- function(chars, pfs, bps, bp_min_n, horizon) {
  # Realized Returns 
  ret_lead <- 1:horizon %>% lapply(function(h) {
    if (h==1) {
      r <- data[, .(id, eom, lead = 1, ret_exc = ret_exc_lead1m)][!is.na(ret_exc)]
    } else {
      r <- returns[, .(eom, lead = h, ret_exc = dplyr::lead(ret_exc, n = h)), by = id][!is.na(ret_exc)]
    }
    r[, eom_ret := ceiling_date(eom, unit = "months")+months(h)-1]
  }) %>% rbindlist()
  # Portfolios 
  chars %>% lapply(function(x) {
    print(paste0("   " , x, ": ", match(x, chars), " out of ", length(chars)))
    data[, var := as.double(get(x))]
    sub <- data[!is.na(var), .(id, eom, var, size_grp, me, me_cap, crsp_exchcd, comp_exchg)]
    # Portfolio Assignment
    if (bps == "nyse") {
      sub[, bp_stock := (crsp_exchcd == 1 & is.na(comp_exchg)) | (comp_exchg == 11 & is.na(crsp_exchcd))]
    }
    if (bps == "non_mc") {
      sub[, bp_stock := (size_grp %in% c("mega", "large", "small"))]
    }
    sub[, bp_n := sum(bp_stock), by = eom]
    sub <- sub[bp_n >= bp_min_n] # require at least 10 stocks for break points
    sub[, cdf := ecdf(var[bp_stock == T])(var), by = eom]
    sub[, min_cdf := min(cdf), by = eom]
    sub[cdf == min_cdf, cdf := 0.00000001] # To ensure that the lowest value is in portfolio 1   
    sub[, pf := ceiling(cdf*pfs), by = eom]  
    sub[pf == 0, pf := 1]  # Happens when non-bp stocks extend beyond bp stock range
    # Align with returns
    sub <- sub[, .(id, eom, me, me_cap, pf)][ret_lead, on = .(id, eom)][!is.na(pf)]
    # Returns 
    pf_returns <- sub[, .(
      characteristic = x,
      n = .N,
      ret_ew = mean(ret_exc),
      ret_vw = sum(ret_exc * me) / sum(me),
      ret_vw_cap = sum(ret_exc * me_cap) / sum(me_cap)
    ), by = .(pf, eom_ret, lead)]
    # HML
    pf_returns[, .(
      characteristic = x,
      ret_ew = ret_ew[pf == pfs] - ret_ew[pf == 1],
      ret_vw = ret_vw[pf == pfs] - ret_vw[pf == 1],
      ret_vw_cap = ret_vw_cap[pf == pfs] - ret_vw_cap[pf == 1]
    ), by = .(eom_ret, lead)][!is.na(ret_ew)]
  }) %>% rbindlist()
}

# Output 
system.time(hml_nonmc3 <- chars %>% pf_func(pfs = 3, bps = "non_mc", bp_min_n = 5, horizon = 12))  # 47 min
system.time(hml_nyse10 <- chars %>% pf_func(pfs = 10, bps = "nyse", bp_min_n = 5, horizon = 12))


rr <- list(hml_nonmc3, hml_nyse10) %>% lapply(function(dt) {
  repl_data <- char_info[dt, on = "characteristic"] %>%
    filter(eom_ret <= start) %>%
    mutate(
      ret_ew = ret_ew*direction,
      ret_vw = ret_vw*direction,
      ret_vw_cap = ret_vw_cap*direction
    ) %>%
    pivot_longer(c(ret_ew, ret_vw, ret_vw_cap), names_to = "type", values_to = "ret")
  
  repl_data <- c(1, 6, 12) %>% lapply(function(h) {
    repl_data %>%
      filter(lead %in% 1:h) %>%
      group_by(characteristic, eom_ret, type, significance) %>%
      filter(n() == h) %>%
      summarise(ret = mean(ret)) %>%
      ungroup() %>%
      mutate(horizon = h)
  }) %>% bind_rows()
  
  rr_func <- function(sample) {
    sample %>%
      group_by(characteristic, type, horizon) %>%
      summarise(
        t = mean(ret)/(sd(ret)/sqrt(n()))
      ) %>%
      group_by(type) %>%
      summarise(
        n = n(),
        rr = mean(t > 1.96)
      )
  }
  
  new_factors <- c(
    "ret_3_1", "ret_9_1",  "ret_12_7",  "corr_1260d",  "rmax5_21d", "rmax5_rvol_21d",
    "ni_be", "ocf_at", "ocf_at_chg1", "mispricing_perf", "mispricing_mgmt",  "qmj",
    "qmj_prof", "qmj_growth",  "qmj_safety")
  
  
  # Our Benchline Raw Return
  baseline <- repl_data %>%
    filter(horizon == 1 & eom_ret <= as.Date("2019-12-31")) %>%
    rr_func() %>%
    mutate(name = "Baseline")
  # Difference in sample period
  hor_diff <- repl_data %>%
    rr_func() %>%
    mutate(name = "Three Horizons")
  # Difference in horizons
  sample_diff <- repl_data %>%
    filter(eom_ret >= as.Date("1967-01-01") & eom_ret <= as.Date("2016-12-31")) %>%
    rr_func() %>%
    mutate(name = "Three Horizons, Shorter Sample")
  # Differences in Factors
  fct_diff <- repl_data %>%
    filter(eom_ret >= as.Date("1967-01-01") & eom_ret <= as.Date("2016-12-31")) %>%
    filter(!(characteristic %in% new_factors)) %>%
    rr_func() %>%
    mutate(name = "Three Horizons, Shorter Sample, Difference in Factors")
  bind_rows(baseline, sample_diff, hor_diff, fct_diff) %>%
    arrange(type, name)
})

# Decomposition  
terc_base <- rr[[1]] %>% filter(name == "Baseline" & type == "ret_vw_cap") %>% pull(rr)
terc_base_vw <- rr[[1]] %>% filter(name == "Baseline" & type == "ret_vw") %>% pull(rr)
terc_hor <- rr[[1]] %>% filter(name == "Three Horizons" & type == "ret_vw") %>% pull(rr)
terc_sample <- rr[[1]] %>% filter(name == "Three Horizons, Shorter Sample" & type == "ret_vw") %>% pull(rr)
terc_factors <- rr[[1]] %>% filter(name == "Three Horizons, Shorter Sample, Difference in Factors" & type == "ret_vw") %>% pull(rr)
dec_factors <- rr[[2]] %>% filter(name == "Three Horizons, Shorter Sample, Difference in Factors" & type == "ret_vw") %>% pull(rr)

# From 56.2% to 44.4% with value weights
terc_base-terc_base_vw
# Multiple Horizons
terc_base_vw-terc_hor
# Shorter sample 
terc_hor-terc_sample
# New Factors
terc_sample-terc_factors
# Deciles instead of terciles and change of BP
terc_factors-dec_factors
# Explained Difference
(expl_rr <- terc_base-((terc_base-terc_base_vw)+(terc_base_vw-terc_hor)+(terc_hor-terc_sample)+(terc_sample-terc_factors)+(terc_factors-dec_factors)))
expl_rr-0.35
