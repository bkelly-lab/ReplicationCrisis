# Prepare Support Data ---------------------------------
# Market Returns
market_returns <- fread(paste0(portfolio_path, "/market_returns.csv"), colClasses = c("eom"="character"))
market_returns[, eom := eom %>% as.Date(format = "%Y-%m-%d")]
market_returns <- market_returns[, .(excntry, eom, mkt_vw_exc, stocks, me_lag1)]
market_returns <- market_returns[
  eom >= settings$start_date & eom <= settings$end_date &
    !(excntry %in% settings$country_excl) &   
    !(excntry == "PER" & eom == as.Date("1992-01-31") & mkt_vw_exc >= 8900) &  # Huge outlier 
    !(excntry == "VEN" & eom == as.Date("2018-02-28") & mkt_vw_exc < -1)]      # Something is clearly wrong

# Labels 
char_info <- readxl::read_xlsx("Factor Details.xlsx",
                               sheet = "details", range = "A1:N300") %>%
  select("characteristic"=abr_jkp, direction, significance, date_range = `in-sample period`, "hxz_group"=group) %>%
  filter(!is.na(characteristic)) %>%
  mutate(
    direction = direction %>% as.integer,
    sample_start = date_range %>% str_extract("^\\d+") %>% as.integer(),
    sample_end = date_range %>% str_extract("\\d+$") %>% as.integer()
  )
base_chars <- char_info$characteristic

# Country Classification
country_classification <- readxl::read_xlsx("Country Classification.xlsx", 
                                            sheet = "countries", range = "A1:I200") %>%
  select(excntry, msci_development, region) %>%
  filter(!is.na(excntry)) %>%
  setDT()

# Regions
region_info <- tibble(
  name = c("us", "developed", "emerging", "frontier", "world", "world_ex_us"),
  country_codes = list(
    "USA",
    country_classification[msci_development == "developed" & excntry != "USA"]$excntry,
    country_classification[msci_development == "emerging"]$excntry,
    country_classification[msci_development == "frontier"]$excntry,
    country_classification$excntry,
    country_classification[excntry != "USA"]$excntry
  ),
  countries_min = c(1, rep(settings$countries_min, 3), 1, 3)
)


# Summary Statistic (for Plotting)
country_info <- fread("Country Stats.csv", colClasses = c("eom"="character"))
country_info[, eom := eom %>% as.Date(format="%Y%m%d")]
country_info <- country_classification[country_info, on = "excntry"]

# Prepare Data --------------------------------------------------------
# HML ----------------------
hml <- fread(paste0(portfolio_path, "/hml.csv"), colClasses = c("eom"="character"))
hml[, eom := eom %>% as.Date(format = "%Y-%m-%d")]
# Choose weighting
hml[excntry == "USA", ret := case_when(
  settings$weighting$us == "vw" ~ ret_vw,
  settings$weighting$us == "ew" ~ ret_ew,
  settings$weighting$us == "vw_cap" ~ ret_vw_cap
)]
hml[excntry != "USA", ret := case_when(
  settings$weighting$global_ex_us == "vw" ~ ret_vw,
  settings$weighting$global_ex_us == "ew" ~ ret_ew,
  settings$weighting$global_ex_us == "vw_cap" ~ ret_vw_cap
)]
# Screens
hml <- hml %>%
  filter(
    characteristic %in% base_chars,
    eom >= settings$start_date & eom <= settings$end_date,
    !is.na(ret) & n_stocks_min >= settings$n_stocks_min,  # Min Stocks is the big one, it removes close to 50% of the obs. with n_stocks_min>=10. Perhaps it's too stringent.
    !(excntry %in% settings$country_excl)
  ) %>%
  select(-signal, -n_stocks_min)
# Set direction as original study
hml <- hml %>%
  left_join(char_info %>% select(characteristic, direction), by = "characteristic") %>%
  mutate(ret = ret * direction) %>%
  select(-ret_vw, -ret_ew, -ret_vw_cap)
# Ensure no Duplicates
if(hml[, .N, by = .(characteristic, excntry, eom)][, max(N)] > 1) {
  warning("THE DATA HAS DUPLICATES")
} 

# Regional Portfolios ------------------------------------------------
regional_data <- function(data, countries, weighting, countries_min, months_min, size_grps = F) {
  # Determine Country Weights
  weights <- market_returns[, .(excntry, eom, mkt_vw_exc, "country_weight" = case_when(
    weighting == "market_cap" ~ me_lag1,
    weighting == "stocks" ~ as.double(stocks),
    weighting == "ew" ~ 1)
  )]
  # Portfolio Return 
  pf <- data[excntry %in% countries]
  pf <- weights[pf, on = .(excntry, eom)]
  if (size_grps) {
    pf <- pf[, .(
      n = .N,
      ret = sum(ret*country_weight) / sum(country_weight),
      mkt_vw_exc = sum(mkt_vw_exc * country_weight) / sum(country_weight) 
    ), by = .(characteristic, size_grp, eom)]
  } else {
    pf <- pf[, .(
      n = .N,
      ret = sum(ret*country_weight) / sum(country_weight),
      mkt_vw_exc = sum(mkt_vw_exc * country_weight) / sum(country_weight) 
    ), by = .(characteristic, eom)]
  }
  # Minimum Requirement: Countries
  pf <- pf[n >= countries_min]
  # Minimum Requirement: Months
  pf[, months := .N, by = .(characteristic)]
  pf <- pf[months >= months_min]
  return(pf)
}
regional_pfs <- 1:nrow(region_info) %>% lapply(function(i) {
  info <- region_info[i, ]
  reg_pf <- hml %>% regional_data(countries = unlist(info$country_codes), weighting = settings$country_weighting,
                                  countries_min = info$countries_min, months_min = settings$months_min)
  reg_pf %>% mutate(region = info$name)
}) %>% bind_rows() 

# Characteristic Managed Portfolios ----------------------
cmp <- fread(paste0(portfolio_path, "/cmp.csv"), colClasses = c("eom"="character"))
cmp[, eom := eom %>% as.Date(format="%Y-%m-%d")]

# Screens
cmp <- cmp %>%
  rename(ret = ret_weighted) %>%
  filter(
    characteristic %in% base_chars,
    eom >= settings$start_date & eom <= settings$end_date,
    !is.na(ret) & signal_weighted != 0 & n_stocks >= settings$n_stocks_min * 2, 
    !(excntry %in% settings$country_excl)
  ) %>%
  select(-signal_weighted, -n_stocks)
# Determine Direction
cmp <- cmp %>%
  left_join(char_info %>% select(characteristic, direction), by = "characteristic") %>%
  mutate(ret = ret * direction)
# Ensure no Duplicates
if(cmp[, .N, by = .(characteristic, excntry, size_grp, eom)][, max(N)] > 1) {
  warning("THE DATA HAS DUPLICATES")
} 
# Regional Portfolios
region_info_cmp <- region_info %>% filter(name == "us")
regional_pfs_cmp <- 1:nrow(region_info_cmp) %>% lapply(function(i) {
  info <- region_info[i, ]
  reg_pf <- cmp %>% regional_data(countries = unlist(info$country_codes), weighting = settings$country_weighting,
                                  countries_min = info$countries_min, months_min = settings$months_min, size_grps = T)
  reg_pf %>% mutate(region = info$name)
}) %>% bind_rows() 

# Regional Market Returns ---
regional_mkt_ret <- 1:nrow(region_info) %>% lapply(function(i) {
  info <- region_info[i, ]
  mkt <- market_returns[excntry %in% unlist(info$country_codes), .(n = .N, market = sum(mkt_vw_exc * me_lag1) / sum(me_lag1)), by = eom]
  mkt <- mkt[n >= info$countries_min][, n:= NULL]
  mkt %>% mutate(region = info$name)
}) %>% bind_rows() 

print(paste("Total Characteristics:", uniqueN(regional_pfs$characteristic)))
