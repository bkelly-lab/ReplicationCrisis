library(xtable)
library(tidyverse)
library(data.table)

# Data [output from SAS code, already screened with obs_main=1, primary_sec=1, exch_main=1]
data_path <- "../../Data/Characteristics"
country_files <- list.files(data_path)
countries <- country_files %>% lapply(function(file) {
  fread(paste0(data_path, "/", file), select = c("excntry", "id", "eom", "me", "size_grp", "ret_local"))
}) %>% rbindlist()
countries[, eom := eom %>% as.character() %>% lubridate::fast_strptime(format = "%Y%m%d") %>% as.Date()]
# Aggregate by month
country_info <- countries[!is.na(me) & !is.na(ret_local), .(
  n = .N,
  n_nano = sum(size_grp == "nano"),
  n_mega = sum(size_grp == "mega"),
  me = sum(me),
  me_p50 = median(me)
), by = .(excntry, eom)]
# Country Classification
country_classification <- readxl::read_xlsx("Country Classification.xlsx", 
                                            sheet = "countries", range = "A1:C200") %>%
  select(excntry, msci_development) %>%
  filter(!is.na(excntry)) %>%
  setDT()
country_info <- country_classification[country_info, on = "excntry"]

# Table
table_country <- function(country_info, info_date) {
  tbl_caption <- paste("The table shows summary statistics by the country where a security is listed.\\", 
                       "We include common stocks that are the primary security of the underlying firm, traded on a standard exchange, with non-missing return and market equity data.\\",
                       "\\textit{Country} is the ISO code of the underlying exchange country.\\", 
                       "For further information, see \\href{https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes}{https://en.wikipedia.org/wiki/List\\_of\\_ISO\\_3166\\_country\\_codes.}",
                       "\\textit{MSCI} shows the MSCI classification of each country as of January 7th 2021.", 
                       "For the most recent classification, see \\href{https://www.msci.com/market-classification}{https://www.msci.com/market-classification}.",
                       "\\textit{Start} is the first date with a valid observation.",
                       "In the next 4 columns, the data is shown as of December 31st 2020.\\",
                       "\\textit{Stocks} is the number of stocks available.\\", 
                       "\\textit{Mega stocks} is the number of stocks with a market cap above the 80th percentile of NYSE stocks.\\",
                       "\\textit{Total Market Cap} is the aggregate market cap in million USD.\\",
                       "\\textit{Median MC} is the median market cap in million USD.")
  
  # Country summary
  country_info[, start_date := min(eom), by = excntry]
  country_stats <- country_info[eom==info_date]
  countries_add <- country_info[!(excntry %in% country_stats$excntry)][, .(excntry, msci_development, eom = info_date, start_date, n=0, n_mega=0, n_nano=0, me=0, me_p50=0)] %>% unique()# Add countries without data by info_date
  country_stats <- country_stats %>% rbind(countries_add) %>% arrange(-me) %>% select(-c(n_nano, eom))
  
  total <- country_stats %>%
    ungroup() %>%
    summarise(
      excntry = "All",
      msci_development = "",
      start_date = NA,
      n = sum(n),
      n_mega = sum(n_mega),
      me = sum(me),
      me_p50 = NA_real_
    )
  
  country_stats %>%
    bind_rows(total) %>%
    mutate(
      n = n %>% prettyNum(big.mark = ",", digits = 0),
      n_mega = n_mega %>% prettyNum(big.mark=",", digits = 0),
      start_date = as.character(start_date),
      me = me %>% formatC(format = "e", digits = 2),
      me_p50 = me_p50 %>% prettyNum(big.mark=",", digits = 0),
      msci_development = msci_development %>% str_to_title(),
      " "= ' ',
    ) %>%
    select(excntry, msci_development, start_date, ` `, everything()) %>%
    rename("Country" = excntry, "MSCI" = msci_development, "Start" = start_date, "Stocks" = n, "Mega Stocks" = n_mega,
           "Total Market Cap" = me, "Median MC" = me_p50) %>%
    xtable(digits = 0, align = "lllllrrrr", caption = tbl_caption) %>%
    print(include.rownames = T, floating = FALSE, latex.environments = "center", hline.after=c(-1, 0), 
          tabular.environment = "longtable", size="\\fontsize{10pt}{12pt}\\selectfont")
}

# Output for paper--------------------
# Remember:
# Only copy from "& country ..." and down.
# In line 94: 
#  - Delete "94"
#  - Delete "NA"
#  - Add hline above and below total
#  - Make "All" in \textbf{}
country_info[!(excntry %in% c("ZWE", "VEN"))] %>% table_country(info_date = as.Date("2020-12-31"))  # We exclude Zimbamwe and Venesuela due to data issues

# Nano Caps in the US
country_info[eom == as.Date("2020-12-31") & excntry == "USA", .(n, n_nano, nano_prop = n_nano / n)] %>% print()
