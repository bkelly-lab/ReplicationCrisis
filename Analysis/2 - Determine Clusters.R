# Hierachical Clustering ----------------------------------------------
factor_hcl <- function(cor_mat, linkage = "ward.D", k, direction_bars = T) {
  dist_mat <- as.dist((1-cor_mat))
  # dist_mat <- as.dist(sqrt((1-cor_mat)*2)) # With ward.D2 gives the same clusters
  hcl <- dist_mat %>% 
    hclust(method=linkage)
  print(str_c("Cophenetic Correlation between Dendogram and Distance Matrix = ", 
              format(cor(cophenetic(hcl), dist_mat), digits = 2, nsmall = 2)))
  
  hcl_labels <- hcl %>%
    cutree(k = k) %>%
    as_tibble(rownames = "characteristic") %>%
    setDT() %>% 
    setnames(c("characteristic", "hcl"))

  hcl_col <- rep(colours_theme[c(1, 2, 3, 4, 5, 7, 9, 11)], ceiling(k/8))[1:k]
  
  dend <- hcl %>% 
    as.dendrogram() %>%
    dendextend::set("labels_col", value = hcl_col, k=k) %>%
    dendextend::set("branches_k_color", value = hcl_col, k=k) %>%
    dendextend::set("labels_cex", value = 0.5) %>%
    dendextend::set("branches_lty", 1) %>%
    dendextend::set("branches_lwd", 0.2) 
  dend %>% plot(horiz=T)
  
  return_list <- list(
    "cor" = cor_mat,
    "labels" = hcl_labels,
    "dend" = dend
  )
  
  if (direction_bars) {
    bar_colours <- tibble("characteristic" = colnames(cor_mat)) %>%
      left_join(char_info %>% select(characteristic, direction), by = "characteristic") %>% 
      mutate(col_dir = if_else(direction == 1, "black", "white"))
    colored_bars(colors = bar_colours %>% select(col_dir), dend = dend, rowLabels = c("Long High"), y_shift = 3, horiz = T)
    return_list$bar_colours <- bar_colours
  }
  return(return_list)
}

hcl_input <- function(data, ret_type = "alpha", ...) { # ret_type %in% c("raw", "alpha")
  data <- copy(data)  # Avoid modifying in place
  if (ret_type == "raw") {
    data[, ret_hcl := ret]
  }
  if (ret_type == "alpha") {
    data[, ret_hcl := ret - mkt_vw_exc * cov(ret, mkt_vw_exc)/var(mkt_vw_exc), by = characteristic]
  }
  data %>%
    select(characteristic, eom, ret_hcl) %>%
    spread(key = characteristic, value = ret_hcl) %>%
    select(-eom) %>%
    cor(...)  
}

# US Clusters -----------
clusters <- regional_pfs %>%
  filter(region == settings$hcl$region & year(eom) >= settings$hcl$start_year) %>%
  hcl_input(ret_type = settings$hcl$ret_type, method = settings$hcl$cor_method, use = "pairwise.complete.obs") %>%
  factor_hcl(linkage = settings$hcl$linkage, k = settings$hcl$k, direction_bars = T)

# Cluster Labels
if (settings$weighting$us == "vw_cap" & settings$hcl$k == 13 & settings$hcl$region == "us" & settings$hcl$start_year == 1975) {
  clusters$labels <- clusters$labels %>% 
    mutate(
      hcl_label = case_when(
        hcl == 1 ~ "Low Leverage",  
        hcl == 2 ~ "Investment",     
        hcl == 3 ~ "Size", 
        hcl == 4 ~ "Value",  
        hcl == 5 ~ "Quality", 
        hcl == 6 ~ "Low Risk", 
        hcl == 7 ~ "Debt Issuance", 
        hcl == 8 ~ "Seasonality", 
        hcl == 9 ~ "Accruals",   
        hcl == 10 ~ "Profitability",
        hcl == 11 ~ "Profit Growth",
        hcl == 12 ~ "Skewness",
        hcl == 13 ~ "Momentum",
        TRUE ~ as.character(hcl))
    ) 
} else {
  clusters$labels <- clusters$labels %>% 
    mutate(hcl_label = hcl)
}
# Output
cluster_labels <- clusters$labels %>%
  select(-hcl)
