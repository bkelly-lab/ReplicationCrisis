# Determine Cluster Order
cluster_order <- c("Accruals", "Debt Issuance", "Investment", "Skewness", "Value",
                   "Low Risk", "Quality", "Momentum", "Profitability", "Profit Growth",
                   "Seasonality", "Size", "Leverage")

# Collect all output in list
output <- list(figures = list(), tables = list())

# Headline Replication Rate
headline_rr <- eb_est$us$factors %>% left_join(char_info, by = "characteristic") %>% filter(significance == 1) %>% summarise(rr = mean(p025>0)) %>% pull(rr)

# Figures --------------------------------------
# HCL
output$figures$hcl_us <- function(tex = F) {
  par(mar = c(3,2,1,10), cex = 1) 
  c <- 1.2
  x <- 37/2
  label_func <- function(x) unique(clusters$labels$hcl_label)[unique(clusters$labels$hcl_label) %>% str_detect(x)]
  
  clusters_tex <- copy(clusters$dend)
  if (tex == T) {
    labels(clusters_tex) <- labels(clusters_tex) %>% str_replace_all("_", "\\\\_")
  }
  
  clusters_tex %>% plot(horiz=T)
  colored_bars(colors = clusters$bar_colours %>% select(col_dir), dend = clusters_tex, rowLabels = c("Long High"), 
               y_shift = 11/2, horiz = T)
  # Labels
  if (settings$hcl$k == 13) {
    text(x = x, y = 147, label_func("Seasonality+"), cex = c, col = colours_theme[5], adj = 0)   #gold-9
    text(x = x, y = 137, label_func("Accruals"), cex = c, col = colours_theme[4], adj = 0)  #lightgreen-7
    text(x = x, y = 131, label_func("Debt Issuance"), cex = c, col = colours_theme[3], adj = 0)  #purple-5
    text(x = x, y = 126, label_func("Size"), cex = c, col = colours_theme[2], adj = 0)       #orange-4
    
    text(x = x, y = 117, label_func("Leverage"), cex = c, col = colours_theme[1], adj = 0)           #darkgreen-3
    text(x = x, y = 106, label_func("Profit Growth"), cex = c, col = colours_theme[11], adj = 0)            #orange-4 
    text(x = x, y = 97, label_func("Momentum"), cex = c, col = colours_theme[9], adj = 0)       #red-2
    text(x = x, y = 85, label_func("Quality"), cex = c, col = colours_theme[7], adj = 0)            #blue-1  
    text(x = x, y = 73, label_func("Skewness"), cex = c, col = colours_theme[5], adj = 0)   #black-11
    text(x = x, y = 63, label_func("Profitability"), cex = c, col = colours_theme[4], adj = 0)            #lightgreen-7
    text(x = x, y = 48, label_func("Value"), cex = c, col = colours_theme[3], adj = 0)   #purple-5
    
    text(x = x, y = 31, label_func("Low Risk"), cex = c, col = colours_theme[2], adj = 0)               #darkgreen-3
    text(x = x, y = 12, label_func("Investment"), cex = c, col = colours_theme[1], adj = 0)        #blue-1
  }
}

# Cluster Validation
output$figures$hcl_us_val <- clusters$cor %>% cluster_val(labels = clusters$labels, op_format = "pdf")

# Literature comparison
print(output$figures$lit_comp <- eb_est$us %>% plot_lit_comp(mt_res = mt, eb_world = eb_est$world, excl_insig=T))

# Comparing Multiple Testing with Empirical Bayes
c(output$figures$mt_factors, output$figures$mt_summary) %<-% plot_mt_eb_comp(
  mt = mt, eb_all = eb_est$all, eb_us = eb_est$us, eb_developed = eb_est$developed, eb_world = eb_est$world,
  eb_emerging = eb_est$emerging, mts = c("OLS", "Bonferroni", "BY"), 
  regs = c("us", "developed", "emerging", "world"), se_methods = c("OLS", "BY", "EB - Region", "EB - All"),
  se_regions = "us")

# Replication Rate by Region
output$figures$gl_by_cluster <- plot_repl_region(eb_all = eb_est$all, cluster_order = cluster_order)

# Global Factor Posterior
output$figures$gl_by_factor <- eb_est$world %>% plot_factor_post(orig_sig = T, cluster_order = cluster_order)

# CI Many Factors
output$figures$ci_many_fcts <- plot_many_factors()

# Tangency Portfolio - World
output$figures$tpf <- tpf_us %>% plot_tpf(cluster_order = cluster_order, ci_low = 0.05, ci_high = 0.95)

# Tangency Portfolio - Regions
output$figures$tpf_regions <- plot_tpf_region(tpf_us = tpf_us, tpf_dev = tpf_dev, tpf_emer = tpf_emer, cluster_order = cluster_order, ci_low = 0.05, ci_high = 0.95)

# Tangency Portfolio - Size Groups
output$figures$tpf_size <- tpf_size %>% plot_tpf_size(cluster_order = cluster_order, ci_low = 0.05, ci_high = 0.95)

# Posterior over Time - Fixed Taus
output$figures$overtime <- posterior_over_time %>% plot_over_time(orig_sig = T, ols_incl = T, lb = 5)

# Posterior over Time - Flexible Taus
output$figures$overtime_flex <- posterior_over_time_flex %>% plot_over_time(orig_sig = T, ols_incl = F, lb=5)

# Posterior over Time - Flexible Taus - Plot taus
output$figures$overtime_flex_taus <- posterior_over_time_flex %>% plot_taus_over_time() 

# By Size - Overall
output$figures$size_overall <- eb_us_size %>% plot_size_overall(flipped = T, text = F)

# By Size - Clusters
output$figures$size_clusters <- eb_us_size %>% plot_size_clusters(cluster_order = cluster_order)

# Model - False Discovery Rate
output$figures$model_fdr <- model_fdr %>% plot_fdr()

# Simulation - False Discovery Rate
output$figures$sim_fdr <- simulation %>% plot_sim_fdr()

# US verus world factor
output$figures$world_vs_us <- plot_world_vs_us(eb_us = eb_est$us, eb_world_ex_us = eb_est$world_ex_us)

# In-sample vs. OOS and Post
c(output$figures$is_pre, output$figures$is_post, output$figures$is_oos) %<-% plot_is_oos_post(is_oos = is_oos, type = "GLS") 

# In-sample vs. OOS and Post - quadratic
c(output$figures$is_pre_quad, output$figures$is_post_quad, output$figures$is_oos_quad) %<-% plot_is_oos_post_quad(is_oos = is_oos, type = "GLS")

# Effect Sizes
c(output$figures$effect_world, output$figures$effect_regions, output$figures$effect_size) %<-% plot_effects(type = "ols", orig_sig = T, cluster_order = cluster_order)

# Replicateion Rate by Cluster - US
output$figures$repl_cluster_us <- eb_est$us$factors %>% plot_repl_cluster(orig_sig = T, cluster_order = cluster_order)

# Simulation benchmarked to Harvey et al (2016)
if (eb_est$us$mle %>% filter(estimate == "tau_s") %>% pull(ml_est) %>% round(2) != 0.21) {
  warning("Tau_w in Harvey et al simulation is inconsistent with data!!")
}
output$figures$sim_harvey <- plot_harvey(harvey_base_res = harvey_base_res, harvey_worst_res = harvey_worst_res, tau_ws = 0.21, act_rr = headline_rr)
output$figures$sim_harvey_robustness <- plot_harvey(harvey_base_res = harvey_base_res, harvey_worst_res = harvey_worst_res, tau_ws = c(0.1, 0.21, 0.3), act_rr = headline_rr)

# TPF Single Factors 
output$figures$tpf_factors_one_cluster <- tpf_factors$wide %>% plot_tpf_one_cluster(cluster_labels = cluster_labels, s = opt_s)
output$figures$tpf_factors_excl_one <- tpf_factors$wide %>% plot_tpf_excl_cl(cluster_labels = cluster_labels, s = opt_s)
output$figures$tpf_factors_imp <- tpf_factors$wide %>% plot_tpf_factor_imp(cluster_labels = cluster_labels, s = opt_s)
output$figures$tpf_factors_imp_cluster <- tpf_factors$wide %>% plot_tpf_factor_imp_cluster(cluster_labels = cluster_labels, s = opt_s)
tpf_evol <- tpf_factors$long %>% plot_tpf_evolution(data_wide = tpf_factors$wide, char_info = char_info, orig_sig_values = settings$tpf_factors$orig_sig, s = opt_s)
output$figures$tpf_evolution <- tpf_evol$plot

# Cummulative returns OOS
output$figures$ts_oos_all <- regional_pfs[region %in% c("us", "world_ex_us")] %>% plot_ts(type="all", oos=T, alphas=T, start = as.Date("1990-01-01"))
output$figures$ts_oos_cluster <- regional_pfs[region %in% c("us", "world_ex_us")] %>% plot_ts(type="cluster", oos=T, alphas=T, start = as.Date("1990-01-01"))

# Save Figures as Pictures -------------------------
if (save_figures) {
  output_fig <- function(path, name, format, width, height) {
    path <- paste0(path, "/", name)   # <- change path as desired 
    if (format == "tex") {
      tikz(str_c(path, ".tex"), width = width, height = height) 
    }
    if (format == "pdf") {
      pdf(str_c(path, ".pdf"), width = width, height = height)
    }
    if (format == "jpg") {
      w_pix <- width / (2.54 / 96 / 2.54)
      h_pix <- height / (2.54 / 96 / 2.54)
      jpeg(str_c(path, ".jpg"), width = w_pix, height = h_pix)
    }
    if (format == "eps") {
      cairo_ps(filename = str_c(path, ".eps"),
               width = width, height = height)
    }
    if (format == "tiff") {
      tiff(filename = str_c(path, ".tiff"), units="in", width=width, height=height, res=500)
    }
  }
  # For Paper -----------------------------------------
  h <- 5
  w <- 9
  format <- "pdf"
  
  # Figure 1
  output_fig(path=output_path, name = "lit_comp", format = format, width = w + 1, height = h)
  output$figures$lit_comp 
  dev.off()
  
  # MT - Factors
  output_fig(path=output_path, name = "mt_factors", format = format, width = w, height = h)
  output$figures$mt_factors + theme(text = element_text(size = 13), axis.text.x = element_blank())
  dev.off()
  
  # CI's 
  output_fig(path=output_path, name = "ci_many_fcts", format = format, width = w, height = h)
  output$figures$ci_many_fcts + theme(axis.text.y = element_text(size = 13), text = element_text(size = 13))
  dev.off()
  
  # MT - Summary
  output_fig(path=output_path, name = "mt_summary", format = format, width = w, height = h)
  output$figures$mt_summary + 
    theme(
      text = element_text(size = 13),
      axis.title.x = element_blank(), 
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9)
    )
  dev.off()
  
  # Global Factors - Factors
  output_fig(path=output_path, name = "gl_by_factor", format = format, width = w, height = h)
  output$figures$gl_by_factor + theme(text = element_text(size = 13), legend.position = "right", axis.text.x = element_text(size = 5)) 
  dev.off()
  
  # Global Factors - Clusters
  output_fig(path=output_path, name = "gl_by_cluster", format = format, width = w, height = h)
  output$figures$gl_by_cluster  + theme(text = element_text(size = 13))
  dev.off()
  
  # Tangency Portfolio - World
  output_fig(path=output_path, name = "tpf", format = format, width = w, height = h)
  output$figures$tpf  + theme(text = element_text(size = 13), axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13))
  dev.off()
  
  # Tangency Portfolio - Regions
  output_fig(path=output_path, name = "tpf_regions", format = format, width = w, height = h)
  output$figures$tpf_regions  + theme(text = element_text(size = 13), axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13))
  dev.off()
  
  # Tangency Portfolio - Size Groups
  output_fig(path=output_path, name = "tpf_size", format = format, width = w, height = h)
  output$figures$tpf_size  + theme(
    text = element_text(size = 13), 
    axis.text.x = element_text(size = 13), 
    axis.text.y = element_text(size = 11))
  dev.off()
  
  # Evidence over time - Fixed Taus
  output_fig(path=output_path, name = "over_time", format = format, width = w, height = h)
  output$figures$overtime
  dev.off()
  
  # Evidence over time - Flexible Taus
  output_fig(path=output_path, name = "over_time_flex", format = format, width = w, height = h)
  output$figures$overtime_flex + theme(text = element_text(size = 13))
  dev.off()
  
  # Evidence over time - Flexible Taus - Taus
  output_fig(path=output_path, name = "over_time_flex_taus", format = format, width = w, height = h)
  output$figures$overtime_flex_taus + theme(text = element_text(size = 13), legend.text = element_text(size = 12))
  dev.off()
  
  # Size - Overall
  output_fig(path=output_path, name = "size_overall", format = format, width = w, height = h)
  output$figures$size_overall + theme(
    axis.title.x = element_text(size = 17),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17))
  dev.off()
  
  # Size - Clusters
  output_fig(path=output_path, name = "size_clusters", format = format, width = w, height = h+1)
  output$figures$size_clusters
  dev.off()
  
  # Hierarchy
  output_fig(path=output_path, name = "us_hcl", format = format, width = w, height = h*2+1.5)
  output$figures$hcl_us()
  dev.off()
  
  # Hierarchical Clusters
  output_fig(path=output_path, name = "us_hcl_pairwise_cor", format = format, width = w-4, height = h)
  output$figures$hcl_us_val()
  dev.off()
  
  # Model FDR
  output_fig(path=output_path, name = "model_fdr", format = format, width = w, height = h)
  output$figures$model_fdr + theme(text = element_text(size = 13))
  dev.off()
  
  # Simulated FDR
  output_fig(path=output_path, name = "sim_fdr", format = format, width = w, height = h)
  output$figures$sim_fdr
  dev.off()
  
  # World vs. US
  output_fig(path=output_path, name = "world_vs_us", format = format, width = h, height = h)
  output$figures$world_vs_us + theme(text = element_text(size = 13)) 
  dev.off()
  
  # IS VS. OOS ---------------
  
  # IS vs. pre/post
  output_fig(path=output_path, name = "is_pre_post", format = format, width = w/3, height = w/3)
  output$figures$is_oos  + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  ) 
  dev.off()
  
  # IS vs. post
  output_fig(path=output_path, name = "is_post", format = format, width = w/3, height = w/3)
  output$figures$is_post  + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  )  
  dev.off()
  
  # IS vs. Pre
  output_fig(path=output_path, name = "is_pre", format = format, width = w/3, height = w/3)
  output$figures$is_pre + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  ) 
  dev.off()
  
  # IS VS. OOS: Quadratic ---------------
  
  # IS vs. pre/post
  output_fig(path=output_path, name = "is_pre_post_quad", format = format, width = w/3, height = w/3)
  output$figures$is_oos_quad  + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  ) 
  dev.off()
  
  # IS vs. post
  output_fig(path=output_path, name = "is_post_quad", format = format, width = w/3, height = w/3)
  output$figures$is_post_quad  + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  )  
  dev.off()
  
  # IS vs. Pre
  output_fig(path=output_path, name = "is_pre_quad", format = format, width = w/3, height = w/3)
  output$figures$is_pre_quad + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  ) 
  dev.off()
  
  # Effect Size - Region
  output_fig(path=output_path, name = "effect_regions", format = format, width = w, height = h)
  output$figures$effect_regions + theme(
    axis.text.y = element_text(size = 13), 
    strip.text.x = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13)
  )
  dev.off()
  
  # Effect Size - Size Grps
  output_fig(path=output_path, name = "effect_size", format = format, width = w, height = h)
  output$figures$effect_size + theme(
    axis.text.y = element_text(size = 13), 
    strip.text.x = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13)
    )
  dev.off()
  
  # Cluster Replication - US
  output_fig(path=output_path, name = "repl_cluster_us", format = format, width = w, height = h)
  output$figures$repl_cluster_us + theme(
    axis.text.y = element_text(size = 13), 
    axis.text.x = element_text(size = 14), 
    text = element_text(size = 14))
  dev.off()
  
  # Harvey et al Simulation - Base tau_w
  output_fig(path=output_path, name = "sim_harvey", format = format, width = w, height = h)
  output$figures$sim_harvey + theme(
    text = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 12)
  )
  dev.off()
  
  # Harvey et al Simulation - Multiple tau_w
  output_fig(path=output_path, name = "sim_harvey_robustness", format = format, width = w, height = h)
  output$figures$sim_harvey_robustness + theme(
    text = element_text(size = 13),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 12)
  )
  dev.off()
  
  # TPF Factor Portfolio (Section H)
  # Cluster + market
  output_fig(path=output_path, name = "cluster_plus_market", format = format, width = w, height = h)
  output$figures$tpf_factors_one_cluster
  dev.off()
  # Cluster importance
  output_fig(path=output_path, name = "excl_cluster", format = format, width = w, height = h)
  output$figures$tpf_factors_excl_one
  dev.off()
  # Factor Importance
  output_fig(path=output_path, name = "factor_tpf_imp", format = format, width = w, height = h*1.5)
  output$figures$tpf_factors_imp + theme(
    axis.text.y = element_text(size = 6)
  )
  dev.off()
  # Factor Importance within Cluster
  output_fig(path=output_path, name = "factor_tpf_imp_cluster", format = format, width = w, height = h*1.5)
  output$figures$tpf_factors_imp_cluster + theme(
    axis.text.y = element_text(size = 6)
  )
  dev.off()
  # Evolution of TPF
  output_fig(path=output_path, name = "tpf_evolution", format = format, width = w, height = h)
  output$figures$tpf_evolution
  dev.off()
  
  # Cumulative OOS Return - Aggregate
  output_fig(path=output_path, name = "ts_oos_all", format = format, width = w, height = h)
  output$figures$ts_oos_all
  dev.off()
  
  # Cumulative OOS Return - Clusters
  output_fig(path=output_path, name = "ts_oos_clusters", format = format, width = w, height = h)
  output$figures$ts_oos_cluster
  dev.off()
  
  # TABLES -----------------
  # Remember:
  # Only copy from "& country ..." and down.
  # In line 94: 
  #  - Delete "94"
  #  - Delete "NA"
  #  - Add hline above and below total
  #  - Make "All" in \textbf{}
  country_info %>% table_country()
  # Estimated Taus
  table_taus()
  # Economic Benefit of more Power
  sig_oos_pfs %>% table_economic_benefit()
  # Factor Performance
  table_factor_info()
  
  # Numbers mentioned in paper --------
  # BY cutoff
  mt %>%
    filter(method == "BY" & region == "us") %>%
    mutate(sig = p<=0.05) %>%
    group_by(sig) %>%
    mutate(
      max = max(abs(statistic)),
      min = min(abs(statistic))
    ) %>%
    filter(abs(statistic)==max & sig==F | abs(statistic)==min & sig==T) %>%
    ungroup() %>%
    summarise(
      by_cutoff = mean(abs(statistic))
    ) %>% 
    print()
  
  # Sales growth factor
  sale_gr_us <- eb_est$us$factors %>% 
    filter(region == "us" & characteristic == "sale_gr1")
  sale_gr_all <- eb_est$all$factors %>% 
    filter(region == "us" & characteristic == "sale_gr1")
  tibble(
    characteristic = rep("sale_gr1",2), 
    region = rep("US", 2), 
    data = c("US", "Global"), 
    post_mean = c(sale_gr_us$post_mean, sale_gr_all$post_mean), 
    post_vol = c(sale_gr_us$post_sd, sale_gr_all$post_sd)
  ) %>% print()
  
  # IS / OOS 
  is_oos$post$regs %>%
    ungroup() %>%
    summarise(
      is = mean(is),
      post = mean(oos),
      decline = post/is-1
    ) %>% 
    print()
  
  c("pre","post","pre_post") %>% lapply(function(x) {
    is_oos[[x]]$regs %>% mutate(period = x)
  }) %>%
    bind_rows() %>%
    group_by(period) %>%
    summarise(
      is = mean(is > 0),
      oos = mean(oos > 0)
    ) %>% 
    print()
  
  # Bayesian Multiple Testing - Originally significant factors
  bayes_sim <- 1000000
  (fdr_196 <- fdr_fwer_rates(t_cutoff = 1.96, orig_sig = T, a_vec = eb_est$world$factor_mean, a_cov = eb_est$world$factor_cov, n_sim = bayes_sim, seed=settings$seed))
  (fdr_278 <- fdr_fwer_rates(t_cutoff = 2.78, orig_sig = T, a_vec = eb_est$world$factor_mean, a_cov = eb_est$world$factor_cov, n_sim = bayes_sim, seed=settings$seed))
  (true_factors_tbl <- true_factors(t_cutoff = 0, a_vec = eb_est$world$factor_mean, a_cov = eb_est$world$factor_cov, orig_sig = T, n_sim = bayes_sim, seed=settings$seed))
  (rr_unc <- true_factors(t_cutoff = 1.96, a_vec = eb_est$world$factor_mean, a_cov = eb_est$world$factor_cov, orig_sig = T, n_sim = bayes_sim, seed=settings$seed))
  
  # Nano Caps in the US
  country_info[eom == as.Date("2019-12-31") & excntry == "USA", .(n, n_nano, nano_prop = n_nano / n)] %>% print()
  
  # Correlations across size and region
  eb_us_size %>%
    select(characteristic, size_grp, ols_est) %>%
    spread(key = size_grp, value = ols_est) %>%
    summarise(
      cor_mega_micro = cor(Mega, Micro, method = "spearman"),
      cor_mega_nano = cor(Mega, Nano, method = "spearman")
    ) %>%
    print()
  
  eb_est$all$factors %>%
    select(characteristic, region, ols_est) %>%
    spread(key = region, value = ols_est) %>%
    na.omit() %>%
    summarise(
      n = n(),
      cor_us_dev = cor(us, developed, method = "spearman"),
      cor_us_emer = cor(us, emerging, method = "spearman")
    ) %>%
    print()
  
  # TPF Evolution numbers
  tpf_evol$data %>%
    filter(year %in% c(min(year), max(year), 2002, 1991)) %>% # char_info[characteristic %in% c("seas_2_5an", "oaccruals_at")]
    arrange(year) %>%
    mutate(
      new_inclusions = case_when(
        year == 1971 ~ "Market",
        year == 1991 ~ "Accruals",
        year == 2002 ~ "Seasonality",
        year == 2019 ~ "[All factors included]",
      )
    ) %>%
    print()
}