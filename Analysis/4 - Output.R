# Determine Cluster Order
cluster_order <- c("Accruals", "Debt Issuance", "Investment", "Short-Term Reversal", "Value",
                   "Low Risk", "Quality", "Momentum", "Profitability", "Profit Growth",
                   "Seasonality", "Size", "Low Leverage")

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
    text(x = x, y = 151, label_func("Short-Term Reversal"), cex = c, col = colours_theme[5], adj = 0)   #gold-9
    text(x = x, y = 141, label_func("Profitability"), cex = c, col = colours_theme[4], adj = 0)  #lightgreen-7
    text(x = x, y = 129, label_func("Low Risk"), cex = c, col = colours_theme[3], adj = 0)  #purple-5
    text(x = x, y = 112, label_func("Value"), cex = c, col = colours_theme[2], adj = 0)       #orange-4
    text(x = x, y = 90, label_func("Investment"), cex = c, col = colours_theme[1], adj = 0)           #darkgreen-3
    text(x = x, y = 73, label_func("Seasonality"), cex = c, col = colours_theme[11], adj = 0)            #orange-4 
    text(x = x, y = 63, label_func("Debt Issuance"), cex = c, col = colours_theme[9], adj = 0)       #red-2
    text(x = x, y = 57, label_func("Size"), cex = c, col = colours_theme[7], adj = 0)            #blue-1  
    text(x = x, y = 51, label_func("Accruals"), cex = c, col = colours_theme[5], adj = 0)   #black-11
    text(x = x, y = 43, label_func("Low Leverage"), cex = c, col = colours_theme[4], adj = 0)            #lightgreen-7
    text(x = x, y = 30, label_func("Profit Growth"), cex = c, col = colours_theme[3], adj = 0)   #purple-5
    text(x = x, y = 22, label_func("Momentum"), cex = c, col = colours_theme[2], adj = 0)               #darkgreen-3
    text(x = x, y = 12, label_func("Quality"), cex = c, col = colours_theme[1], adj = 0)        #blue-1
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

# Tangency Portfolio - US
output$figures$tpf <- tpf_us %>% plot_tpf(cluster_order = cluster_order, ci_low = 0.05, ci_high = 0.95)

# Tangency Portfolio - Regions
output$figures$tpf_regions <- plot_tpf_region(tpf_us = tpf_us, tpf_dev = tpf_dev, tpf_emer = tpf_emer, cluster_order = cluster_order, ci_low = 0.05, ci_high = 0.95)

# Tangency Portfolio - Size Groups
output$figures$tpf_size <- tpf_size %>% plot_tpf_size(cluster_order = cluster_order, ci_low = 0.05, ci_high = 0.95)

# Posterior over Time - Fixed Taus
output$figures$overtime <- posterior_over_time %>% plot_over_time(orig_sig = T, ols_incl = T, lb = 5, bw=F)
output$figures$overtime_bw <- posterior_over_time %>% plot_over_time(orig_sig = T, ols_incl = T, lb = 5, bw=T)

# Posterior over Time - Flexible Taus
output$figures$overtime_flex <- posterior_over_time_flex %>% plot_over_time(orig_sig = T, ols_incl = F, lb=5, bw=F)
output$figures$overtime_flex_bw <- posterior_over_time_flex %>% plot_over_time(orig_sig = T, ols_incl = F, lb=5, bw=T)

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


# Cumulative returns OOS - Marginally significant factors
output$figures$marg_sig_oos <- sig_oos_pfs %>% plot_sig_oos(sig_type = "marg_sig", cutoff_2012 = T, first_date = as.Date("1990-01-01"), leg_pos = c(.85, .55)) # Also include table for caption

# Cummulative returns OOS - EB significant factors
output$figures$eb_sig_oos <- sig_oos_pfs %>% plot_sig_oos(sig_type = "eb_sig", cutoff_2012 = F, first_date = as.Date("1990-01-01"), leg_pos = c(.85, .40)) # Also include table for caption


# Save Figures as Pictures -------------------------
if (save_figures) {
  # Base settings
  output_fig <- function(path, name, format, width, height) {
    file <- paste0(path, "/", name, ".eps")
    ggsave(file=file, width = width, height = height, units = "in", dpi = 300)
  }
  
  h <- 5
  w <- 9
  # For main text -----------------------------------------
  # Figure 1 ------
  output$figures$lit_comp
  output_fig(path=output_path, name = "fig1", width = w + 1, height = h)
  dev.off()
  
  output$figures$lit_comp + scale_fill_manual(values = c("black", "grey35"))
  output_fig(path=output_path, name = "fig1_bw", width = w + 1, height = h)
  dev.off()
  
  # Figure 2 -----
  output$figures$marg_sig_oos
  output_fig(path=output_path, name = "fig2", width = w, height = h*2/3)
  dev.off()
  
  output$figures$marg_sig_oos + scale_colour_manual(values = c("black", "black"))
  output_fig(path=output_path, name = "fig2_bw", width = w, height = h*2/3)
  dev.off()
  
  # Figure 3 -----
  output$figures$sim_fdr
  output_fig(path=output_path, name = "fig3", format = format, width = w, height = h)
  dev.off()
  
  output$figures$sim_fdr + scale_colour_manual(values = c("black", "black", "black"))
  output_fig(path=output_path, name = "fig3_bw", format = format, width = w, height = h)
  dev.off()
  
  
  # Figure 4 -----
  output$figures$mt_factors + theme(text = element_text(size = 13), axis.text.x = element_blank())
  output_fig(path=output_path, name = "fig4", format = format, width = w, height = h)
  dev.off()
  
  output$figures$mt_factors + theme(text = element_text(size = 13), axis.text.x = element_blank()) + 
    scale_colour_manual(values = rep("black", 3))
  output_fig(path=output_path, name = "fig4_bw", format = format, width = w, height = h)
  dev.off()
  
  # Figure 5a -----
  output$figures$size_overall + theme(
    axis.title.x = element_text(size = 17),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17))
  output_fig(path=output_path, name = "fig5a", format = format, width = w, height = h)
  dev.off()
  
  output$figures$size_overall + theme(
    axis.title.x = element_text(size = 17),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)) + geom_col(fill="grey35")
  output_fig(path=output_path, name = "fig5a_bw", format = format, width = w, height = h)
  dev.off()
  
  # Figure 5b -----
  output$figures$repl_cluster_us + theme(
    axis.text.y = element_text(size = 13), 
    axis.text.x = element_text(size = 14), 
    text = element_text(size = 14))
  output_fig(path=output_path, name = "fig5b", format = format, width = w, height = h)
  dev.off()
  
  output$figures$repl_cluster_us + theme(
    axis.text.y = element_text(size = 13), 
    axis.text.x = element_text(size = 14), 
    text = element_text(size = 14)) + scale_fill_manual(values = rep("grey35", 13))
  output_fig(path=output_path, name = "fig5b_bw", format = format, width = w, height = h)
  dev.off()
  
  # Figure 6 -----
  output$figures$mt_summary + 
    theme(
      text = element_text(size = 13),
      axis.title.x = element_blank(), 
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9)
    )
  output_fig(path=output_path, name = "fig6", format = format, width = w, height = h)
  dev.off()
  
  output$figures$mt_summary + 
    theme(
      text = element_text(size = 13),
      axis.title.x = element_blank(), 
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9)
    ) +
    scale_fill_grey()
  output_fig(path=output_path, name = "fig6_bw", format = format, width = w, height = h)
  dev.off()
  
  # Figure 7 -----
  output$figures$world_vs_us + theme(text = element_text(size = 13)) 
  output_fig(path=output_path, name = "fig7", format = format, width = h, height = h)
  dev.off()
  
  output$figures$world_vs_us + theme(text = element_text(size = 13)) + scale_colour_manual(values = c("black", "grey35"))
  output_fig(path=output_path, name = "fig7_bw", format = format, width = h, height = h)
  dev.off()
  
  # Figure 8a -----
  output$figures$is_pre + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  ) 
  output_fig(path=output_path, name = "fig8a", format = format, width = w/3, height = w/3)
  dev.off()
  
  output$figures$is_pre + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  )  + geom_point(colour = "black")
  output_fig(path=output_path, name = "fig8a_bw", format = format, width = w/3, height = w/3)
  dev.off()
  
  # Figure 8b -----
  output$figures$is_post  + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  ) 
  output_fig(path=output_path, name = "fig8b", format = format, width = w/3, height = w/3)
  dev.off()
  
  output$figures$is_post  + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  ) + geom_point(colour = "black")
  output_fig(path=output_path, name = "fig8b_bw", format = format, width = w/3, height = w/3)
  dev.off()
  
  # Figure 8c -----
  output$figures$is_oos  + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  ) 
  output_fig(path=output_path, name = "fig8c", format = format, width = w/3, height = w/3)
  dev.off()
  
  output$figures$is_oos  + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  )  + geom_point(colour = "black")
  output_fig(path=output_path, name = "fig8c_bw", format = format, width = w/3, height = w/3)
  dev.off()
  
  # Figure 9 -----
  output$figures$overtime
  output_fig(path=output_path, name = "fig9", format = format, width = w, height = h)
  dev.off()
  
  output$figures$overtime_bw
  output_fig(path=output_path, name = "fig9_bw", format = format, width = w, height = h)
  dev.off()
  
  
  # Figure 10 -----
  output$figures$sim_harvey + theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.text = element_text(size = 11)
  )
  output_fig(path=output_path, name = "fig10", format = format, width = w, height = h)
  dev.off()
  
  output$figures$sim_harvey + theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.text = element_text(size = 11)
  ) + scale_colour_manual(values = rep("black", 3))
  output_fig(path=output_path, name = "fig10_bw", format = format, width = w, height = h)
  dev.off()
  
  # Figure 11 -----
  output$figures$gl_by_factor + theme(text = element_text(size = 13), legend.position = "right", axis.text.x = element_text(size = 5)) 
  output_fig(path=output_path, name = "fig11", format = format, width = w, height = h)
  dev.off()
  
  output$figures$gl_by_factor + theme(text = element_text(size = 13), legend.position = "right", axis.text.x = element_text(size = 5)) +
    scale_colour_manual(values = rep("black", 13))
  output_fig(path=output_path, name = "fig11_bw", format = format, width = w, height = h)
  dev.off()
  
  
  # Figure 12 -----
  output$figures$effect_size + theme(
    axis.text.y = element_text(size = 13), 
    strip.text.x = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13)
  )
  output_fig(path=output_path, name = "fig12a", format = format, width = w, height = h)
  dev.off()
  
  output$figures$effect_size + theme(
    axis.text.y = element_text(size = 13), 
    strip.text.x = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13)
  ) + scale_fill_manual(values = rep("grey35", 13))
  output_fig(path=output_path, name = "fig12a_bw", format = format, width = w, height = h)
  dev.off()
  
  # Figure 12b -----
  output$figures$effect_regions + theme(
    axis.text.y = element_text(size = 13), 
    strip.text.x = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13)
  )
  output_fig(path=output_path, name = "fig12b", format = format, width = w, height = h)
  dev.off()
  
  output$figures$effect_regions + theme(
    axis.text.y = element_text(size = 13), 
    strip.text.x = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13)
  ) + scale_fill_manual(values = rep("grey35", 13))
  output_fig(path=output_path, name = "fig12b_bw", format = format, width = w, height = h)
  dev.off()
  
  # Figure 13 -----
  output$figures$tpf + theme(text = element_text(size = 13), axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13))
  output_fig(path=output_path, name = "fig13", format = format, width = w, height = h)
  dev.off()
  
  output$figures$tpf + theme(text = element_text(size = 13), axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13)) +
    scale_fill_manual(values = rep("grey35", 14))
  output_fig(path=output_path, name = "fig13_bw", format = format, width = w, height = h)
  dev.off()
  
  # Figure 14 -----
  # Evolution of TPF
  output$figures$tpf_evolution
  output_fig(path=output_path, name = "fig14", format = format, width = w, height = h*0.8)
  dev.off()
  
  output$figures$tpf_evolution
  output_fig(path=output_path, name = "fig14_bw", format = format, width = w, height = h*0.8)
  dev.off()
  
  
  # Figure IA.1 ----- # NEED TO DO!!
  if (FALSE) {
    # Need to run everything with settings$weighting$us = "vw" and settings$weighting$global_ex_us="vw"
    output$figures$lit_comp
    output_fig(path=output_path, name = "figIA1", format = format, width = w + 1, height = h)
    dev.off()
  }
  
  
  # Figure IA.2 ------
  output$figures$eb_sig_oos
  output_fig(path=output_path, name = "figIA2", format = format, width = w, height = h*2/3)
  dev.off()
  
  # Figure IA.3a -----
  output$figures$is_pre_quad + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  ) 
  output_fig(path=output_path, name = "figIA3a", format = format, width = w/3, height = w/3)
  dev.off()
  
  # Figure IA.3b -----
  output$figures$is_post_quad  + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  )  
  output_fig(path=output_path, name = "figIA3b", format = format, width = w/3, height = w/3)
  dev.off()
  
  # Figure IA.3c -----
  output$figures$is_oos_quad  + theme(
    text = element_text(size = 12), 
    plot.title = element_text(size = 10, vjust = -2), 
    plot.subtitle = element_text(size = 8, vjust = 0),
    plot.margin = unit(c(0,0,0,0), "cm")
  ) 
  output_fig(path=output_path, name = "figIA3c", format = format, width = w/3, height = w/3)
  dev.off()
  
  
  # Figure IA.4 -----
  output$figures$overtime_flex + theme(text = element_text(size = 13))
  output_fig(path=output_path, name = "figIA4", format = format, width = w, height = h)
  dev.off()
  
  # Figure IA.5 -----
  output$figures$overtime_flex_taus + theme(text = element_text(size = 13), legend.text = element_text(size = 12))
  output_fig(path=output_path, name = "figIA5", format = format, width = w, height = h)
  dev.off()
  
  # Figure IA.6 ----
  output$figures$sim_harvey_robustness + theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.text = element_text(size = 11)
  )
  output_fig(path=output_path, name = "figIA6", format = format, width = w, height = h)
  dev.off()
  
  # Figure IA.7 ----
  output$figures$gl_by_cluster  + theme(text = element_text(size = 13))
  output_fig(path=output_path, name = "figIA7", format = format, width = w, height = h)
  dev.off()
  
  # Figure IA.8 ----
  output$figures$size_clusters
  output_fig(path=output_path, name = "figIA8", format = format, width = w, height = h+1)
  dev.off()
  
  # Figure IA.9 ----
  output$figures$tpf_regions  + theme(text = element_text(size = 13), axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13))
  output_fig(path=output_path, name = "figIA9", format = format, width = w, height = h)
  dev.off()
  
  # Figure IA.10 ----
  output$figures$tpf_size  + theme(
    text = element_text(size = 13), 
    axis.text.x = element_text(size = 13), 
    axis.text.y = element_text(size = 11))
  
  output_fig(path=output_path, name = "figIA10", format = format, width = w, height = h*1.5)
  dev.off()
  
  # Figure IA.11 ----
  output$figures$tpf_factors_one_cluster
  output_fig(path=output_path, name = "figIA11", format = format, width = w, height = h)
  dev.off()
  
  # Figure IA.12 ----
  output$figures$tpf_factors_excl_one
  output_fig(path=output_path, name = "figIA12", format = format, width = w, height = h)
  dev.off()
  
  # Figure IA.13 ----
  output$figures$tpf_factors_imp_cluster + theme(
    axis.text.y = element_text(size = 6)
  )
  output_fig(path=output_path, name = "figIA13", format = format, width = w, height = h*1.5)
  dev.off()
  
  # Figure IA.14 ----
  output$figures$tpf_factors_imp + theme(
    axis.text.y = element_text(size = 6)
  )
  output_fig(path=output_path, name = "figIA14", format = format, width = w, height = h*1.5)
  dev.off()
  
  # Figure IA.15 (special, because not ggplot) ----
  pdf(str_c(output_path, "/figIA15.pdf"), width = w, height = h*2+1.5)
  output$figures$hcl_us()
  dev.off()
  
  # Figure IA.16 (special, because not ggplot) ----
  pdf(str_c(output_path, "/figIA16.pdf"), width = 6, height = 6)
  output$figures$hcl_us_val()
  dev.off()
  
  
  
  
  # Other figures not included in paper --------
  output$figures$ci_many_fcts + theme(axis.text.y = element_text(size = 13), text = element_text(size = 13))
  output$figures$model_fdr + theme(text = element_text(size = 13))

  # TABLES -----------------
  # Estimated Taus
  table_taus()
  # Economic Benefit of more Power
  sig_oos_pfs %>% table_economic_benefit()
  # Factor Performance
  table_factor_info()
  
  # Numbers mentioned in paper --------
  # Bayesian Multiple Testing
  bayes_sim <- 1000000
  (fdr_196 <- fdr_fwer_rates(t_cutoff = 1.96, orig_sig = T, a_vec = eb_est$world$factor_mean, a_cov = eb_est$world$factor_cov, n_sim = bayes_sim, seed=settings$seed))
  (fdr_278 <- fdr_fwer_rates(t_cutoff = 2.78, orig_sig = T, a_vec = eb_est$world$factor_mean, a_cov = eb_est$world$factor_cov, n_sim = bayes_sim, seed=settings$seed))
  (true_factors_tbl <- true_factors(t_cutoff = 0, a_vec = eb_est$world$factor_mean, a_cov = eb_est$world$factor_cov, orig_sig = T, n_sim = bayes_sim, seed=settings$seed))
  (rr_unc <- true_factors(t_cutoff = 1.96, a_vec = eb_est$world$factor_mean, a_cov = eb_est$world$factor_cov, orig_sig = T, n_sim = bayes_sim, seed=settings$seed))
  
  # Mentioned in introduction
  paste0("Replication rate SE: ", round(rr_unc$sd*100, 2), "%")
  paste0("Bayesian FDR: ", round(fdr_196$fdr_dist$mean*100, 2), "%, with 95% CI of [", round(fdr_196$fdr_dist$p025*100, 2), "%, ", round(fdr_196$fdr_dist$p975*100, 2), "%], SE: ", round(fdr_196$fdr_dist$sd*100, 2))
  paste0("Bayesian FWER: ", round(fdr_196$fwer_dist$mean*100, 2), "%, with SE of ", round(fdr_196$fwer_dist$sd*100, 2), "%")
  paste0("Expected fraction of true factors: ", round(true_factors_tbl$mean*100, 2), "%, with SE of ", round(true_factors_tbl$sd*100, 2), "%")
  
  
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
  
  # Change in Book equity factor
  be_gr_us <- eb_est$us$factors %>% 
    filter(region == "us" & characteristic == "be_gr1a")
  be_gr_all <- eb_est$all$factors %>% 
    filter(region == "us" & characteristic == "be_gr1a")
  tibble(
    characteristic = rep("be_gr1a",2), 
    region = rep("US", 2), 
    data = c("US", "Global"), 
    post_mean = c(be_gr_us$post_mean, be_gr_all$post_mean), 
    post_vol = c(be_gr_us$post_sd, be_gr_all$post_sd),
    t = post_mean / post_vol
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
      n = n(),
      is = mean(is > 0),
      oos = mean(oos > 0)
    ) %>% 
    print()
  
  # Posterior over time width
  posterior_over_time %>% plot_over_time(orig_sig = T, ols_incl = T, lb = 5)
  
  # Bayesian Multiple Testing
  fdr_196$fdr_dist
  fdr_278$fwer_fdr # FWER at t>2.78
  true_factors_tbl
  
  # Replication rates in different size groups
  eb_us_size %>% plot_size_overall(flipped = T, text = T)
  
  # Publication Bias
  plot_harvey(harvey_base_res = harvey_base_res, harvey_worst_res = harvey_worst_res, tau_ws = 0.21, act_rr = headline_rr)
  
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
    arrange(year) %>%
    mutate(tpf_sr_l1 = dplyr::lag(tpf_sr)) %>%
    filter(year %in% c(min(year), max(year), 2002, 1991)) %>% # char_info[characteristic %in% c("seas_2_5an", "oaccruals_at")]
    arrange(year) %>%
    mutate(
      new_inclusions = case_when(
        year == 1971 ~ "Market",
        year == 1991 ~ "Accruals",
        year == 2002 ~ "Seasonality",
        year == year(settings$end_date) ~ "[All factors included]",
      )
    ) %>%
    print()
  
  # Average pairwise correlations
  eb_est$us$input$long %>%
    select(characteristic, eom, ret_neu) %>%
    spread(key = characteristic, value = ret_neu) %>%
    select(-eom) %>%
    cor(use = "pairwise.complete.obs") %>%
    as_tibble(rownames = "char1") %>%
    gather(-char1, key = "char2", value = "cor") %>% 
    filter(char1 != char2) %>% 
    summarise(average_cor = mean(cor))
  
}