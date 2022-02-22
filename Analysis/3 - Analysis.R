# Empirical Bayes Estimation --------------------------
# search_list: c(regions, type, layers, size_grp)
search_list <- list(
  "us" = list("us", "hml", 2), 
  "developed" = list("developed", "hml", 2),
  "emerging" = list("emerging", "hml", 2),
  "all" = list(c("us", "developed", "emerging"), "hml", 3),
  "world" = list("world", "hml", 2),
  "world_ex_us" = list("world_ex_us", "hml", 2),
  "us_mega" = list("us", "cmp", 2, "mega"),
  "us_large" = list("us", "cmp", 2, "large"),
  "us_small" = list("us", "cmp", 2, "small"),
  "us_micro" = list("us", "cmp", 2, "micro"),
  "us_nano" = list("us", "cmp", 2, "nano")
)
eb_est <- search_list %>% sapply(simplify = F, USE.NAMES = T, function(x) {
  print(paste("Region:", x[[1]]))
  regions <- x[[1]]
  if (x[[2]] == "cmp") {
    base_data <- copy(regional_pfs_cmp) %>% filter(size_grp == x[[4]])
  }
  if (x[[2]] == "hml") {
    base_data <- copy(regional_pfs)
  }
  # Prepare Data
  data <- base_data %>% 
    filter(eom >= settings$start_date & eom <= settings$end_date) %>%
    filter(region %in% regions) %>% 
    eb_prepare(
      scale_alpha = settings$eb$scale_alpha, 
      overlapping = settings$eb$overlapping 
    )
  # Run Empirical Bayes
  op <- data %>% 
    emp_bayes( 
      cluster_labels = cluster_labels, 
      min_obs = settings$eb$min_obs,
      fix_alpha = settings$eb$fix_alpha, 
      bs_cov = settings$eb$bs_cov, 
      layers = x[[3]], 
      shrinkage = settings$eb$shrinkage, 
      cor_type = settings$eb$cor_type, 
      bs_samples = settings$eb$bs_samples, 
      seed = settings$seed
    )
  # Output
  return(op)
})

# Simulations EB vs. BY --------------
# Simulations
if (update_sim) {
  # Values from Data
  pairwise_cor <- eb_est$us$input$long %>%
    select(characteristic, eom, ret_neu) %>%
    spread(key = characteristic, value = ret_neu) %>%
    select(-eom) %>%
    cor(use = "pairwise.complete.obs") 
  
  cor_value <- pairwise_cor %>%
    as_tibble(rownames = "char1") %>%
    gather(-char1, key = "char2", value = "cor") %>%
    left_join(cluster_labels %>% select(characteristic, "hcl1" = hcl_label), by = c("char1"="characteristic")) %>%
    left_join(cluster_labels %>% select(characteristic, "hcl2" = hcl_label), by = c("char2"="characteristic")) %>%
    filter(char1 != char2) %>%
    mutate(same_cluster = (hcl1 == hcl2)) %>%
    group_by(same_cluster) %>%
    summarise(avg_cor = mean(cor))
  
  # Time periods
  med_months <- eb_est$us$input$long %>% group_by(characteristic) %>% summarise(n = n()) %>% pull(n) %>% median()
  
  data <- list(
    yrs = round(med_months / 12),
    cor_within = cor_value %>% filter(same_cluster == T) %>% pull(avg_cor) %>% round(digits = 2),
    cor_across = cor_value %>% filter(same_cluster == F) %>% pull(avg_cor) %>% round(digits = 2)
  )
  # Simulation Settings
  set.seed(settings$seed)
  sim <- list(
    "alpha_0" = 0,
    "t" = 12*70,      # Median amount of data
    "clusters" = 13,
    "fct_pr_cl" = 10,
    "corr_within" = 0.58, 
    "corr_across" = 0.02,
    "n_sims" = 10000,
    "tau_c" = c(0.01, seq(from = 0.05, to = 0.5, by = 0.05)),
    "tau_w" = c(0.01, 0.2)
  )
  sim$se <- (10/sqrt(12))/sqrt(sim$t)
  sim$n <- sim$clusters * sim$fct_pr_cl
  
  # Check settings are consistent with data [Alert if there is a significant difference]
  if (abs(sim$t - data$yrs*12) > 12 | abs(sim$corr_within - data$cor_within) > 0.05 | abs(sim$corr_across - data$cor_across) > 0.05) {
    warning("SIMULATION AND DATA VALUES ARE NOT CONSISTENT!")
    print(data)
    print(list("yrs"=sim$t/12, "corr_within"=sim$corr_within, "corr_across"=sim$corr_across))
  }
  simulation <- sim_mt_control(sim_settings = sim)
  simulation %>% saveRDS(file = paste0(object_path, "/fdr_sim.RDS"))
} else {
  simulation <- readRDS(file = paste0(object_path, "/fdr_sim.RDS"))
}
# False Discovery Rate
model_fdr <- fdr_sim(t_low = 0, a_vec = eb_est$us$factor_mean, a_cov = eb_est$us$factor_cov, n_sim = 10000, seed = settings$seed)
# Multiple Testing Adjustments
mt <- multiple_testing(eb_all = eb_est$all, eb_world = eb_est$world)
# Tangency Portfolios -----------------------------
# Regions
tpf_world <- eb_est$world$input$long %>% tpf_cluster(mkt_region = "world", orig_sig = T, min_date = settings$tpf$start$world, n_boots = settings$tpf$bs_samples, shorting = settings$tpf$shorting, seed = settings$seed)
tpf_us <- eb_est$us$input$long %>% tpf_cluster(mkt_region = "us", orig_sig = T, min_date = settings$tpf$start$us, n_boots = settings$tpf$bs_samples, shorting = settings$tpf$shorting, seed = settings$seed)
tpf_dev <- eb_est$developed$input$long %>% tpf_cluster(mkt_region = "developed", orig_sig = T, min_date = settings$tpf$start$developed, n_boots = settings$tpf$bs_samples, shorting = settings$tpf$shorting, seed = settings$seed)
tpf_emer <- eb_est$emerging$input$long %>% tpf_cluster(mkt_region = "emerging", orig_sig = T, min_date = settings$tpf$start$emerging, n_boots = settings$tpf$bs_samples, shorting = settings$tpf$shorting, seed = settings$seed)
# Size Groups
tpf_size <- c("mega", "large", "small", "micro", "nano") %>% lapply(function(x) {
  eb_est[[paste0("us_", x)]]$input$long %>% tpf_cluster(mkt_region = "us", orig_sig = T, min_date = settings$tpf$start$size_grps, n_boots = settings$tpf$bs_samples, shorting = settings$tpf$shorting, seed = settings$seed) %>%
    mutate(size_grp = x)
}) %>%
  bind_rows()
# Single Factor TPF
tpf_factors <- prepare_tpf_factors(region = settings$tpf_factors$region, orig_sig_values = settings$tpf_factors$orig_sig, 
                                   start = settings$tpf_factors$start, scale = settings$tpf_factors$scale)
opt_s <- tpf_factors$long %>% optimal_shrinkage(k = settings$tpf_factors$k)

# Posterior Over time -----------------
ot_region <- "world"
if (update_post_over_time) {
  for (fix_taus in c(T,F)) {
    if (fix_taus) {
      fixed_priors <- list(
        "alpha" = eb_est[[ot_region]]$mle %>% filter(estimate == "alpha") %>% pull(ml_est), 
        "tau_c" = eb_est[[ot_region]]$mle %>% filter(estimate == "tau_c") %>% pull(ml_est), 
        "tau_s" = eb_est[[ot_region]]$mle %>% filter(estimate == "tau_s") %>% pull(ml_est)
      )
    } else {
      fixed_priors <- NULL
    }
    
    periods <- sort(unique(regional_pfs$eom))
    periods <- periods[month(periods) == 12]  # Only estimate once per year
    
    time_chars <- regional_pfs %>% 
      filter(region == ot_region & eom <= as.Date("1960-12-31")) %>% 
      group_by(characteristic) %>%
      filter(n() >= settings$eb$min_obs) %>%
      pull(characteristic) %>% 
      unique()
    
    posterior_over_time <- periods[periods >= as.Date("1960-12-31")] %>% lapply(function(end_date) { 
      print(end_date)
      # Prepare Data
      data <- regional_pfs %>% 
        filter(characteristic %in% time_chars) %>%
        filter(eom >= settings$start_date & eom <= end_date) %>%
        filter(region == ot_region) %>% 
        eb_prepare(
          scale_alpha = settings$eb$scale_alpha, 
          overlapping = settings$eb$overlapping 
        )
      # Run Empirical Bayes
      eb_act <- data %>% 
        emp_bayes( 
          cluster_labels = cluster_labels, 
          min_obs = settings$eb$min_obs,
          fix_alpha = settings$eb$fix_alpha, 
          bs_cov = settings$eb$bs_cov, 
          layers = 2, 
          shrinkage = settings$eb$shrinkage, 
          cor_type = settings$eb$cor_type, 
          bs_samples = 1000, 
          priors = fixed_priors,
          seed = settings$seed
        )
      eb_act$input <- NULL
      eb_act$end_date <- end_date
      return(eb_act)
    }) 
    if (fix_taus) {
      posterior_over_time %>% saveRDS(file = paste0(object_path, "/posterior_over_time.RDS"))
    } else {
      posterior_over_time %>% saveRDS(file = paste0(object_path, "/posterior_over_time_flex.RDS"))
    }
  }
} 
posterior_over_time <- readRDS(file = paste0(object_path, "/posterior_over_time.RDS"))
posterior_over_time_flex <- readRDS(file = paste0(object_path, "/posterior_over_time_flex.RDS"))

# Size Dimension
eb_us_size <- c("mega", "large", "small", "micro", "nano") %>% lapply(function(x) {
  eb_est[[str_c("us_", x)]]$factors %>% mutate(size_grp = x)
}) %>% 
  bind_rows() %>%
  mutate(
    size_grp = str_to_title(size_grp),
    size_grp = size_grp %>% factor(levels = c("Mega", "Large", "Small", "Micro", "Nano"))
  )

# In-Sample / Out-of-Sample ------------------------------------
is_oos <- c("pre", "post", "pre_post") %>% sapply(simplify = F, USE.NAMES = T, function(t) {
  data <- eb_est$us$input$long %>% prepare_is_oos(min_obs = 60, ret_scaled = "all", orig_group = T, type = t, print=T)
  regs <- data %>%
    group_by(characteristic, period, n_is, n_oos) %>%
    nest() %>%
    mutate(
      fit = data %>% map(~lm(ret_adj ~ mkt_vw_exc, data = .x)),
      tidied = fit %>% map(tidy)
    ) %>% 
    unnest(tidied) %>%
    filter(term == "(Intercept)") %>%
    select(characteristic, period, n_is, n_oos, estimate) %>%
    spread(key = period, value = estimate)
  list(data=data, regs=regs)
})

# Economi Benefit of More Powerful Multiple Comparison 
if (update_post_is) {
  periods <- sort(unique(regional_pfs$eom))
  periods <- periods[month(periods) == 12 & year(periods) >= 1959]  # Only estimate once per year
  
  posterior_is <- periods %>% lapply(function(end_date) { 
    print(paste("Date", end_date, "-" , match(end_date, periods), "out of", length(periods)))
    # Prepare Data
    data <- regional_pfs %>% 
      filter(eom >= settings$start_date & eom <= end_date) %>%
      filter(region == "us") %>% 
      eb_prepare(
        scale_alpha = settings$eb$scale_alpha, 
        overlapping = settings$eb$overlapping 
      )
    # Run Empirical Bayes
    eb_act <- data %>% 
      emp_bayes( 
        cluster_labels = cluster_labels, 
        min_obs = settings$eb$min_obs,
        fix_alpha = settings$eb$fix_alpha, 
        bs_cov = settings$eb$bs_cov, 
        layers = 2, 
        shrinkage = settings$eb$shrinkage, 
        cor_type = settings$eb$cor_type, 
        bs_samples = 1000,
        seed = settings$seed
      )
    # Output 
    eb_act$factors %>% mutate(est_date = end_date)
  }) %>% bind_rows()
  posterior_is %>% saveRDS(file = paste0(object_path, "/posterior_is.RDS"))
} else {
  posterior_is <- readRDS(file = paste0(object_path, "/posterior_is.RDS"))
}
sig_oos_pfs <- posterior_is %>% trading_on_significance()

# Harvey et al (2016) Simulation - Baseline ------------------------------
# We use the baseline specification from table 5 - Panel A where the average correlation is 0 (the average correlation among factors in our data is 7%)
# 1300 * (1-0.396)  The harvey et al numbers are m=1297 and m_true=783
harvey_base <- list(
  alpha_0 = 0,
  t = 70*12,  
  ret = 4.4 / 12,
  vol = 10 / sqrt(12),
  cl = 26,
  cl_true = 16,
  fct_pr_cl = 50,
  corr_across = 0.02,
  corr_within = 0.58,
  tau_ws = c(0.21),         # We estimate it at 0.21
  n_sims = 50,
  fix_alpha = T
)
harvey_base$se <- harvey_base$vol / sqrt(harvey_base$t)
harvey_base$n <- harvey_base$cl * harvey_base$fct_pr_cl
harvey_base$n_true <- harvey_base$cl_true * harvey_base$fct_pr_cl

if (update_harvey_baseline) {
  harvey_base_res <- harvey_et_al_sim(sim_settings = harvey_base, seed = settings$seed)
  harvey_base_res <- list("settings" = harvey_base, "sim" = harvey_base_res)
  harvey_base_res %>% saveRDS(file = paste0(object_path, "/harvey_res_baseline.RDS"))
} else {
  harvey_base_res <- readRDS(file = paste0(object_path, "/harvey_res_baseline.RDS"))
}

# Harvey et al (2016) Simulation - Worst Case
# We use the worst cases specification from table 5 - Panel B where the average correlation is 0 (the average correlation among factors in our data is 7%)
# 2500 * (1-0.683) = 800  The harvey numbers are m=2458 and m_true=779
harvey_worst <- list(
  alpha_0 = 0,
  t = 70*12,                # Median number of years for US factor
  ret = 4.4 / 12,
  vol = 10 / sqrt(12),
  cl = 50,
  cl_true = 16,
  fct_pr_cl = 50,
  corr_across = 0.02,
  corr_within = 0.58,
  tau_ws = c(0.21),         # We estimate it at 0.21 Same as what we estimate
  n_sims = 50,
  fix_alpha = T
)
harvey_worst$se <- harvey_worst$vol / sqrt(harvey_worst$t)
harvey_worst$n <- harvey_worst$cl * harvey_worst$fct_pr_cl
harvey_worst$n_true <- harvey_worst$cl_true * harvey_worst$fct_pr_cl

if (update_harvey_worstcase) {
  harvey_worst_res <- harvey_et_al_sim(sim_settings = harvey_worst, seed = settings$seed)
  harvey_worst_res <- list("settings" = harvey_worst, "sim" = harvey_worst_res)
  harvey_worst_res %>% saveRDS(file = paste0(object_path, "/harvey_res_worstcase.RDS"))
} else {
  harvey_worst_res <- readRDS(file = paste0(object_path, "/harvey_res_worstcase.RDS"))
}

# Estimate parameters on OOS data -----------
if (FALSE) {
  reg <- "us"
  # Prepare Data
  data <- regional_pfs %>% 
    filter(eom >= settings$start_date & eom <= settings$end_date) %>%
    filter(region==reg) %>%
    left_join(char_info %>% select(characteristic, sample_start, sample_end), by = "characteristic") %>%
    filter(year(eom) < sample_start | year(eom) > sample_end) %>%
    eb_prepare(
      scale_alpha = settings$eb$scale_alpha, 
      overlapping = settings$eb$overlapping # If we start in 1955-02-28 we lose 12 factors relative to starting in 1972-11-30
    )
  # Run Empirical Bayes
  op <- data %>% 
    emp_bayes( 
      cluster_labels = cluster_labels, 
      min_obs = settings$eb$min_obs,
      fix_alpha = settings$eb$fix_alpha, 
      bs_cov = settings$eb$bs_cov, 
      layers = 2, 
      shrinkage = settings$eb$shrinkage, 
      cor_type = settings$eb$cor_type, 
      bs_samples = settings$eb$bs_samples,
      seed = settings$seed
    )
  # OOS-replication rate
  op$factors %>%
    left_join(char_info %>% select(characteristic, significance), by = "characteristic") %>%
    filter(significance == 1) %>%
    summarise(
      n = n(),
      sd_ols = sd(ols_est),
      eb_rr = mean(p025 > 0),
      eb_ols = mean(ols_est - 1.96*ols_se > 0)
    )
  # OOS Hyperparameters
  op$mle %>% mutate(estimate = if_else(estimate == "tau_s", "tau_w", estimate))
  # Full sample Tau's
  eb_est[[reg]]$mle %>% mutate(estimate = if_else(estimate == "tau_s", "tau_w", estimate))
  # Replication rate with OOS hyperparameters
  repl_rate <- function(chars, alphas, sigma, alpha0, tau_c, tau_w, cluster_labels, char_info) {
    # Alpha zero vector
    alpha0_vec <- rep(alpha0, length(alphas))
    # Signal Membership
    cm <- tibble(characteristic = chars, "alpha"= alphas) %>% 
      left_join(cluster_labels, by = "characteristic") 
    m <- cm %>%
      mutate(cm = 1) %>%
      select(characteristic, hcl_label, cm) %>%
      spread(key = hcl_label, value = cm) %>% 
      select(-characteristic) %>% 
      as.matrix()
    m[is.na(m)] <- 0
    mm <- m %*% t(m)
    # Omega 
    omega <- diag(length(alphas)) * tau_w^2 + mm * tau_c^2
    # Posterior
    post_cov <- solve(solve(omega) + solve(sigma))
    post_alpha <- post_cov %*% (solve(omega) %*% alpha0_vec + solve(sigma) %*% alphas)
    # Replication Rate
    tibble(
      characteristic = chars,
      alpha = drop(post_alpha),
      se = sqrt(diag(post_cov))
    ) %>%
      left_join(char_info, by = "characteristic") %>%
      filter(significance == T) %>%
      summarise(
        repl_rate = mean(alpha - 1.96*se > 0)
      )
  }
  # Full Sample Hyper-parameters
  repl_rate(
    chars = eb_est[[reg]]$factors$characteristic, 
    alphas = eb_est[[reg]]$factors$ols_est, 
    sigma = eb_est[[reg]]$sigma,
    alpha0 = eb_est[[reg]]$mle %>% filter(estimate == "alpha") %>% pull(ml_est),
    tau_c = eb_est[[reg]]$mle %>% filter(estimate == "tau_c") %>% pull(ml_est),
    tau_w = eb_est[[reg]]$mle %>% filter(estimate == "tau_s") %>% pull(ml_est),
    cluster_labels = cluster_labels, 
    char_info = char_info
  )
  # OOS Hyper-parameters
  repl_rate(
    chars = eb_est[[reg]]$factors$characteristic, 
    alphas = eb_est[[reg]]$factors$ols_est, 
    sigma = eb_est[[reg]]$sigma,
    alpha0 = op$mle%>% filter(estimate == "alpha") %>% pull(ml_est),
    tau_c = op$mle %>% filter(estimate == "tau_c") %>% pull(ml_est),
    tau_w = op$mle %>% filter(estimate == "tau_s") %>% pull(ml_est),
    cluster_labels = cluster_labels, 
    char_info = char_info
  )
}
