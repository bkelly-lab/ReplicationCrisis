eb_prepare <- function(data, scale_alphas, overlapping) {
  if (overlapping) {
    data <- data %>%
      group_by(region, characteristic) %>%
      mutate(obs = n()) %>%
      ungroup() %>%
      filter(obs == max(obs)) %>%
      select(-obs)
  }
  # Adjust for Beta
  data <- data %>% 
    group_by(region, characteristic) %>%
    mutate(
      beta = cov(ret, mkt_vw_exc)/var(mkt_vw_exc),
      ret_neu = (ret - mkt_vw_exc * beta)*100,
      scaling_fct = sqrt(10^2/12) / sd(ret_neu),
      ret_neu_scaled = ret_neu * scaling_fct
    ) %>%
    ungroup()
  # Make Wide
  data <- data %>% mutate(name_wide = str_c(characteristic, "__", region))
  if(scale_alphas) {
    data_wide <- data %>%
      select(name_wide, eom, ret_neu_scaled) %>%
      spread(key = name_wide, value = ret_neu_scaled)
  } else {
    data_wide <- data %>%
      select(name_wide, eom, ret_neu) %>%
      spread(key = name_wide, value = ret_neu)
  }
  # Return
  list(
    "long" = data,
    "wide" = data_wide
  )
}

block_cluster_func <- function(cor_mat, cl_lables) {
  cor_long <- cor_mat %>%
    as_tibble(rownames = "char1") %>%
    gather(-char1, key = "char2", value = "cor") %>%
    separate(col = "char1", into = c("char1", "region1"), sep = "__") %>%
    separate(col = "char2", into = c("char2", "region2"), sep = "__") %>%
    left_join(cl_lables %>% select(characteristic, "hcl1"  = hcl_label), by = c("char1"="characteristic")) %>%
    left_join(cl_lables %>% select(characteristic, "hcl2"  = hcl_label), by = c("char2"="characteristic")) %>%
    rowwise() %>%
    mutate(
      hclreg1 = str_c(hcl1, "__", region1),
      hclreg2 = str_c(hcl2, "__", region2)
    ) %>%
    select(-hcl1, -hcl2) %>%
    mutate(hcl_pair = str_c(min(c(hclreg1, hclreg2)), "_x_", max(c(hclreg1, hclreg2)))) %>%
    unite(col = "name1", char1, region1, sep = "__", remove = T) %>%
    unite(col = "name2", char2, region2, sep = "__", remove = T) %>%
    ungroup()
  
  cluster_wise_cor <- cor_long %>%
    filter(name1 != name2) %>%  # Exclude cor(factor_i, factor_i)=1 
    group_by(hcl_pair) %>%
    summarise(
      cor_avg = mean(cor)
    ) 
  
  cluster_block_cor <- cor_long %>%
    left_join(cluster_wise_cor, by = "hcl_pair") %>% 
    mutate(cor_avg = if_else(name1 == name2, 1, cor_avg)) %>%  # ONLY IF SAME REGION!!
    select(name1, name2, cor_avg) %>%
    spread(key = name2, value = cor_avg)
  
  cbc_rows <- cluster_block_cor$name1  
  cluster_block_cor <- cluster_block_cor %>% select(-name1) %>% as.matrix()
  rownames(cluster_block_cor) <- cbc_rows
  return(cluster_block_cor)
}

# Empirical Bayes ----------------
emp_bayes <- function(data, cluster_labels, min_obs = 5 * 12, fix_alpha = F, bs_cov = F, cor_type = "sample", shrinkage = 0, layers = 3, bs_samples = 10000, seed, priors = NULL, sigma = NULL, plot = T) { # cor_type %in% c("raw", "block_2", "block_clusters")
  set.seed(seed)
  y_raw <- data$wide %>% select(-eom) %>% as.matrix()
  obs <- y_raw %>% apply(2, function(x) sum(!is.na(x)))
  y <- y_raw[, obs >= min_obs]
  n_fcts <- ncol(y)
  y_mean <- y %>% apply(2, mean, na.rm = T)  
  
  if (is.null(sigma)) {
    if (bs_cov) {
      bs_full <- y %>%
        rsample::bootstraps(times = bs_samples) %>%
        mutate(
          res = splits %>% map(~.x %>% rsample::analysis() %>% apply(2, mean, na.rm = T) %>% as_tibble(rownames = "characteristic"))
        ) %>%
        select(-splits) %>%
        unnest(res)
      
      bs_full_cov <- bs_full %>%
        spread(key = characteristic, value = value) %>%
        select(-id) %>%
        cov()
      
      alpha_sd <- sqrt(diag(bs_full_cov)) 
      alpha_cor <- solve(diag(alpha_sd)) %*% bs_full_cov %*% solve(diag(alpha_sd))
      colnames(alpha_cor) <- names(alpha_sd)
      rownames(alpha_cor) <- names(alpha_sd)
      
    } else {
      y_sd <- y %>% apply(2, sd, na.rm=T) 
      y_scor <- y %>% cor(use = "complete.obs")  
      alpha_sd <- y_sd / sqrt(nrow(y))
      alpha_cor <- y_scor
    }
    
    # Apply Shrinkage
    alpha_cor_shrunk <- alpha_cor * (1-shrinkage) + diag(n_fcts) * shrinkage
    
    # Correlation Block Adjustment
    if (cor_type == "sample") {
      alpha_cor_adj <- alpha_cor_shrunk
    }
    if (cor_type == "block_clusters") {
      alpha_cor_adj <- alpha_cor_shrunk %>% block_cluster_func(cl_lables = cluster_labels)
    }
    
    sigma <- diag(alpha_sd) %*% alpha_cor_adj %*% diag(alpha_sd)  # This is really the equivalent of sigma/T from the paper
    colnames(sigma) <- colnames(alpha_cor_shrunk)
    
    print(str_c("Condition Number: Raw = ", round(kappa(alpha_cor_shrunk), 2), ", Adjusted = ", round(kappa(alpha_cor_adj), 2)))
  } else {
    alpha_sd <- sqrt(diag(sigma))
    names(alpha_sd) <- colnames(sigma)
  }
  
  # Cluster Membership
  cm <- y_mean %>% 
    as_tibble(rownames = "char_reg") %>%
    mutate(
      characteristic = str_split(char_reg, "__", simplify = T)[, 1]
    ) %>%
    left_join(cluster_labels, by = "characteristic") 
  
  m <- cm %>%
    mutate(cm = 1) %>%
    select(char_reg, hcl_label, cm) %>%
    spread(key = hcl_label, value = cm) %>% 
    select(-char_reg) %>% 
    as.matrix()
  m[is.na(m)] <- 0
  mm <- m %*% t(m)
  
  n_cl <- ncol(m)
  
  # Signal Membership
  z <- cm %>%
    mutate(sm = 1) %>%
    select(char_reg, characteristic, sm) %>%
    spread(key = characteristic, value = sm) %>%
    select(-char_reg) %>%
    as.matrix()
  
  z[is.na(z)] <- 0
  zz <- z %*% t(z)
  
  n_s <- ncol(z)
  
  # Starting Values
  starting_values <- cm %>%
    group_by(hcl_label, characteristic) %>%
    summarise(
      n_s = n(),
      signal_mean = mean(value),
      signal_sd = sd(value)
    ) %>%
    group_by(hcl_label) %>%
    summarise(
      n_c = sum(n_s),
      cl_mean = mean(signal_mean),
      cl_sd = sd(signal_mean),
      cl_signal_within = mean(signal_sd)
    ) %>%
    ungroup() %>%
    mutate(cl_sd = if_else(n_c == 1, 0, cl_sd)) %>%
    summarise(
      alpha_mean = mean(cl_mean),  
      sd_cl_mean = if_else(condition = fix_alpha, sqrt(sum((cl_mean^2) / (n() - 1))), sd(cl_mean)),
      sd_within_cl = mean(cl_sd),
      sd_within_signal = mean(cl_signal_within)
    )
  
  if (fix_alpha) {
    sd_all <- sqrt(sum(y_mean^2) / (length(y_mean) - 1))
  } else {
    sd_all <- sd(y_mean)
  }
  
  # Maximum Likelihood
  omega_func <- function(layers, tau_c, tau_s, tau_w) {
    if (layers == 1) {
      a_omega <- diag(n_fcts) * tau_c^2                                        # All alphas are drawn from same distribution
    }
    if (layers == 2) {
      a_omega <- diag(n_fcts) * tau_s^2 + mm * tau_c^2                       # All cluster alphas are drawn from the same distribution, could be done with signals as well  
    }
    if (layers == 3) {
      a_omega <- diag(n_fcts) * tau_w^2 + zz * tau_s^2 + mm * tau_c^2      # Cluster distrib., signal distrib. factor distrib.
    }
    return(a_omega)
  }
  
  # Choose between specifying prior parameters or finding them via EB
  if (is.null(priors)) {
    if (layers == 1) {
      start_list <- list(
        a = starting_values$alpha_mean, 
        tc = sd_all)
      
      mle_func <- function(a, tc) {
        a_vec <- rep(a, n_fcts)
        a_omega <- omega_func(layers = layers, tau_c = tc, tau_s = NULL, tau_w = NULL)
        
        a_cov <- sigma + a_omega  #  / t_mat
        
        -(mvtnorm::dmvnorm(x = y_mean, mean = a_vec, sigma = a_cov, log = T))  #  + dgamma(param[2], 2, 5, log = T)*sum(mm)/2  + dgamma(param[3], 2, 10, log = T)*140 
      }
    } 
    if (layers == 2) {
      start_list <- list(
        a = starting_values$alpha_mean, 
        tc = starting_values$sd_cl_mean, 
        ts = starting_values$sd_within_cl)
      
      mle_func <- function(a, tc, ts) {
        a_vec <- rep(a, n_fcts)
        a_omega <- omega_func(layers = layers, tau_c = tc, tau_s = ts, tau_w = NULL)
        
        a_cov <- sigma + a_omega  #  / t_mat
        
        -(mvtnorm::dmvnorm(x = y_mean, mean = a_vec, sigma = a_cov, log = T))  
      }
    }
    if (layers == 3) {
      start_list <- list(
        a = starting_values$alpha_mean, 
        tc = starting_values$sd_cl_mean, 
        ts = starting_values$sd_within_cl,
        tw = starting_values$sd_within_signal)
      
      mle_func <- function(a, tc, ts, tw) {
        a_vec <- rep(a, n_fcts)
        a_omega <- omega_func(layers = layers, tau_c = tc, tau_s = ts, tau_w = tw)
        
        a_cov <- sigma + a_omega  
        
        -(mvtnorm::dmvnorm(x = y_mean, mean = a_vec, sigma = a_cov, log = T))
      }
    }
    
    # Maximum likelihood estimation
    for (k in 1:10) {
      initial_params <- start_list %>% lapply(function(x) max(x+rnorm(1, mean = 0, sd = 0.01), 0)) # Max is just to ensure that variances are not negative, never in use
      if (fix_alpha) {
        (hyper_pars <- stats4::mle(minuslogl = mle_func, start = initial_params, lower = c(-Inf, 0, 0, 0)[1:length(start_list)], fixed = list(a = 0)))
      } else {
        (hyper_pars <- stats4::mle(minuslogl = mle_func, start = initial_params, lower = c(-Inf, 0, 0, 0)[1:length(start_list)]))
      }
      if (hyper_pars@details$convergence==0) break
    }
    # Check convergence
    if (hyper_pars@details$convergence != 0) {
      warning("MLE step did not converge!!!")
      return(NULL)
    } 
    
    mu <- hyper_pars@fullcoef["a"]
    tau_c <- hyper_pars@fullcoef["tc"]
    tau_s <- hyper_pars@fullcoef["ts"]
    tau_w <- hyper_pars@fullcoef["tw"]
  } else {
    mu <- priors$alpha
    tau_c <- priors$tau_c
    tau_s <- priors$tau_s
    tau_w <- priors$tau_w
  }
  
  theta <- omega_func(layers = layers, tau_c = tau_c, tau_s = tau_s, tau_w = tau_w)
  colnames(theta) <- rownames(theta) <- names(y_mean)
  print(paste("Condition Number Omega =", round(kappa(theta))))
  
  # Signal Posteriors ------------------
  if (layers == 3) {
    as_mean <- tau_w^2*t(z) %*% (theta + sigma) %*% (y_mean - rep(mu, n_fcts))
    as_cov <- tau_w^2 * diag(n_s) - tau_w^4 * t(z) %*% (theta + sigma) %*% z
    as_sd <- sqrt(diag(as_cov))
    colnames(as_mean) <- "post_mean"
    
    signal_summary <- as_mean %>% as_tibble(rownames = "characteristic") %>%
      left_join(as_sd %>% as_tibble(rownames = "characteristic") %>% rename("post_sd" = value), by = "characteristic")
  }
  
  # Factor Posteriors ------------------
  ai_cov <- solve(solve(theta) + solve(sigma))  # t_mat * solve(sigma)
  ai_sd <- sqrt(diag(ai_cov))
  ai_mean <- ai_cov %*% (solve(theta) %*% rep(mu, n_fcts) + solve(sigma) %*% y_mean) ## (t_mat * solve(sigma))
  
  rownames(ai_mean) <- names(y_mean)
  colnames(ai_mean) <- "post_mean"
  names(ai_sd) <- names(y_mean)
  
  factor_summary <- ai_mean %>% as_tibble(rownames = "char_reg") %>%
    left_join(ai_sd %>% as_tibble(rownames = "char_reg") %>% rename("post_sd" = value), by = "char_reg") %>%
    left_join(y_mean %>% as_tibble(rownames = "char_reg") %>% rename("ols_est" = value), by = "char_reg") %>%
    left_join(alpha_sd %>% as_tibble(rownames = "char_reg") %>% rename("ols_se" = value), by = "char_reg") %>%
    mutate(
      characteristic = str_split(char_reg, "__", simplify = T)[, 1],
      # characteristic = char_reg %>% str_extract(".+[?=_{2}]") %>% str_remove("__")
      p025 = post_mean - 1.96 * post_sd,
      p975 = post_mean + 1.96 * post_sd
    ) %>%
    left_join(cluster_labels, by = "characteristic") %>%
    mutate(
      region = char_reg %>% str_extract(pattern = "(?<=_{2}).+")
    ) %>%
    select(char_reg, characteristic, hcl_label, region, everything()) 
  
  # Output
  if (is.null(priors)) {
    comparison <- tibble(
      estimate = c("alpha", "tau_c", "tau_s", "tau_w")[1:(layers + 1)],
      crude = drop(unlist(start_list)),
      ml_est = c(mu, tau_c, tau_s, tau_w)[1:(layers + 1)]
    )
    if (fix_alpha) {
      ml_se <- c(NA_real_, sqrt(diag(solve(hyper_pars@details$hessian))))
    } else {
      ml_se <- sqrt(diag(solve(hyper_pars@details$hessian)))
    }
    comparison$ml_se <- ml_se
    
    print(comparison)
  }
  
  if (plot == T) {
    list("factors" = factor_summary) %>% eb_plots()
  }
  
  ret_list <- list(
    "input" = data,
    # "clusters" = cluster_summary,
    "factors" = factor_summary,
    "factor_mean" = ai_mean,
    "factor_cov" = ai_cov,
    "theta" = theta,
    "sigma" = sigma
  )
  if (is.null(sigma)) {
    ret_list[["alpha_cor_raw"]] <- alpha_cor_shrunk
    ret_list[["alpha_cor_adj"]] <- alpha_cor_adj
  }
  if (is.null(priors)) {
    ret_list[["mle"]] <- comparison
  }
  if (layers == 3) {
    ret_list$signal <- signal_summary
  }
  return(ret_list)
}

fdr_sim <- function(t_low, a_vec, a_cov, n_sim = 10000, seed=1) {
  set.seed(seed)
  t_all <- a_vec / sqrt(diag(a_cov))
  t_steps <- sort(t_all[t_all > t_low])
  t_steps <- head(t_steps, -1)  # Don't include the last t-value (no significant)
  # Simulated alphas
  sims <- mvtnorm::rmvnorm(n = n_sim, mean = a_vec, sigma = a_cov)
  # False Discovery as a Function of t-cutoff
  t_steps %>% lapply(function(t) {
    # Significant alphas under t-cutoff
    sig <- (t_all >= t) 
    # False Discovery Rate
    sims_fdr <- rowMeans(sign(sims[, sig]) != sign(a_vec[sig]))
    # Output
    tibble(t_cutoff = t, n_sig = sum(sig), fdr = mean(sims_fdr), fwr = mean(sims_fdr > 0))
  }) %>% bind_rows()
}

fdr_fwer_rates <- function(t_cutoff, a_vec, a_cov, orig_sig = F, n_sim = 10000, seed=1) {
  set.seed(seed)
  # Simulate from full posterior
  sims <- mvtnorm::rmvnorm(n = n_sim, mean = a_vec, sigma = a_cov)
  if (orig_sig == T) {
    orig_factors <- char_info %>% filter(significance == T) %>% pull(characteristic) %>% str_c("__world")
    sims <- sims[, match(x = orig_factors, table = rownames(a_vec))]
    a_vec <- a_vec[orig_factors, ]
    a_cov <- a_cov[orig_factors, orig_factors]
  } 
  t_all <- a_vec / sqrt(diag(a_cov))
  sig <- (t_all >= t_cutoff) 
  sig_sims <- sims[, sig]
  false_discoveries <- sig_sims %>% apply(1, function(x) mean(x<0))
  # FDR Distribution
  fdr_dist <- tibble(
    min = min(false_discoveries),
    p025 = quantile(false_discoveries, 0.025),
    p50 = quantile(false_discoveries, 0.5),
    p975 = quantile(false_discoveries, 0.975),
    max = max(false_discoveries),
    mean = mean(false_discoveries),
    sd = sd(false_discoveries)
  )
  fwer_dist <- tibble(
    min = min(false_discoveries!=0),
    p025 = quantile(false_discoveries!=0, 0.025),
    p50 = quantile(false_discoveries!=0, 0.5),
    p975 = quantile(false_discoveries!=0, 0.975),
    max = max(false_discoveries!=0),
    mean = mean(false_discoveries!=0),
    sd = sd(false_discoveries!=0)
  )
  # FWER Distribution
  # Output 
  print(paste("Factors:", length(t_all), "- Sig:", sum(sig)))
  print(paste("Mean p-value:", round(mean(1-pnorm(t_all[sig])), 6)))
  fwer_fdr <- tibble(t_cutoff = t_cutoff, n_sig = sum(sig), fdr = mean(false_discoveries), fwer = mean(false_discoveries > 0))
  list("fdr_dist"=fdr_dist, "fwer_dist" = fwer_dist, "fwer_fdr"=fwer_fdr)
}

# True Factors 
true_factors <- function(t_cutoff, a_vec, a_cov, orig_sig = T, n_sim = 10000, seed=1) {
  set.seed(seed)
  post_vol <- sqrt(diag(a_cov))
  # Simulate using all factors
  sims <- mvtnorm::rmvnorm(n = n_sim, mean = a_vec, sigma = a_cov)
  # Decide which factors to look at
  if (orig_sig == T) {
    orig_factors <- char_info %>% filter(significance == T) %>% pull(characteristic) %>% str_c("__world")
    orig_factors_match <- match(x = orig_factors, table = rownames(a_vec))
    sims <- sims[, orig_factors_match]
    post_vol <- post_vol[orig_factors_match]
    a_vec <- a_vec[orig_factors_match]
  }
  sims <- 1:ncol(sims) %>% sapply(function(i) sims[, i] / post_vol[i])
  true_factors_dist <- sims %>% apply(1, function(x) mean(x > t_cutoff)) 
  true_stat <- mean(a_vec/post_vol > t_cutoff)
  # From point 4 in https://influentialpoints.com/Training/bootstrap_confidence_intervals-principles-properties-assumptions.htm
  bc_ci <- function(stat, bootstraps, alpha=0.05) { # bias corrected bootstrap standard errors
    # estimate bias in std. norm deviates
    b <- qnorm((sum(bootstraps > stat)+sum(bootstraps==stat)/2)/length(bootstraps))  # Proportion of bootstrap samples above the "population" estimate. If unbiased, b=0.5. To handle discrete statistics, half of the sample at the population estimate is assumed to lie above
    z <- qnorm(c(alpha/2,1-alpha/2)) # Std. norm. limits
    p <- pnorm(z-2*b) # bias-correct & convert to proportions
    
    quantile(bootstraps,p=p) # Bias-corrected percentile lims.
  } 
  bias_corrected <- bc_ci(stat = true_stat, bootstraps = true_factors_dist, alpha = 0.05)
  tibble(
    min = min(true_factors_dist),
    p025 = quantile(true_factors_dist, 0.025),
    p50 = quantile(true_factors_dist, 0.5),
    p975 = quantile(true_factors_dist, 0.975),
    max = max(true_factors_dist),
    mean = mean(true_factors_dist),
    sd = sd(true_factors_dist),
    p025_bc = bias_corrected[1],
    p975_bc = bias_corrected[2]
  )
}

# Simulations for figure 2
sim_mt_control <- function(sim_settings) {
  # Cluster membership
  m <- matrix(0, nrow = sim_settings$n, ncol = sim_settings$clusters)  # Cluster membership
  j <- 0
  for (i in 1:sim_settings$clusters) {
    m[(j+1):(j + sim_settings$fct_pr_cl), i] <- 1
    j <- j + sim_settings$fct_pr_cl
  }
  # Correlation Matrix
  corr_mat <- m %*% t(m)
  corr_mat[corr_mat == 0] <- sim_settings$corr_across
  corr_mat[corr_mat == 1] <- sim_settings$corr_within
  diag(corr_mat) <- 1
  # Sigma
  sigma <- sim_settings$se^2 * corr_mat
  # Predefine variables
  alpha_0_vec <- rep(sim_settings$alpha_0, sim_settings$n)
  i_n <- diag(sim_settings$n)
  # Simulation
  search_grid <- expand.grid("tau_c" = sim_settings$tau_c, "tau_w" = sim_settings$tau_w)
  1:nrow(search_grid) %>% lapply(function(i) {
    tau_c <- search_grid[i, "tau_c"]
    tau_w <- search_grid[i, "tau_w"]
    print(paste("Iteration", i, "out of", nrow(search_grid)))
    alpha_noise <- MASS::mvrnorm(n = sim_settings$n_sims, mu = rep(0, sim_settings$n), Sigma = sigma) # Preallocate alpha noise for efficiency
    s <- 1:sim_settings$n_sims %>% lapply(function(s) {
      omega <- m %*% t(m) * tau_c^2 + i_n * tau_w^2
      alpha_c <- rnorm(sim_settings$clusters) * tau_c
      alpha_w <- rnorm(sim_settings$n) * tau_w
      alpha_true <- alpha_0_vec + m %*% alpha_c + alpha_w
      alpha_hat <- alpha_true + alpha_noise[s, ]
      
      post_var <- solve(solve(omega) + solve(sigma))
      post_alpha <- post_var %*% (solve(omega) %*% alpha_0_vec + solve(sigma) %*% alpha_hat)
      
      eb <- tibble("type" = "eb", "true_alpha" = drop(alpha_true), "z" = drop(post_alpha / sqrt(diag(post_var))), "p" = 2 * pnorm(abs(z), lower.tail = F))
      ols <- tibble("type" = "ols", "true_alpha" = drop(alpha_true), "z" = drop(alpha_hat / sqrt(diag(sigma))), "p" = 2 * pnorm(abs(z), lower.tail = F))
      by <- tibble("type" = "by", "true_alpha" = drop(alpha_true), "z" = ols$z)
      by$p <- p.adjust(ols$p, method = "BY")
      rbind(eb, ols, by) %>%
        mutate(sig = z > 0 & p < 0.025) %>%
        group_by(type) %>%
        summarise(
          sim = s,
          n_disc = sum(sig),
          true_disc = sum(sign(true_alpha[sig == T]) == sign(z[sig == T])),
          false_disc = n_disc - true_disc
        )
    }) %>% bind_rows()
    s %>% 
      group_by(type) %>%
      mutate(fdp = if_else(n_disc == 0, 0, false_disc / n_disc)) %>%
      summarise(
        fdr = mean(fdp),
        n_disc = mean(n_disc),
        false_disc = mean(false_disc),
        true_disc = mean(true_disc),
        tau_c = tau_c,
        tau_w = tau_w,
        n = n()
      ) %>%
      mutate(true_disc_rate = true_disc / (sim_settings$n / 2))
  }) %>% bind_rows()
}

multiple_testing <- function(eb_all, eb_world = NULL) {
  eb_all$factors %>%
    bind_rows(eb_world$factors) %>%
    mutate(
      t_ols = ols_est/ols_se,
      p_ols = 2*pnorm(abs(t_ols), lower.tail = F)
    ) %>%
    group_by(region) %>%
    mutate(
      n = n(),
      p_bonf = p_ols %>% p.adjust(method = "bonferroni"),
      p_holm = p_ols %>% p.adjust(method = "holm"),
      p_bh = p_ols %>% p.adjust(method = "BH"),
      p_by = p_ols %>% p.adjust(method = "BY")
    ) %>%
    select(n, region, char_reg, "estimate" = ols_est, "statistic" = t_ols, "se" = ols_se, starts_with("p_")) %>%
    gather(starts_with("p_"), key = "method", value = "p") %>%
    mutate(
      method = method %>% str_remove("^p_"),
      mt_adj = case_when(
        method == "ols" ~ "None",
        method == "bh" ~ "FDR",
        method == "by" ~ "FDR",
        method == "bonf" ~ "FWR",
        method == "holm" ~ "FWR"
      ),
      method = case_when(
        method == "ols" ~ "OLS",
        method == "bh" ~ "BH",
        method == "by" ~ "BY",
        method == "bonf" ~ "Bonferroni",
        method == "holm" ~ "Holm",
        TRUE ~ method
      )
    ) 
}

# Bootstrap Tangency Portfolio --
# BS Func
bootstrap_tpf <- function(data, n_boots = 100, shorting = T, seed = 1) {
  set.seed(seed)
  if (shorting) {
    boot_func <- function(splits, ...) {
      df <- analysis(splits) %>% apply(2, function(x) x / sd(x)) %>% as.data.frame()
      lm(rep(1, nrow(df)) ~ -1 + ., data = df) %>% 
        broom::tidy() %>% 
        mutate(weight = estimate / sum(estimate)) %>% 
        mutate(term = term %>% str_remove_all("`")) %>%
        select(term, weight)
    }
  } else {
    boot_func <- function(splits, ...) {
      df <- analysis(splits) %>% apply(2, function(x) x / sd(x))
      glmnet::glmnet(y = rep(1, nrow(df)), x = df %>% as.matrix(), 
                     lambda = 0, lower.limits = 0, intercept = F) %>% 
        broom::tidy(return_zeros = T) %>% 
        filter(term != "(Intercept)") %>% 
        mutate(weight = estimate / sum(estimate)) %>%
        select(term, weight)
    }
  } 
  
  data %>% 
    bootstraps(times = n_boots, apparent = T) %>% # Apparent = T --> Generate original data 
    mutate(
      coef = splits %>% map(.f = boot_func)
    ) 
}

# Full tpf
tpf_cluster <- function(data, mkt_region, orig_sig, min_date, n_boots, shorting, seed) {
  if (orig_sig) {
    orig_sig_values <- T
  } else {
    orig_sig_values <- c(T, F)
  }
  market_ret <- regional_mkt_ret[region == mkt_region]
  
  cluster_pf <- data %>%
    left_join(cluster_labels, by = "characteristic") %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>%
    filter(orig_sig %in% orig_sig_values) %>%
    group_by(hcl_label, eom) %>%
    summarise(
      ret = mean(ret)
    )
  
  tpf_data <- cluster_pf %>% 
    filter(eom >= min_date) %>% 
    spread(key = hcl_label, value = ret) %>%
    left_join(market_ret %>% select(eom, market), by = c("eom")) %>%
    rename(Market = market)
  
  tpf_data %>% select(-eom) %>% bootstrap_tpf(n_boots = n_boots, shorting = shorting, seed = seed) %>% mutate(market_region = mkt_region)
}

# In-Sample / Out-of-Sample Functions
prepare_is_oos <- function(input, min_obs, orig_group, ret_scaled, type, print=F) {  # ret_scaled in ('none', "all", "is") & type in ('is_oos', 'is_post', 'is_pre')  
  data <- input %>%
    select(characteristic, eom, ret, mkt_vw_exc) %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig %in% orig_group) %>%
    left_join(char_info %>% select(characteristic, sample_start, sample_end), by = "characteristic") %>%
    mutate(
      period = case_when(
        year(eom) >= sample_start & year(eom) <= sample_end ~ "is",
        type == "pre" & year(eom) < sample_start ~ "oos",
        type == "post" & year(eom) > sample_end ~ "oos",
        type == "pre_post" & (year(eom) < sample_start | year(eom) > sample_end) ~ "oos"
      ),
      ret = ret * 100,
      mkt_vw_exc = mkt_vw_exc * 100
    ) %>%
    filter(!is.na(period))
  
  # Exclude data
  data_excl <- data %>%
    group_by(characteristic) %>%
    mutate(n_is = sum(period == "is"), n_oos = sum(period == "oos")) %>% 
    filter(n_is >= min_obs & n_oos >= min_obs)
  
  if (ret_scaled == "none") {
    data_adj <- data_excl %>% mutate(ret_adj = ret)
  }
  
  if (ret_scaled == "all") {
    data_excl <- data_excl %>% 
      group_by(characteristic, period) %>%
      mutate(
        ret_neu = ret - cov(ret, mkt_vw_exc)/var(mkt_vw_exc) * mkt_vw_exc,
        ret_adj = ret * (10/sqrt(12))/sd(ret_neu)) %>%
      select(-ret_neu) %>%
      ungroup()
  }
  if (ret_scaled == "is") {
    is_vol <- data_excl %>%
      filter(period == "is") %>%
      group_by(characteristic) %>%
      mutate(ret_neu = ret - cov(ret, mkt_vw_exc)/var(mkt_vw_exc) * mkt_vw_exc) %>%
      summarise(
        is_sd = sd(ret_neu)
      )
    
    data_excl <- data_excl %>% 
      left_join(is_vol, by = c("region", "characteristic")) %>%
      group_by(characteristic, period) %>%
      mutate(ret_adj = ret * (10/sqrt(12))/is_sd) %>%
      ungroup() %>%
      select(-is_sd)
  }
  
  full <- data %>% summarise(fct_all = uniqueN(characteristic))
  excl <- data_excl %>% summarise(fct_excl = uniqueN(characteristic))
  
  if (print) {
    print(tibble("type"=type, full, excl))
  }
  
  return(data_excl)
}

# Economic Benefit of more Power
trading_on_significance <- function(posterior_is) {
  pf_base <- posterior_is %>%
    left_join(char_info %>% select(characteristic, significance, sample_end), by = "characteristic") %>%
    filter(significance == 1 & est_date >= sample_end) %>%
    group_by(est_date) %>%
    mutate(
      ols_p = pnorm(abs(ols_est / ols_se), lower.tail = F)*2,
      by_p = p.adjust(ols_p, method = "BY")
    ) 
  
  print(pf_base %>%
          summarise(
            rr_eb = mean(p025 > 0),
            rr_ols = mean(ols_p <= 0.05 & ols_est > 0),
            rr_by = mean(by_p <= 0.05 & ols_est > 0),
          ) %>%
          gather(rr_eb, rr_ols, rr_by, key = "type", value = "rr") %>%
          ggplot(aes(est_date, rr, colour = type)) +
          geom_point() +
          geom_line())
  
  positions <- pf_base %>%
    ungroup() %>%
    mutate(
      position_year = year(est_date) + 1,
      eb_sig = (p025 > 0),
      by_sig = (by_p <= 0.05 & ols_est > 0)
    ) %>%
    select(position_year, characteristic, eb_sig, by_sig)
  
  candidate_factors <- regional_pfs %>%
    mutate(year = year(eom)) %>%
    left_join(positions, by = c("characteristic", "year" = "position_year")) %>%
    mutate(marg_sig = (eb_sig == T & by_sig == F)) %>%
    gather(marg_sig, eb_sig, by_sig, key = "type", value = "significant")
  
  candidate_factors %>%
    filter(significant == T) %>%
    group_by(region, type, significant, eom) %>%
    summarise(
      n = n(),
      ret = mean(ret),
      mkt = mean(mkt_vw_exc)
    )  
}

# Simulation according to specification for Harvey et al (2016)
harvey_et_al_sim <- function(sim_settings, seed) {
  set.seed(seed)
  # Cluster membership
  m <- matrix(0, nrow = sim_settings$n, ncol = sim_settings$cl)  # Cluster membership
  j <- 0
  for (i in 1:sim_settings$cl) {
    m[(j+1):(j + sim_settings$fct_pr_cl), i] <- 1
    j <- j + sim_settings$fct_pr_cl
  }
  mm <- m %*% t(m)
  # Correlation Matrix
  corr_mat <- mm
  corr_mat[corr_mat == 0] <- sim_settings$corr_across
  corr_mat[corr_mat == 1] <- sim_settings$corr_within
  diag(corr_mat) <- 1
  
  # Average Correlation (Should be close to zero)
  mean(corr_mat[lower.tri(corr_mat)])
  
  # Sigma
  sigma <- sim_settings$se^2 * corr_mat
  
  # Predefine variables
  alpha_0_vec <- rep(sim_settings$alpha_0, sim_settings$n)
  i_n <- diag(sim_settings$n)
  
  # Simulation
  sim_settings$tau_ws %>% lapply(function(tau_w) {
    start <- proc.time()
    alpha_noise <- MASS::mvrnorm(n = sim_settings$n_sims, mu = rep(0, sim_settings$n), Sigma = sigma) # Preallocate alpha noise for efficiency
    tau_sim <- 1:sim_settings$n_sims %>% sapply(simplify = F, USE.NAMES = T, function(s) {
      print(paste("Tau_w:", tau_w, "- Simulation", s, "out of", sim_settings$n_sims))
      # Simulate Alphas
      alpha_c <- c(rep(sim_settings$ret, times = sim_settings$cl_true), rep(0, times = (sim_settings$cl - sim_settings$cl_true)))
      alpha_w <- c(rnorm(sim_settings$n_true) * tau_w, rep(0, sim_settings$n - sim_settings$n_true))
      alpha_true <- alpha_0_vec + m %*% alpha_c + alpha_w
      alpha_hat <- as.vector(alpha_true + alpha_noise[s, ])
      
      # MLE Function
      mle_func <- function(a, tc, tw) {
        a_vec <- rep(a, sim_settings$n)
        a_omega <- i_n * tw^2 + mm * tc^2
        a_cov <- sigma + a_omega  #  / t_mat
        
        -(mvtnorm::dmvnorm(x = alpha_hat, mean = a_vec, sigma = a_cov, log = T))  
      }
      
      # Starting Values
      starting_values <- tibble(a = alpha_hat, cl = rep(1:sim_settings$cl, each = sim_settings$fct_pr_cl)) %>%
        group_by(cl) %>%
        summarise(
          cl_mean = mean(a),
          cl_sd = sd(a)
        ) %>%
        summarize(
          crude_a0 = mean(cl_mean),
          crude_tc = if_else(sim_settings$fix_alpha, sqrt(sum((cl_mean^2) / (n() - 1))), sd(cl_mean)),
          crude_tw = mean(cl_sd)
        )
      start_list <- list(
        a = starting_values$crude_a0, 
        tc = starting_values$crude_tc, 
        tw = starting_values$crude_tw)
      
      # Estimate Parameters
      if (sim_settings$fix_alpha) {
        (hyper_pars <- stats4::mle(minuslogl = mle_func, start = start_list, lower = c(-Inf, 0, 0), fixed = list(a = 0)))
      } else {
        (hyper_pars <- stats4::mle(minuslogl = mle_func, start = start_list, lower = c(-Inf, 0, 0)))
      }
      
      # Check convergence
      if (hyper_pars@details$convergence != 0) {
        warning("MLE step did not converge!!!")
        return(NULL)
      }
      mu <- hyper_pars@fullcoef["a"]
      tc <- hyper_pars@fullcoef["tc"]
      tw <- hyper_pars@fullcoef["tw"]
      mle <- tibble(
        s = rep(s, 3),
        coef = c("a", "tc", "tw"),
        mle = c(mu, tc, tw),
        crude = c(start_list$a, start_list$tc, start_list$tw)
      )
      print(mle)
      
      # Specify Posterior
      omega <- i_n * tw^2 + mm * tc^2
      post_cov <- solve(solve(omega) + solve(sigma))
      post_alpha <- post_cov %*% (solve(omega) %*% alpha_0_vec + solve(sigma) %*% alpha_hat)
      
      list("alpha_true" = alpha_true, "alpha_hat" = alpha_hat, "post_alpha" = post_alpha, "post_cov" = post_cov, "mle" = mle)
    })
    print(proc.time() - start)  # 5 iterations took 232.13/60 = 4 minutes  
    return(tau_sim)
  }) 
}


# Single Factor TP --
sr_func <- function(data, w) {
  ret_vec <- data %>% colMeans()
  cov_mat <- data %>% cov()
  drop(w %*% ret_vec / sqrt(t(w) %*% cov_mat %*% w))
}

epo_tpf <- function(data, s) {
  sd <- data %>% apply(2, sd)
  cor <- data %>% cor()
  ret_vec <- data %>% colMeans()
  cor_shrunk <- diag(length(sd)) * s + cor * (1-s)
  cov_shrunk <- diag(sd) %*% cor_shrunk %*% diag(sd)
  drop((solve(cov_shrunk) %*% ret_vec) / sum(solve(cov_shrunk) %*% ret_vec))
}

# Prepare data for 
prepare_tpf_factors <- function(region, orig_sig_values, start, scale) {
  mkt <- regional_mkt_ret %>% filter(region == !!region) %>% select(-region)
  
  tpf_factors <- eb_est[[region]]$input$long %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>%
    filter(orig_sig %in% orig_sig_values) %>%
    group_by(eom) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(eom >= start) %>%
    select(characteristic, eom, n, ret)
  
  # Ensure that all factors have data
  missing <- tpf_factors %>% filter(n != max(n)) %>% select(eom, n) %>% distinct()
  if (nrow(missing) > 0) {
    warning("UNBALANCED PANEL - SOME FACTORS ARE MISSING DATA!")
  }
  tpf_factors <- tpf_factors %>% filter(n == max(n)) %>% select(-n)  
  
  # 1. Overall TPF
  tpf_factors <- tpf_factors %>%
    bind_rows(mkt %>% rename("ret"="market") %>% mutate(characteristic = "market") %>% filter(eom %in% tpf_factors$eom)) 
  
  if (scale) {
    tpf_factors <- tpf_factors %>%
      group_by(characteristic) %>%
      mutate(ret = ret * (0.1 / sqrt(12)) / sd(ret))
  }
  
  tpf_factors_wide <- tpf_factors %>%
    pivot_wider(names_from = characteristic, values_from = ret) %>%
    select(-eom) 
  # Output
  list("long"=tpf_factors, "wide" = tpf_factors_wide)
}

# Optimal Shrinkage
optimal_shrinkage <- function(data, k, epo_range = seq(0, 1, 0.1)) {
  finance_kfold <- function(dates, k, horizon) {
    # Helper Function
    helper_eom_seq <- function(ends, horizon) {
      all <- ends %>% lapply(function(d) seq.Date(from = ceiling_date(d, unit = "month") - months(horizon - 1), to = ceiling_date(d, unit = "month"), by = "1 month") - 1)
      all_unique <- do.call(c, all) %>% unique()
    }
    # Split Dates into k groups
    date_vec <- dates %>% unique() %>% sort()
    n <- length(date_vec)
    n_fold <- floor(length(date_vec) / k)
    split <- split(date_vec, cut(seq_along(date_vec), k, labels = FALSE))
    # Create train/test split
    1:k %>% lapply(function(i) {
      test_ends <- split[[i]]
      test_dates <- test_ends %>% helper_eom_seq(horizon = horizon)
      train_ends <- do.call(c, split[-i])
      train_dates <- train_ends %>% helper_eom_seq(horizon = horizon)
      # Avoid Data Leakage
      train_dates <- train_dates[!(train_dates %in% test_dates)]
      tibble(fold = i, train = list(train_dates), test = list(test_dates))
    }) %>%
      bind_rows()
  }
  
  date_split <- unique(data$eom) %>% finance_kfold(k = k, horizon = 1)
  cross_val <- 1:k %>% lapply(function(i)  {
    test_dates <- date_split$test[[i]]
    test <- data %>%
      filter(eom %in% test_dates) %>%
      pivot_wider(names_from = characteristic, values_from = ret) 
    test_eom <- test$eom
    test <- test %>% select(-eom)
    
    train_dates <- date_split$train[[i]]
    train <- data %>%
      filter(eom %in% train_dates) %>%
      pivot_wider(names_from = characteristic, values_from = ret) %>%
      select(-eom) 
    
    # Create Weights
    nonneg_w <- glmnet::glmnet(y = rep(1, nrow(train)), x = train %>% as.matrix(), 
                               lambda = 0, lower.limits = 0, intercept = F) %>% 
      tidy(return_zeros = T) %>% 
      filter(term != "(Intercept)") %>% 
      mutate(
        type = "Non-Negative",
        weight = estimate / sum(estimate)
      ) %>%
      select(type, weight)
    
    epo_w <- epo_range %>% lapply(function(s) {
      epo <- train %>% epo_tpf(s = s)
      tibble(type = paste0("EPO s=", s), weight = epo)
    })
    
    # OOS Performance
    c(list(nonneg_w), epo_w) %>% lapply(function(x) {
      w <- x$weight
      tibble(
        type = unique(x$type),
        eom = test_eom, 
        ret = drop(as.matrix(test) %*% w)
      )
    }) %>%
      bind_rows() %>%
      mutate(
        i = i,
        test_range = paste0(year(min(test_dates)), "-", year(max(test_dates)))
      )
  }) %>% bind_rows()
  
  cross_val_summary <- cross_val %>%
    group_by(type) %>%
    summarise(
      ann_ret = mean(ret),
      sd = sd(ret),
      sr = ann_ret/sd
    ) %>%
    mutate(
      type_overall = if_else(str_detect(type, "EPO"), "EPO", "Non-Negative"),
      type = if_else(type == "EPO s=0", "Unconstrained", type),
      type = type %>% factor(levels = c("Non-Negative", "Unconstrained", paste0("EPO s=", seq(0.1, 1, 0.1))))
    ) 
  
  print(cross_val_summary %>%
          ggplot(aes(type, sr, group=type_overall)) +
          geom_point() +
          geom_path() +
          labs(colour = "Test Period:", y = "Monthly OOS SR of TPF") +
          theme(legend.position = "top", axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, vjust=0.5)))
  
  opt_s_summary <- cross_val_summary %>% filter(type_overall == "EPO" & sr == max(sr)) %>% mutate(s = type %>% str_remove("EPO s=") %>% as.numeric()) 
  print(paste0("Highest OOS SR: ", opt_s_summary %>% pull(sr) %>% round(2), ", Standard MVO: ", cross_val_summary %>% filter(type == "Unconstrained") %>% pull(sr) %>% round(2)))
  
  # Optimal shrinkage
  opt_s_summary %>% pull(s)
}


# Table Functions --------------------------------------------------------
table_is_oos_ols <- function(is_oos_regs, is_post_regs) {
  oos_us <- lm(oos ~ is, data = is_oos_regs %>% filter(region == "us"))
  post_us <- lm(post ~ is, data = is_post_regs %>% filter(region == "us"))
  
  stargazer::stargazer(
    post_us, oos_us,
    title = "OLS - Biased: $\\hat{\\alpha}_\\text{Out-of-Sample} = \\gamma_0 + \\gamma_1\\times\\hat{\\alpha}_\\text{In-Sample}$", out.header=T,
    no.space=T, digits=3, type='latex', single.row=F,
    align = T, notes.align = "l",
    omit.stat = c("adj.rsq", "f", "ser"),
    covariate.labels = c("Intercept", "$\\hat{\\alpha}_\\text{IS}$"), dep.var.caption = "Dependent Variable:", dep.var.labels = c("$\\hat{\\alpha}_\\text{Post IS}$", "$\\hat{\\alpha}_\\text{Pre \\& Post IS}$"),
    notes.append=T, report = "vc*t", intercept.bottom = F)
}

table_is_oos_nls <- function(nls_post, nls_oos) {
  info <- list(
    "post" = list(
      "dep" = "post",
      "fit" = nls_post
    ),
    "oos" = list(
      "dep" = "oos",
      "fit" = nls_oos
    )
  ) 
  
  op <- info %>% sapply(simplify = F, USE.NAMES = T, function(x) {
    # Create Fake Linear Model
    fake_data <- tibble(y = rnorm(10), k0 = rnorm(10), kh = rnorm(10))
    colnames(fake_data) <- c(x$dep, "k0", "kh")
    lm_string <- paste0(x$dep, "~k0 + kh -1")
    fake_lm = lm(lm_string, data = fake_data)
    fake_x = c("k0", "kh")
    
    # Generate various parts for output
    sum_xx = summary(x$fit$nls_fit)
    mat_xx = sum_xx$coefficients[1:2, ]
    colnames(mat_xx) = c("coef","se", "t", "p")
    indVarNames = rownames(mat_xx)
    
    # Generate coefficients, se, t-stat and p values 
    df_xx = as.data.frame(mat_xx)
    vCoef = df_xx$coef; names(vCoef)=fake_x
    vSE = df_xx$se; names(vSE)=fake_x
    vT = df_xx$t; names(vT)=fake_x
    vP = df_xx$p; names(vP)=fake_x
    
    formulaTxt = sum_xx$formula
    nParameters = sum_xx$df[1]
    nDF = sum_xx$df[2]
    obs <- length(x$fit$nls_fit$m$resid())
    n_fcts <- uniqueN(x$fit$nls_data$c)
    rss = round(sum_xx$sigma, 3)
    convTolerance = x$fit$nls_fit$m$conv()
    list("lm"=fake_lm, "coef" = vCoef, "se" = vSE, "t" = vT, "p" = vP, "rss" = rss, "obs" = obs, "n_fcts" = n_fcts)
  })
  
  # Determine order
  y1 <- "post"
  y2 <- "oos"
  
  # Aesthetics 
  vTitle = "NLS - Unbiased: $R_{i,t} = \\alpha_i + (\\kappa_0 + \\kappa_h \\times \\alpha_i)\\times 1_\\text{\\{Out-of-Sample\\}}$"
  vType = "latex"
  # v_col_label = c("USA", "Developed", "Emerging")
  lines_obs <- c("Observations", 
                 sprintf("\\multicolumn{1}{r}{%s}", prettyNum(op[[y1]]$obs, big.mark = ",")), 
                 sprintf("\\multicolumn{1}{r}{%s}", prettyNum(op[[y2]]$obs, big.mark = ",")))
  lines_fcts <- c("Factors", sprintf("\\multicolumn{1}{r}{%s}", op[[y1]]$n_fcts), sprintf("\\multicolumn{1}{r}{%s}", op[[y2]]$n_fcts))
  dep_var_options <- list("post" = "Post IS", "oos" = "Pre \\& Post IS")
  dep_var_lbls <- c(dep_var_options[[y1]], dep_var_options[[y2]])
  
  # Output
  stargazer::stargazer(
    op[[y1]]$lm, op[[y2]]$lm,
    title = vTitle, out.header=T,
    no.space=T, digits=3, type=vType, single.row=F,
    align = T, notes.align = "l",
    omit.stat = c("rsq","adj.rsq", "f", "n", "ser"),
    covariate.labels = c("$\\kappa_0$", "$\\kappa_h$"), dep.var.labels.include = T, dep.var.caption = "OOS Period:", dep.var.labels = dep_var_lbls,
    add.lines=list(lines_fcts, lines_obs), report = "vc*t", intercept.bottom = F,
    # notes=vNotes, notes.append=T,
    coef=list(op[[y1]]$coef, op[[y2]]$coef), se=list(op[[y1]]$se, op[[y2]]$se), t=list(op[[y1]]$t, op[[y2]]$t), p=list(op[[y1]]$p, op[[y2]]$p)
  )
}

table_taus <- function(){
  taus <- list(
    list("USA", "us"),
    list("Developed", "developed"),
    list("Emerging", "emerging"),
    list("USA, Developed & Emerging", "all"),
    list("World", "world"),
    list("World ex. US", "world_ex_us"),
    list("USA - Mega", "us_mega"),
    list("USA - Large", "us_large"),
    list("USA - Small", "us_small"),
    list("USA - Micro", "us_micro"),
    list("USA - Nano", "us_nano")
  ) %>% lapply(function(x) {
    eb_est[[x[[2]]]]$mle %>% 
      select(estimate, ml_est) %>%
      spread(key = estimate, value = ml_est) %>%
      mutate(sample = x[[1]])
  }) %>% 
    bind_rows() %>%
    select(sample, tau_c, tau_s, tau_w)
  
  tau_cap <- paste(
    "The table shows the tau parameters estimated by maximum likelihood.",
    "$\\tau_c$ is the estimated dispersion in cluster alphas.",
    "$\\tau_w$ is the estimated dispersion in factor alphas with a cluster.",
    "$\\tau_s$ is the estimated dispersion in alpha of the same factor in different regions."
  )
  
  taus %>%
    select("Sample" = sample, "$\\tau_c$" = tau_c, "$\\tau_w$" = tau_s, "$\\tau_s$" = tau_w) %>% # Here I use the notation from eq 23
    xtable(auto=T, digits = 2, caption = tau_cap) %>% 
    print(include.rownames = F, caption.placement = "top", sanitize.colnames.function = identity) 
}

# Table - Factor Performance
table_factor_info <- function() {
  table <- eb_est$all$factors %>%
    mutate(p_zero = pnorm(q = 0, mean = post_mean, sd = post_sd)) %>%
    select(characteristic, region, ols_est, "eb_est" = post_mean, p_zero) %>% 
    pivot_wider(names_from = region, values_from = c(ols_est, eb_est, p_zero), names_sep = "_") %>%
    select(characteristic, ends_with("_us"), ends_with("_developed"), ends_with("_emerging")) %>%
    left_join(char_info %>% select(characteristic, significance), by = "characteristic") %>%
    mutate(char_name = if_else(significance == 0, paste0(characteristic, "*"), characteristic)) %>%
    select(-characteristic, -significance) %>%
    select(char_name, everything()) %>%
    arrange(ols_est_us) %>%
    as.data.frame() 
  
  data.frame(table[, 1:4], "empty1" = rep("", nrow(table)), table[, 5:7], "empty2" = rep("", nrow(table)), table[, 8:10]) %>%
    xtable() %>%
    print()
}

table_economic_benefit <- function(sig_pfs) {
  sig_regs <- c("us", "developed", "emerging") %>% lapply(function(x) {
    fit <- lm(ret ~ mkt, data = sig_pfs %>% filter(type == "marg_sig" & region == x) %>% mutate(ret = ret*100, mkt = mkt*100)) # Ensures alpha is in Percentages
    nw <- fit %>% lmtest::coeftest(vcov = sandwich::NeweyWest(fit, lag = 6))
    list("fit"=fit, "nw"=nw)
  })
  
  lines_obs <- c("Observations", 
                 sprintf("\\multicolumn{1}{r}{%s}", prettyNum(length(sig_regs[[1]]$fit$residuals), big.mark = ",")), 
                 sprintf("\\multicolumn{1}{r}{%s}", prettyNum(length(sig_regs[[2]]$fit$residuals), big.mark = ",")), 
                 sprintf("\\multicolumn{1}{r}{%s}", prettyNum(length(sig_regs[[3]]$fit$residuals), big.mark = ",")))
  lines_r2 <- c("Adjusted $R^2$", 
                sprintf("\\multicolumn{1}{r}{%s}", formatC(summary(sig_regs[[1]]$fit)$adj.r.squared, digits = 2, format = "f")), 
                sprintf("\\multicolumn{1}{r}{%s}", formatC(summary(sig_regs[[2]]$fit)$adj.r.squared, digits = 2, format = "f")), 
                sprintf("\\multicolumn{1}{r}{%s}", formatC(summary(sig_regs[[3]]$fit)$adj.r.squared, digits = 2, format = "f")))
  
  stargazer::stargazer(sig_regs[[1]]$nw, sig_regs[[2]]$nw, sig_regs[[3]]$nw, dep.var.labels.include = F, dep.var.caption = "Region",
                       no.space = F, intercept.bottom = F, report = "vc*t", column.labels = c("US", "Developed ex. US", "Emerging"),
                       add.lines=list(lines_obs, lines_r2), covariate.labels = c("Alpha", "Market Beta"), align=T, digits=2)
}



# PLOT FUNCTIONS ---------------------------------------------------------
cluster_val <- function(cor, labels, op_format = "pdf") {
  pairwise_cor <- cor %>%
    as_tibble(rownames = "char1") %>%
    gather(-char1, key = "char2", value = "cor") %>%
    left_join(select(labels, characteristic, "label1" = hcl_label), by = c("char1"="characteristic")) %>%
    left_join(select(labels, characteristic, "label2" = hcl_label), by = c("char2"="characteristic")) %>%
    filter(char1 != char2) %>% 
    mutate(hcl_pair = str_c(label1, "_", label2)) %>%
    group_by(hcl_pair) %>%
    summarise(
      n = n(),
      cor_avg = mean(cor)
    ) %>%
    ungroup() %>%
    separate(hcl_pair, c("hcl1", "hcl2"), sep = "_") %>%
    select(hcl1, hcl2, cor_avg) %>%
    spread(key = hcl2, cor_avg) 
  
  pairwise_cor_names <- pairwise_cor$hcl1
  pairwise_cor <- pairwise_cor %>% select(-hcl1) %>% as.matrix()
  rownames(pairwise_cor) <- pairwise_cor_names
  
  # Needs to Be saved as a functional
  if (op_format == "tex") {
    corrplot_cex <- list(tl = 0.8, number = 0.5)
  }
  if (op_format == "pdf") {
    corrplot_cex <- list(tl = 0.7, number = 0.45)
  }
  function() {
    par(xpd=TRUE)
    pairwise_cor %>%
      corrplot::corrplot(method = "color", addCoef.col = "black", type = "lower", mar = c(0, 0, 3, 0), tl.cex = corrplot_cex$tl,
                         number.cex = corrplot_cex$number, tl.col = "black", col = colorRampPalette(c(colours_theme[2], "white", colours_theme[1]))(200))
  }
}
plot_mt_eb_comp <- function(mt, eb_all, eb_us = NULL, eb_developed = NULL, eb_emerging = NULL, eb_world = NULL, mts = c("OLS", "Bonferroni", "BY"), regs = c("us", "developed", "emerging", "world"), se_methods, se_regions) {
  mt_sub <- mt %>%
    mutate(method = method %>% factor(levels = c("OLS", "Bonferroni", "Holm", "BH", "BY", "EB - Region", "EB - Full"))) %>%
    filter(method %in% mts & region %in% regs)
  
  (t_cutoff <- mt_sub %>% 
      group_by(method, region) %>%
      summarise(
        t_cut = (min(abs(statistic)[p < 0.05]) + max(abs(statistic)[p > 0.05])) / 2
      ))
  
  eb_comb <- bind_rows(
    eb_all$factors %>% mutate(method = "EB - All"),
    eb_us$factors %>% mutate(method = "EB - Region"),
    eb_developed$factors %>% mutate(method = "EB - Region"),
    eb_emerging$factors %>% mutate(method = "EB - Region"),
    eb_world$factors %>% mutate(method = "EB - Region")
  ) %>% mutate(
    method = method %>% factor(levels = c("OLS", "Bonferroni", "Holm", "BH", "BY", "EB - Region", "EB - All"))
  )
  
  mt_table <- mt_sub %>%
    mutate(characteristic = char_reg %>% str_remove("__.+")) %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig == T) %>%
    group_by(region, method, mt_adj) %>%
    summarise(
      n = n(),
      significant = mean(p < 0.05 & estimate > 0), # Estimates also needs to be positive
      max_t_insig = max(abs(statistic[p > 0.05]))
    ) 
  
  eb_table <- eb_comb %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig == T) %>%
    mutate(ols_t = ols_est / ols_se) %>%
    group_by(region, method) %>%
    summarise(
      mt_adj = "Bayesian",
      n = n(),
      significant = mean(p025 > 0),
      max_t_insig = max(abs(ols_t[p025 < 0 & p975 > 0]))
    )
  
  (comp_table <- mt_table %>% 
      bind_rows(eb_table)) 
  
  # To install drlib put: devtools::install_github("dgrtwo/drlib")
  if (FALSE) {
    repl_plot <- comp_table %>%
      mutate(
        region_pretty = case_when(
          region == "us" ~ "US",
          region == "developed" ~ "Developed Ex. US",
          region == "emerging" ~ "Emerging",
          region == "world" ~ "World"
        ),
        region_pretty = region_pretty %>% factor(levels = c("US", "Developed Ex. US", "Emerging", "World"))
      ) %>%
      filter((region == "world" & method == "EB - All") | region != "world") %>%
      filter(method != "Bonferroni") %>%
      # filter(!(region == "world" & method != "Empirical Bayes")) %>%
      ggplot(aes(drlib::reorder_within(method, significant, region_pretty), significant, fill = method)) +
      geom_col() +
      drlib::scale_x_reordered() +
      geom_text(aes(label = str_c(formatC(round(significant * 100, 2), digits = 2, format = "f"), "%")), nudge_y = 0.025, size = 3) +
      facet_wrap(~region_pretty, nrow = 1, scales = "free_x") +
      labs(x = "Method", fill = "Multiple Testing Adj.", y = "Replication Rate (%)") +
      theme(legend.position = "none")
  }
  
  repl_plot <- comp_table %>%
    # filter((region == "world" & method == "EB - All") | region != "world") %>%
    filter(method != "Bonferroni") %>%
    group_by(method) %>%
    # mutate(sort_var = significant[region == "us"]) %>%
    mutate(
      region_pretty = case_when(
        region == "us" ~ "US",
        region == "developed" ~ "Developed Ex. US",
        region == "emerging" ~ "Emerging",
        region == "world" ~ "World"
      ),
      region_pretty = region_pretty %>% factor(levels = c("US", "Developed Ex. US", "Emerging", "World")),
      method_pretty = case_when(
        method == "BY" ~ "Benjamini-Yekutieli",
        method == "EB - Region" ~ "Empirical Bayes - Region",
        method == "EB - All" ~ "Empirical Bayes - All",
        method == "OLS" ~ "OLS"
      ),
      method_pretty = method_pretty %>% factor(levels = c("OLS", "Benjamini-Yekutieli", "Empirical Bayes - Region", "Empirical Bayes - All"))
    ) %>%
    ggplot(aes(method_pretty, significant*100, fill = method_pretty)) + #reorder(method_pretty, sort_var)
    geom_col() +
    geom_text(aes(label = str_c(formatC(round(significant * 100, 1), digits = 1, format = "f"), "%")), nudge_y = 2.5, size = 3) +
    facet_grid(. ~ region_pretty, scales = "free", space='free') +
    # facet_wrap(~region_pretty, nrow = 1, scales = "free_x") +
    labs(x = "Method", fill = "Multiple Testing Adj.", y = "Replication Rate (%)") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), axis.title.x = element_blank())
  
  
  eb_overview <- eb_comb %>% 
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    mutate(
      t_cut = 1.96,
      type = case_when(
        p025 > 0 & orig_sig == 1 ~ "Replicated",
        p025 <= 0 & orig_sig == 1 ~ "Not Replicated",
        orig_sig == 0 ~ "Never Significant"
      )
    ) %>% 
    select(region, method, type, char_reg, estimate = post_mean, t_cut, se = post_sd)
  
  
  
  mt_plot <- mt_sub %>%
    mutate(characteristic = char_reg %>% str_remove("__.+")) %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    left_join(t_cutoff, by = c("region", "method")) %>%
    mutate(
      # significant = if_else(p < 0.05 & estimate > 0, "Significant", "Insignificant"),
      type = case_when(
        p <= 0.05 & estimate > 0 & orig_sig == 1 ~ "Replicated",
        (p > 0.05 | estimate <= 0) & orig_sig == 1 ~ "Not Replicated",
        orig_sig == 0 ~ "Never Significant"
      )
    ) %>%
    bind_rows(eb_overview) %>%
    filter(region %in% se_regions & method %in% se_methods) %>%
    mutate(
      characteristic = char_reg %>% str_extract(".+[?=__]") %>% str_remove("__"),
      type = type %>% factor(levels = c("Replicated", "Not Replicated", "Never Significant")),
      method_pretty = case_when(
        method == "BY" ~ "Multiple Testing - Benjamini-Yekutieli",
        method == "EB - Region" ~ "Empirical Bayes - US",
        method == "EB - All" ~ "Empirical Bayes - Global",
        method == "OLS" ~ "OLS"
      ),
      method_pretty = method_pretty %>% factor(levels = c("OLS", "Multiple Testing - Benjamini-Yekutieli", "Empirical Bayes - US", "Empirical Bayes - Global"))
    ) %>%
    group_by(characteristic) %>%
    # mutate(sort_var = statistic[method == "OLS" & region == "us"]) %>%
    mutate(sort_var = estimate[method == "OLS" & region == "us"]) %>%
    group_by(region, method) %>%
    mutate(
      ols_rank = frank(sort_var),
      repl_rate = sum(type == "Replicated") / sum(type %in% c("Replicated", "Not Replicated"))
    ) %>%
    ggplot(aes(reorder(ols_rank, sort_var), estimate, colour = type, linetype = type)) +
    geom_point() +
    geom_text(aes(x = 35, y = 1.45, label = str_c("Replication Rate: ", formatC(round(repl_rate*100, 1), digits = 1, format = "f"), "%")), 
              colour = "black", size = 3, check_overlap = T) +
    geom_errorbar(aes(ymin = estimate - t_cut * se, ymax = estimate + t_cut * se)) +
    facet_wrap(~method_pretty, ncol = length(se_methods) / 2) + 
    coord_cartesian(ylim = c(-1, 1.5)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    guides(colour = guide_legend(override.aes = list(shape = NA))) + 
    labs(y = "Monthly Alpha (%)") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      # text = element_text(size = 13),
      legend.title = element_blank(),
      legend.position = "top"
    )
  
  list("mt" = mt_plot, "repl" = repl_plot)
}

plot_fdr <- function(simulated_fdr) {
  simulated_fdr %>%
    gather(fdr, fwr, key = "type", value = "rate") %>%
    mutate(type = type %>% str_to_upper()) %>%
    ggplot(aes(t_cutoff, rate, colour = type)) +
    geom_point() +
    geom_hline(yintercept = 0.05, linetype = "dashed") + 
    geom_vline(xintercept = 1.96, linetype = "dotted") +
    scale_y_continuous(breaks = c(0, 0.05, 0.25, 0.5, 0.75, 1)) +
    scale_x_continuous(breaks = c(0, 1.96, 2.5, 5.0, 7.5, 10)) +
    labs(x = "Critical Value (t)", y = "Rate", colour = "Type:") +
    theme(
      legend.position = "top"
    )
}

plot_factor_post <- function(eb, orig_sig, cluster_order) {
  if (orig_sig) {
    orig_sig_values <- T
  } else {
    orig_sig_values <- c(T, F)
  }
  
  eb$factors %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>%
    filter(orig_sig %in% orig_sig_values) %>%
    group_by(hcl_label) %>%
    mutate(
      sort_var = median(post_mean) + post_mean / 1000000
    ) %>%
    mutate(hcl_label = hcl_label %>% factor(levels = cluster_order)) %>%
    ggplot(aes(reorder(characteristic, sort_var), post_mean, colour = hcl_label, shape = hcl_label)) +
    geom_point() +
    scale_shape_manual(values=1:13) +
    geom_errorbar(aes(ymin = post_mean - 1.96 * post_sd, ymax = post_mean + 1.96 * post_sd)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(y = "Monthly Alpha with 95% Confidence Interval (%)", colour = "Cluster", shape = "Cluster") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
}

plot_repl_region <- function(eb_all, cluster_order) {
  eb_all$factors %>% 
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig == 1) %>%
    mutate(hcl_label = hcl_label %>% factor(levels = cluster_order)) %>%
    group_by(region, hcl_label) %>% 
    summarise(n = n(), repl_rate = mean(post_mean - 1.96 * post_sd > 0)) %>% 
    group_by(hcl_label) %>%
    mutate(
      sort_var = repl_rate[region == "us"] + n[region == "us"] / 1e6,
      region_pretty = case_when(
        region == "us" ~ "USA",
        region == "developed" ~ "Developed Ex. USA",
        region == "emerging" ~ "Emerging"
      ),
      region_pretty = region_pretty %>% factor(levels = c("USA", "Developed Ex. USA", "Emerging"))
    ) %>%
    ggplot(aes(reorder(hcl_label, sort_var), repl_rate*100, fill = hcl_label)) +
    geom_col() +
    labs(y = "Replication Rate (%)") +
    facet_wrap(~region_pretty, ncol = 1) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
}

# Figure 1 - Waterfall Graph
plot_lit_comp <- function(eb_us, mt_res, eb_world, excl_insig=T) {
  if (excl_insig) {
    sig_group <- T
  } else {
    sig_group <- c(T, F)
  }
  raw_reg <- eb_us$input$long %>%
    group_by(characteristic) %>%
    nest() %>%
    mutate(
      raw_reg = data %>% map(~ lm(ret ~ 1, data = .x)),
      tidied = raw_reg %>% map(tidy)
    ) %>%
    unnest(tidied) %>% 
    ungroup() 
  
  raw_overall <- raw_reg %>%
    summarise(repl_rate = mean(p.value < 0.05 & estimate > 0)) %>%
    pull(repl_rate)
  
  raw_sig <- raw_reg %>% 
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig == T) %>%
    summarise(repl_rate = mean(p.value < 0.05 & estimate > 0)) %>%
    pull(repl_rate)
  
  capm <- mt_res %>% 
    filter(region == "us" & method %in% c("BY", "OLS")) %>% 
    mutate(characteristic = char_reg %>% str_remove("__.+")) %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig %in% sig_group) %>%
    group_by(method) %>% 
    summarise(repl_rate = mean(p < 0.05 & estimate > 0))
  
  eb_us_repl <- eb_us$factors %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig %in% sig_group) %>%
    summarise(repl_rate = mean(p025 > 0)) %>%
    pull(repl_rate)
  
  eb_global_repl <- eb_world$factors %>% 
    ungroup() %>% 
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig %in% sig_group) %>%
    summarise(repl_rate = mean(post_mean - 1.96 * post_sd > 0)) %>% 
    pull(repl_rate)
  
  # Waterfall Graph
  litterature_comp <- tribble(
    ~ type, ~repl_rate,
    "hxz", 0.35,
    "raw", raw_overall,
    "raw_sig", raw_sig,
    "alpha", capm %>% filter(method == "OLS") %>% pull(repl_rate),
    "mt", capm %>% filter(method == "BY") %>% pull(repl_rate),
    "eb_us", eb_us_repl,
    "eb_global", eb_global_repl
  ) 
  if (excl_insig == F) {
    litterature_comp <- litterature_comp %>% filter(type != "raw_sig")
  } 
  litterature_comp <- litterature_comp %>%
    mutate(
      repl_rate = repl_rate * 100,
      type = type %>% factor(levels = c("hxz", "raw", "raw_sig", "alpha", 
                                        "mt", "eb_us", "eb_global")),
      prev_repl_rate = dplyr::lag(repl_rate, default = 0),
      impact = if_else(repl_rate > prev_repl_rate, "Increase", "Decrease"),
      impact = impact %>% factor(levels = c("Increase", "Decrease"))
    ) %>%
    setDT() 
  
  w <- 0.3  #use to set width of bars
  l1 <- -3
  inc <- -3 
  col_top <- "black" # colours_theme[2]
  col_bot <- "black" # colours_theme[1]
  type <- litterature_comp$type
  plot <- litterature_comp %>%
    ggplot(aes(xmin = as.integer(type) - w, xmax = as.integer(type) + w, ymin = prev_repl_rate, ymax = repl_rate, fill = impact)) +
    geom_rect() +
    geom_segment(data = litterature_comp[1:(.N - 1)], aes(x = as.integer(type) + w, xend = as.integer(type) + w + 1, y = repl_rate, yend = repl_rate)) +
    scale_x_discrete(limits = type) +
    geom_text(aes(x = as.integer(type), y = repl_rate + if_else(sign(repl_rate - prev_repl_rate)!=0, sign(repl_rate - prev_repl_rate), 1) * 2, label = str_c(formatC(round(repl_rate, 1), digits = 1, format = "f"), "%"))) + 
    scale_fill_manual(values = (c("Increase" = colours_theme[1], "Decrease" = colours_theme[2]))) +
    labs(x = "Implementation", y = "Replication Rate (%)") +
    coord_cartesian(ylim = c(0, 90), expand = FALSE, clip = "off") +
    # HXZ
    annotate(geom = "text", x = "hxz", y = l1, label = "Hou, Xue, and", colour = col_top, fontface = 2) +
    annotate(geom = "text", x = "hxz", y = l1 + inc*1, label = "Zhang (2020)", colour = col_top, fontface = 2) +
    annotate(geom = "text", x = "hxz", y = l1 + inc*2, label = "Raw returns", colour = col_bot, fontface = 1) +
    # Our Raw
    annotate(geom = "text", x = "raw", y = l1, label = "Our sample", colour = col_top, fontface = 2) +
    annotate(geom = "text", x = "raw", y = l1 + inc*1, label = "Raw returns,", colour = col_bot) +
    annotate(geom = "text", x = "raw", y = l1 + inc*2, label = "our methodology", colour = col_bot) +
    # Our Alpha
    annotate(geom = "text", x = "alpha", y = l1, label = "Our sample", colour = col_top, fontface = 2) +
    annotate(geom = "text", x = "alpha", y = l1 + inc*1, label = "CAPM alphas", colour = col_bot) +
    # Our MT
    annotate(geom = "text", x = "mt", y = l1, label = "Harvey, Liu, and", colour = col_top, fontface = 2) +
    annotate(geom = "text", x = "mt", y = l1 + inc*1, label = "Zhu (2016)", colour = col_bot, fontface = 2) +
    annotate(geom = "text", x = "mt", y = l1 + inc*2, label = "Multiple testing", colour = col_bot) +
    annotate(geom = "text", x = "mt", y = l1 + inc*3, label = "adjustment", colour = col_bot) +
    # Our EB-US
    annotate(geom = "text", x = "eb_us", y = l1, label = "Our Bayesian", colour = col_top, fontface = 2) +
    annotate(geom = "text", x = "eb_us", y = l1 + inc*1, label = "estimation", colour = col_top, fontface = 2) +
    annotate(geom = "text", x = "eb_us", y = l1 + inc*2, label = "US data", colour = col_bot) +
    # Our EB-US
    annotate(geom = "text", x = "eb_global", y = l1, label = "Our Bayesian", colour = col_top, fontface = 2) +
    annotate(geom = "text", x = "eb_global", y = l1 + inc*1, label = "estimation", colour = col_top, fontface = 2) +
    annotate(geom = "text", x = "eb_global", y = l1 + inc*2, label = "Global data", colour = col_bot) +
    theme(
      legend.title = element_blank(),
      plot.margin = unit(c(1, 1, 4, 1), "lines"),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()) 
  
  if (excl_insig) {
    # Our Raw Significant only
    plot <- plot +
      annotate(geom = "text", x = "raw_sig", y = l1, label = "Our sample", colour = col_top, fontface = 2) +
      annotate(geom = "text", x = "raw_sig", y = l1 + inc*1, label = "Excl. factors", colour = col_bot) +
      annotate(geom = "text", x = "raw_sig", y = l1 + inc*2, label = "never significant", colour = col_bot)
  }
  return(plot)
}

plot_many_factors <- function() {
  # The Power of Many Factors
  many_factors_se <- eb_est$us$input$long %>%
    select("char_reg" = name_wide, ret_neu_scaled, mkt_vw_exc) %>%
    mutate(
      region = char_reg %>% str_extract(pattern = "(?<=_{2}).+"),
      region = case_when(
        region == "us" ~ "USA",
        region == "developed" ~ "Developed Ex. USA",
        region == "emerging" ~ "Emerging"
      ),
      region = region %>% factor(levels = c("USA", "Developed Ex. USA", "Emerging"))
    ) %>%
    filter(!is.na(ret_neu_scaled)) %>%
    group_by(char_reg, region) %>%
    nest() %>%
    mutate(
      fit = data %>% map(~lm(ret_neu_scaled ~ mkt_vw_exc, data = .x)),  # They are market neutral by construction but this takes care of the degress of freedom adjustment 
      nw = fit %>% map(~ lmtest::coeftest(.x, vcov = sandwich::NeweyWest(.x)) %>% broom::tidy()),
      df = fit %>% map_dbl(~ .x$df.residual)
    ) %>%
    unnest(nw) %>%
    filter(term == "(Intercept)") %>%
    rename("p_ols" = p.value) %>%
    # group_by(region) %>%
    ungroup() %>%
    mutate(
      n = n(),
      p_bonf = p_ols %>% p.adjust(method = "bonferroni"),
      p_holm = p_ols %>% p.adjust(method = "holm"),
      p_bh = p_ols %>% p.adjust(method = "BH"),
      p_by = p_ols %>% p.adjust(method = "BY")
    ) %>%
    select(n, region, char_reg, estimate, statistic, df, "se" = std.error, starts_with("p_")) %>%
    gather(starts_with("p_"), key = "method", value = "p") %>%
    mutate(
      method = method %>% str_remove("^p_"),
      mt_adj = case_when(
        method == "ols" ~ "None",
        method == "bh" ~ "FDR",
        method == "by" ~ "FDR",
        method == "bonf" ~ "FWR",
        method == "holm" ~ "FWR"
      ),
      method = case_when(
        method == "ols" ~ "OLS",
        method == "bh" ~ "BH",
        method == "by" ~ "Benjamini-Yekutieli",
        method == "bonf" ~ "Bonferroni",
        method == "holm" ~ "Holm",
        TRUE ~ method
      ),
      method = method %>% factor(levels = c("OLS", "Bonferroni", "Holm", "BH", "Benjamini-Yekutieli", "EB - Region", "EB - Full"))
    ) 
  
  (mf_t <- many_factors_se %>% 
      group_by(method) %>%
      summarise(
        t_140 = (min(abs(statistic)[p < 0.05]) + max(abs(statistic)[p > 0.05])) / 2
      ))
  
  avg_se <- mean(many_factors_se$se)
  
  ols_ci <- tibble(
    method = c("OLS", "Bonferroni", "Benjamini-Yekutieli", "Empirical Bayes"), 
    t_1 = rep(1.96, 4)
  ) %>%
    left_join(mf_t, by = "method") %>%
    gather(t_1, t_140, key = "n_factors", value = "t") %>%
    mutate(
      n_factors = n_factors %>% str_remove("t_") %>% as.integer(),
      p025 = -t * avg_se,
      p975 = t * avg_se
    ) %>%
    filter(!(method == "Empirical Bayes" & n_factors == 140)) 
  
  eb_ci <- eb_est$us$factors %>%
    summarise(
      eb_se = mean(post_sd)
    ) %>%
    transmute(
      method = "Empirical Bayes",
      n_factors = 140,
      p025 = -1.96 * eb_se,
      p975 = 1.96 * eb_se
    )  
  
  comb_data <- bind_rows(ols_ci, eb_ci)
  
  comb_data %>%
    ggplot(aes(n_factors, colour = method, linetype = method)) +
    geom_line(aes(y = p025)) +
    geom_line(aes(y = p975)) +
    geom_ribbon(data=comb_data %>% filter(method == "Empirical Bayes"), 
                aes(x = n_factors, ymin=p025,ymax=p975), fill=colours_theme[3], alpha=0.2, inherit.aes = F) +
    labs(x = "Number of Factors", y = "Centered 95% Confidence Interval", colour = "Method", linetype = "Method") +
    scale_y_continuous(breaks=c(avg_se * 1.96, 0, -avg_se * 1.96),
                       labels=c(expression(hat(alpha) + sigma[hat(alpha)] %*% t), expression(hat(alpha)), expression(hat(alpha) - sigma[hat(alpha)] %*% t))) +
    scale_x_continuous(breaks = c(1, 140), expand=expansion(mult = c(0, 0.3), 
                                                            add = c(5, 0))) +
    geom_dl(aes(label = method, y = p975), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
}


# Plot In-Sample vs. Out of Sample
plot_is_oos <- function(ub_us, ub_dev, ub_emer) {
  is_oos_split <- ub_us$ols_regs %>% 
    mutate(region = "us") %>%
    bind_rows(
      ub_dev$ols_regs %>% mutate(region = "dev"),
      ub_emer$ols_regs %>% mutate(region = "emer")
    ) %>%
    mutate(
      region = case_when(
        region == "us" ~ "USA",
        region == "dev" ~ "Developed",
        region == "emer" ~ "Emerging"
      ),
      region = region %>% factor(levels = c("USA", "Developed", "Emerging"))
    ) %>%
    rename("characteristic" = c)
  
  cluster_plot <- is_oos_split %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig == 1) %>%
    left_join(cluster_labels, by = "characteristic") %>%
    gather(is, post_sample, key = "period", value = "estimate") %>%
    group_by(region, hcl_label, period) %>%
    summarise(
      alpha_mean = mean(estimate)
    ) %>% 
    select(region, hcl_label, period, alpha_mean) %>%
    spread(key = period, value = alpha_mean) %>%
    mutate(nudge_y = -0.015) %>%
    ggplot(aes(is, post_sample)) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = hcl_label), nudge_y = -0.015) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    # geom_smooth(method = "lm", se = F) +
    facet_wrap(~region) +
    labs(x = "Monthly Alpha (%): In-Sample", y = "Monthly Alpha (%): Post Sample")
  
  is_oos_split %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig == 1) %>%
    ggplot(aes(is, post_sample)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    geom_smooth(method = "lm", se = F, formula = "y ~ x") +
    facet_wrap(~region, ncol = 1) +
    labs(x = "Monthly Alpha (%): In-Sample", y = "Monthly Alpha (%): Post Sample")
}


plot_is_oos_factors <- function(is_oos_regions) {
  is_oos_data <- is_oos_regions %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig == 1) %>%
    mutate(
      region = case_when(
        region == "us" ~ "USA",
        region == "developed" ~ "Developed Ex. USA",
        region == "emerging" ~ "Emerging"
      ),
      region = region %>% factor(levels = c("USA", "Developed Ex. USA", "Emerging"))
    ) %>%
    select(region, characteristic, period, estimate) %>%
    spread(key = period, value = estimate) 
  
  is_oos_data %>%
    group_by(region) %>%
    nest() %>%
    mutate(
      fit = data %>% map(~lm(oos ~ is, data = .x)),
      n = fit %>% map_dbl(~length(.x$residuals)),
      tidied = fit %>% map(tidy)
    ) %>%
    unnest(tidied)
  
  is_oos_data %>% 
    ggplot(aes(is, oos)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = F, formula = "y ~ x") +
    facet_wrap(~region) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    coord_fixed() +
    labs(x = "Monthly Alpha (%): In-Sample", y = "Monthly Alpha (%): Out-of-Sample")
}

plot_tpf <- function(tpf, cluster_order, ci_low = 0.05, ci_high = 0.95) {
  orig <- tpf %>%
    filter(id == "Apparent") %>%
    select(coef) %>%
    unnest(coef) %>%
    rename("tpf_weight" = weight)
  
  bs <- tpf %>%
    filter(id != "Apparent") %>%
    unnest(coef) %>%
    group_by(term) %>%
    summarise(
      bs_mean = mean(weight),
      bs_sd = sd(weight),
      bs_se = bs_sd / sqrt(n()),
      bs_low = weight %>% quantile(ci_low),
      bs_high = weight %>% quantile(ci_high),
      bs_prob_zero = mean(weight == 0)
    ) %>%
    left_join(orig, by = "term") %>%
    mutate(bs_bias = bs_mean - tpf_weight)
  print(paste0("Clusters with significantly positive TPF weight: ", sum(filter(bs, term != "Market")$bs_low>0)))
  bs %>% 
    mutate(
      term = term %>% factor(levels = c(cluster_order, "Market"))
    ) %>%
    ggplot(aes(reorder(term, tpf_weight), tpf_weight*100, fill = term)) +
    geom_col() +
    coord_flip() +
    geom_errorbar(mapping = aes(ymin = bs_low*100, ymax = bs_high*100), width = 0.2, size = 0.2) +
    labs(y = "Weight in Tangency PF (%)") +
    theme(
      axis.title.y = element_blank(),
      legend.position = "none"
    )
}

plot_tpf_region <- function(tpf_us, tpf_dev, tpf_emer, cluster_order, ci_low = 0.05, ci_high = 0.95) {
  all <- bind_rows(tpf_us, tpf_dev, tpf_emer)
  
  orig <- all %>%
    filter(id == "Apparent") %>%
    select(market_region, coef) %>%
    unnest(coef) %>%
    rename("tpf_weight" = weight)
  
  bs <- all %>%
    filter(id != "Apparent") %>%
    unnest(coef) %>%
    group_by(market_region, term) %>%
    summarise(
      bs_mean = mean(weight),
      bs_sd = sd(weight),
      bs_se = bs_sd / sqrt(n()),
      bs_low = weight %>% quantile(ci_low),
      bs_high = weight %>% quantile(ci_high),
      bs_prob_zero = mean(weight == 0)
    ) %>%
    left_join(orig, by = c("market_region", "term")) %>%
    mutate(bs_bias = bs_mean - tpf_weight)
  
  
  bs %>% 
    group_by(term) %>% 
    mutate(
      sort_var = tpf_weight[market_region == "us"],
      region_pretty = case_when(
        market_region == "us" ~ "USA",
        market_region == "developed" ~ "Developed Ex. USA",
        market_region == "emerging" ~ "Emerging"
      ),
      region_pretty = region_pretty %>% factor(levels = c("USA", "Developed Ex. USA", "Emerging"))
    ) %>%
    mutate(term = term %>% factor(levels = c(cluster_order, "Market"))) %>%
    ggplot(aes(reorder(term, sort_var), tpf_weight*100, fill = term)) +
    geom_col() +
    geom_errorbar(mapping = aes(ymin = bs_low*100, ymax = bs_high*100), width = 0.2, size = 0.2) +
    labs(y = "Weight in Tangency PF (%)") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "none"
    ) +
    facet_wrap(~region_pretty, ncol = 1)
}

plot_tpf_size <- function(tpf_size_samples, cluster_order, ci_low = 0.05, ci_high = 0.95) {
  orig <- tpf_size_samples %>%
    filter(id == "Apparent") %>%
    select(size_grp, coef) %>%
    unnest(coef) %>%
    rename("tpf_weight" = weight)
  
  bs <- tpf_size_samples %>%
    filter(id != "Apparent") %>%
    unnest(coef) %>%
    group_by(size_grp, term) %>%
    summarise(
      bs_mean = mean(weight),
      bs_sd = sd(weight),
      bs_se = bs_sd / sqrt(n()),
      bs_low = weight %>% quantile(ci_low),
      bs_high = weight %>% quantile(ci_high),
      bs_prob_zero = mean(weight == 0)
    ) %>%
    left_join(orig, by = c("size_grp", "term")) %>%
    mutate(bs_bias = bs_mean - tpf_weight)
  
  
  bs %>% 
    group_by(term) %>% 
    mutate(
      sort_var = tpf_weight[size_grp == "mega"],
      size_grp_pretty = size_grp %>% str_to_title(),
      size_grp_pretty = size_grp_pretty %>% factor(levels = c("Mega", "Large", "Small", "Micro", "Nano"))
    ) %>%
    mutate(term = term %>% factor(levels = c(cluster_order, "Market"))) %>%
    ggplot(aes(reorder(term, sort_var), tpf_weight*100, fill = term)) +
    geom_col() +
    geom_errorbar(mapping = aes(ymin = bs_low*100, ymax = bs_high*100), width = 0.2, size = 0.2) +
    labs(y = "Weight in Tangency PF (%)") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "none"
    ) +
    facet_wrap(~size_grp_pretty, ncol = 1)
}

plot_over_time <- function(posterior_over_time, orig_sig, ols_incl, lb, bw) {
  if (orig_sig) {
    orig_sig_values <- T
  } else {
    orig_sig_values <- c(T, F)
  }
  
  all_factors <- tibble("char_reg" = rownames(posterior_over_time[[1]]$factor_mean)) %>%
    mutate(characteristic = char_reg %>% str_remove(paste0("__", ot_region))) %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>%
    mutate(
      selected_factors = (orig_sig %in% orig_sig_values) 
    )
  i <- all_factors$selected_factors
  
  full_posterior <- posterior_over_time %>% lapply(function(eb_act) {
    a <- eb_act$factor_mean[i]
    a_cov <- eb_act$factor_cov[i, i]
    n <- length(a)
    w <- rep(1/n, n)
    post_mean <- drop(t(a) %*% w)
    post_sd <- drop(sqrt(t(w) %*% a_cov %*% w))
    avg_ols <- mean(eb_act$factors$ols_est[i])
    tibble("end_date"= eb_act$end_date, n=n, post_mean, post_sd, avg_ols) 
  }) %>% bind_rows()
  
  # Black and white coloring
  if (bw) {
    col1 <- "black"
    col2 <- "grey35"
  } else {
    col1 <- colours_theme[1]
    col2 <- colours_theme[2]
  }
  
  if (ols_incl) {
    # Create OLS benchmarks
    ols_bm <- seq.Date(from = as.Date("1959-12-31"), to = settings$end_date, by = "1 year") %>% lapply(function(end_date) {
      data <- regional_pfs[region == ot_region] %>% 
        filter(characteristic %in% all_factors$characteristic[i]) %>%
        filter(eom >= settings$start_date & eom <= end_date) %>%
        eb_prepare(
          scale_alpha = settings$eb$scale_alpha, 
          overlapping = settings$eb$overlapping 
        )
      
      avg_alpha_full <- data$long %>%
        group_by(characteristic) %>%
        summarise(
          n = n(),
          alpha = mean(ret_neu_scaled)
        ) %>%
        ungroup() %>%
        summarise(
          end_date = end_date,
          type = "avg_alpha_full",
          alpha = mean(alpha)
        )
      
      avg_alpha_st <- data$long %>%
        filter(year(eom) > (year(end_date)-lb)) %>%
        group_by(characteristic) %>%
        mutate(
          ret_neu_st = (ret - cov(ret, mkt_vw_exc)/var(mkt_vw_exc) * mkt_vw_exc)*100,
          ret_neu_st = ret_neu_st / sd(ret_neu_st) * (10 / sqrt(12))
        ) %>%
        summarise(alpha = mean(ret_neu_st)) %>%
        ungroup() %>%
        summarise(
          end_date = end_date,
          type = "avg_alpha_st",
          alpha = mean(alpha)
        )
      
      alpha_avg <- data$long %>%
        group_by(eom) %>%
        summarise(
          n = n(),
          ret = mean(ret_neu_scaled),
          mkt_vw_exc = unique(mkt_vw_exc)
        ) %>%
        ungroup() %>%
        mutate(
          ret_neu = ret - cov(ret, mkt_vw_exc) / var(mkt_vw_exc) * mkt_vw_exc
        ) %>%
        summarise(
          end_date = end_date,
          type = "alpha_avg_full",
          alpha = mean(ret_neu)
        )
      
      bind_rows(avg_alpha_full, avg_alpha_st, alpha_avg)
    }) %>% bind_rows()
    
    ols_bm_wide <- ols_bm %>% spread(key = type, value = alpha)
    
    (plot_1 <- full_posterior %>%
        left_join(ols_bm_wide, by = "end_date") %>%
        ggplot(aes(end_date)) +
        geom_point(aes(y = post_mean, colour="Average Posterior Alpha", shape = "Average Posterior Alpha")) +
        geom_point(aes(y = avg_alpha_full, colour="Average OLS Alpha", shape = "Average OLS Alpha")) +
        geom_errorbar(aes(ymin = post_mean + 1.96 * post_sd, ymax = post_mean - 1.96 * post_sd)) +
        scale_colour_manual(name = "Test", values = c("Average Posterior Alpha"=col1, "Average OLS Alpha"=col2)) +
        scale_shape_manual(name = "Test", values = c("Average Posterior Alpha" = 16, "Average OLS Alpha" = 17)) +
        labs(y = "Posterior Alpha with 95% CI (%)") +
        ylim(c(0, NA)) +
        scale_x_date(breaks = seq.Date(as.Date("1960-12-31"), as.Date("2020-12-31"), by = "10 years"), date_labels = "%Y-%m") +
        theme(
          legend.title = element_blank(),
          legend.position = "top",
          axis.text.x = element_blank(),
          axis.title.x = element_blank()
        ))
    
    plot_2 <- full_posterior %>%
      left_join(ols_bm_wide, by = "end_date") %>%
      ggplot(aes(end_date, avg_alpha_st)) +
      geom_col() +
      labs(y = "5-year Rolling Alpha (%)") +
      scale_x_date(breaks = seq.Date(as.Date("1960-12-31"), as.Date("2020-12-31"), by = "10 years"), date_labels = "%Y-%m") +
      theme(
        axis.title.x = element_blank()
      )
    
    # print(full_posterior %>% left_join(ols_bm_wide, by = "end_date") %>% mutate(diff_pm = post_mean - lag(post_mean), diff_aaf = avg_alpha_full - lag(avg_alpha_full)) %>% filter(end_date != as.Date("1960-12-31")) %>% summarise(sd_pm = sd(diff_pm), sd_aaf = sd(diff_aaf)))
    
    plot <- cowplot::plot_grid(plot_1, plot_2, ncol = 1, rel_heights = c(2, 1))
  } else {
    plot <- full_posterior %>%
      ggplot(aes(end_date)) +
      geom_point(aes(y = post_mean), colour=col1, shape = 16) +
      geom_errorbar(aes(ymin = post_mean + 1.96 * post_sd, ymax = post_mean - 1.96 * post_sd)) +
      labs(y = "Posterior Alpha with 95% CI (%)") +
      ylim(c(0, NA)) +
      scale_x_date(breaks = seq.Date(as.Date("1960-12-31"), as.Date("2020-12-31"), by = "10 years"), date_labels = "%Y-%m") +
      theme(
        legend.title = element_blank(),
        legend.position = "top",
        axis.title.x = element_blank()
      )
  }
  print(full_posterior %>% mutate(ci_width = post_sd*1.96*2) %>% filter(end_date %in% c(as.Date("1960-12-31"), settings$end_date)))
  plot
}



plot_taus_over_time <- function(posterior_over_time_flex) {
  data <- posterior_over_time_flex %>% lapply(function(x) {
    x$mle %>% mutate(end_date = x$end_date)
  }) %>% 
    bind_rows() %>%
    filter(estimate != "alpha") %>%
    mutate(estimate_pretty = if_else(estimate == "tau_s", "tau_w", estimate)) 
  
  ymax <- max(data$ml_est)
  
  data %>%
    ggplot(aes(end_date, ml_est, colour = estimate_pretty, linetype = estimate_pretty)) +
    geom_line() +
    scale_linetype_manual(values = c('tau_c' = "solid", 'tau_w' = "longdash"), name = '', labels = c(expression(tau[c]), expression(tau[w]))) +
    scale_colour_manual(values = c('tau_c' = colours_theme[1], 'tau_w' = colours_theme[2]), name = '', labels = c(expression(tau[c]), expression(tau[w]))) +
    scale_x_date(breaks = seq.Date(as.Date("1960-12-31"), as.Date("2020-12-31"), by = "10 years"), date_labels = "%Y-%m") +
    labs(y = "Maximum Likelihood Estimate (%)") +
    ylim(c(0, ymax)) +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.title.x = element_blank()
    )
}

plot_sim_fdr <- function(simulation) {
  tau_w_names <- c(
    `0.01` = expression(tau[w] ~ "= 0.01%"),
    `0.2` = expression(tau[w] ~ "= 0.20%")
  )
  stat_labels <- c(
    `False Discovery Rate` = expression(~"False Discovery Rate"),
    `True Discovery Rate` = expression(~"True Discovery Rate"),
    `True Discoveries` = expression(~"True Discoveries"),
    `False Discoveries` = expression(~"False Discoveries")
  )
  
  plot_data <- simulation %>%
    gather(n_disc, fdr, true_disc, false_disc, true_disc_rate, key = "stat", value = "number") %>%
    filter(stat %in% c("fdr", "true_disc", "false_disc", "true_disc_rate")) %>%
    mutate(
      stat = case_when(
        stat == "fdr" ~ "False Discovery Rate",
        stat == "true_disc" ~ "True Discoveries",
        stat == "false_disc" ~ "False Discoveries",
        stat == "true_disc_rate" ~ "True Discovery Rate"
      ),
      stat = stat %>% factor(levels = c("False Discovery Rate", "True Discovery Rate", "True Discoveries", "False Discoveries")),
      type = case_when(
        type == "by" ~ "Benjamini and Yekutieli",
        type == "ols" ~ "OLS",
        type == "eb" ~ "Empirical Bayes"
      ),
      type = type %>% factor(levels = c("OLS", "Benjamini and Yekutieli", "Empirical Bayes")),
      # tau_w_title = formatC(tau_w, digits = 2, format = "f"),
      # tau_w_title = as.character(eval(bquote(tau[w] ~ "=" ~ .(tau_w_title)~ "%")))
      tau_w_title = tau_w %>% factor(labels = tau_w_names),
      stat_title = stat %>% factor(label = stat_labels)
    ) 
  
  fdr_plot <- plot_data %>% 
    filter(stat == "False Discovery Rate") %>%
    ggplot(aes(tau_c, number, colour = type)) +
    geom_point() +
    geom_line() +
    labs(x = "tau_c (%)", y = "False Discovery Rate", colour = "Adjustment") +
    facet_wrap(stat~tau_w_title, labeller = label_bquote(tau[w] ~ "=" ~ .(tau_w_title)~ "%"))
  
  true_disc_rate <- plot_data %>% 
    filter(stat == "True Discovery Rate") %>%
    ggplot(aes(tau_c, number, colour = type)) +
    geom_point() +
    geom_line() +
    labs(x = "tau_c (%)", y = "True Discovery Rate", colour = "Adjustment") +
    facet_wrap(stat~tau_w_title, ncol = 2) 
  
  plot_data %>%
    filter(stat %in% c("False Discovery Rate", "True Discovery Rate")) %>%
    group_by(stat) %>%
    mutate(scale_max = max(number)) %>%
    mutate(scale_min = min(number)) %>%
    ggplot(aes(tau_c, number, colour = type, shape = type)) +
    geom_point() +
    geom_point(aes(y = scale_max), alpha = 0) +
    geom_point(aes(y = scale_min), alpha = 0) +
    geom_line() +
    labs(x = bquote(bold(tau[c])~"(%)"), colour = "Type:", linetype = "Type:", shape = "Type:") +
    facet_wrap(stat_title~tau_w_title, scales = "free_y", labeller = label_parsed) +
    # facet_wrap(stat_title~tau_w_title) +
    # facet_wrap(stat~tau_w_title, scales = "free_y", labeller = label_bquote(tau[w] ~ "=" ~ .(tau_w_title)~ "%")) +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 12),
      legend.position = "top"
    )
}


plot_size_overall <- function(eb_size, flipped = F, text = F) {
  size_repl <- eb_size %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig == 1) %>%
    group_by(size_grp) %>%
    summarise(
      repl_rate = mean(p025>0)
    )
  if (flipped) {
    size_plot <- size_repl %>%
      mutate(size_grp = size_grp %>% factor(levels = c("Nano", "Micro", "Small", "Large", "Mega"))) %>%
      ggplot(aes(x = size_grp, y = repl_rate*100)) +
      geom_col(fill = colours_theme[1]) +
      coord_flip() +
      labs(y = "Replication Rate (%)") +
      theme(axis.title.y = element_blank())
    if (text) {
      size_plot <- size_plot + geom_text(aes(label = str_c(formatC(round(repl_rate * 100, 1), digits = 1, format = "f"), "%")), nudge_y = 7, size = 5.5)
    }
  } else {
    size_plot <- size_repl %>%
      ggplot(aes(x = size_grp, y = repl_rate*100)) +
      geom_col(fill = colours_theme[1]) +
      labs(y = "Replication Rate (%)") +
      theme(axis.title.x = element_blank())
    if (text) {
      size_plot <- size_plot + geom_text(aes(label = str_c(formatC(round(repl_rate * 100, 1), digits = 1, format = "f"), "%")), nudge_y = 2.5, size = 4)
    }
  }
  size_plot
}

plot_size_clusters <- function(eb_size, cluster_order) {
  overall <- eb_size %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig == 1) %>%
    group_by(size_grp) %>%
    summarise(
      overall_rr = mean(p025>0)
    )
  
  cluster_rr <- eb_size %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    filter(orig_sig == 1) %>%
    mutate(hcl_label = hcl_label %>% factor(levels = cluster_order)) %>%
    group_by(size_grp, hcl_label) %>%
    summarise(
      repl_rate = mean(p025>0)
    ) %>%
    group_by(hcl_label) %>%
    mutate(sort_var = repl_rate[size_grp == "Mega"]) %>%
    left_join(overall, by = "size_grp") %>%
    mutate(size_title = str_c(size_grp, " - Replication Rate: ", formatC(round(overall_rr * 100, 1), digits = 1, format = "f"), "%"))
  
  titles <- cluster_rr$size_title %>% unique()
  title_order <- c(titles[str_detect(titles, "Mega")], titles[str_detect(titles, "Large")], titles[str_detect(titles, "Small")], 
                   titles[str_detect(titles, "Micro")], titles[str_detect(titles, "Nano")])
  
  cluster_rr %>%
    mutate(size_title = size_title %>% factor(levels = title_order)) %>%
    ggplot(aes(x = reorder(hcl_label, sort_var), y = repl_rate*100, fill = hcl_label)) +
    geom_col() +
    labs(y = "Replication Rate (%)") +
    facet_wrap(~size_title, ncol = 1) +
    theme(
      axis.title.x = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10)
    )
}

plot_sign_test <- function(sign_test) {
  sig <- sign_test %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>% 
    mutate(
      type = case_when(
        p <= 0.05 & orig_sig == 1 ~ "Replicated",
        p > 0.05 & orig_sig == 1 ~ "Not Replicated",
        orig_sig == 0 ~ "Never Significant"
      ),
      type = type %>% factor(levels = c("Replicated", "Not Replicated", "Never Significant"))      
      # p_value = if_else(p < 0.05, "Significant", "Insignificant"),
      # p_value = p_value %>% factor(levels = c("Significant", "Insignificant"))
    ) 
  
  sig_overall <- sig %>% 
    summarise(repl_rate = sum(type == "Replicated") / sum(type %in% c("Replicated", "Not Replicated"))) %>% 
    pull(repl_rate)
  
  plot_sign_factors <- sig %>%
    ggplot(aes(reorder(characteristic, pos_act), pos_act*100, fill = type)) +
    geom_col() +
    ylim(c(0, 100)) +
    labs(y = "Countries with Positive Return (%)", fill = "Bootstrapped p-Value:") +
    geom_text(aes(x = 18, y = 100, label = str_c("Replication Rate: ", round(sig_overall*100, 2), "%")), inherit.aes = F) +
    # geom_text(aes(label = round(p, 2)), nudge_y = 1, size = 1.5) +
    theme(
      axis.title.x = element_blank(),
      # legend.title = element_blank(),
      legend.position = "top",
      axis.text.x = element_text(size = 7, angle = 90, vjust = 0, hjust = 1),
      text = element_text(size = 10)
    )
  
  plot_pos <- sig %>%
    left_join(cluster_labels, by = "characteristic") %>% 
    mutate(hcl_label = hcl_label %>% factor(levels = cluster_order)) %>%
    group_by(hcl_label) %>%
    filter(type %in% c("Replicated", "Not Replicated")) %>%
    summarise(
      repl_rate = sum(type == "Replicated") / sum(type %in% c("Replicated", "Not Replicated")),
      pos = mean(pos_act)
    ) %>% 
    ggplot(aes(reorder(hcl_label, pos), pos*100, fill = hcl_label)) +
    geom_col() +
    labs(y = "Countries with Positive Alpha (%)") +
    ylim((c(0, 100))) +
    theme(
      legend.position = "none",
      axis.title.y = element_text(size=8),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
  
  plot_sig <- sig %>%
    left_join(cluster_labels, by = "characteristic") %>% 
    mutate(hcl_label = hcl_label %>% factor(levels = cluster_order)) %>%
    group_by(hcl_label) %>% 
    summarise(repl_rate = sum(type == "Replicated") / sum(type %in% c("Replicated", "Not Replicated"))) %>% 
    ggplot(aes(reorder(hcl_label, repl_rate), repl_rate*100, fill = hcl_label)) +
    geom_col() +
    labs(y = "Sign Test Replication Rate (%)") +
    theme(
      legend.position = "none",
      axis.title.y = element_text(size=8),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
  plot_sign_clusters <- cowplot::plot_grid(plot_pos, plot_sig, ncol = 1, labels = c("A", "B"), label_y = 1, label_x = 0)
  list("factors" = plot_sign_factors, "clusters" = plot_sign_clusters)
}

# World ex us versus us
plot_int_cor <- function(eb_us, eb_world_ex_us) {
  cor_data <- eb_us$input$long %>%
    bind_rows(eb_world_ex_us$input$long) %>%
    select(characteristic, region, eom, ret) %>%
    spread(key = region, value = ret) %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>%
    filter(!is.na(us) & !is.na(world_ex_us)) %>%
    group_by(characteristic) %>%
    summarise(
      monhts = n(),
      cor = cor(us, world_ex_us)
    ) 
  
  print(cor_data %>%
          pull(cor) %>%
          quantile())
  
  cor_data %>%
    left_join(cluster_labels, by = "characteristic") %>%
    group_by(hcl_label) %>%
    summarise(
      cor_avg = mean(cor)
    ) %>%
    ggplot(aes(reorder(hcl_label, cor_avg), cor_avg)) +
    geom_col(fill = colours_theme[1]) +
    labs(y = "Correlation of US and World ex. US factor (Avg. within Cluster)") +
    coord_flip() +
    theme(
      axis.title.y = element_blank()
    )
}

plot_world_vs_us <- function(eb_us, eb_world_ex_us) {
  cor_data <- eb_us$input$long %>%
    bind_rows(eb_world_ex_us$input$long) %>%
    select(characteristic, region, eom, ret) %>%
    spread(key = region, value = ret) %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>%
    filter(!is.na(us) & !is.na(world_ex_us)) %>%
    group_by(characteristic) %>%
    summarise(
      monhts = n(),
      cor = cor(us, world_ex_us)
    ) 
  
  print(cor_data %>%
          pull(cor) %>%
          quantile())
  
  
  
  region_data <- eb_us$factors %>%
    bind_rows(eb_world_ex_us$factors) %>%
    select(characteristic, region, ols_est) %>%
    spread(key = region, value = ols_est) %>%
    left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") 
  
  max_scale <- max(c(region_data$world_ex_us, region_data$us))
  min_scale <- min(c(region_data$world_ex_us, region_data$us))
  
  fit_all <- lm(world_ex_us ~ us, data = region_data)
  int <- fit_all$coefficients[1]
  slp <- fit_all$coefficients[2]
  r2 <- summary(fit_all)$r.squared
  
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(int), digits = 2),
                        b = format(unname(slp), digits = 2),
                        r2 = format(r2, digits = 3)))
  lbl <- as.character(as.expression(eq))
  
  t_int <- formatC(round(summary(fit_all)$coefficients["(Intercept)", "t value"], 2), format='f', digits=2)
  t_us <- formatC(round(summary(fit_all)$coefficients["us", "t value"], 2), format='f', digits=2)
  
  region_data %>%
    mutate(
      orig_sig_pretty = if_else(orig_sig == 1, "Studied", "Not Studied"),
      orig_sig_pretty = orig_sig_pretty %>% factor(levels = c("Studied", "Not Studied"))
    ) %>%
    ggplot(aes(us, world_ex_us, colour = orig_sig_pretty, shape = orig_sig_pretty)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    # geom_abline(intercept = int, slope = slp) +
    xlim(c(min_scale, max_scale)) +
    ylim(c(min_scale, max_scale)) +
    annotate("text", label=lbl, parse=TRUE, x=min_scale, y=max_scale, hjust = 0) +
    annotate("text", label=paste0("        (", t_int, ")   (", t_us, ")"), parse=F, x=min_scale, y=max_scale-0.07, hjust = 0, size = 3.3) +
    labs(x = "US Alpha (%)", y = "World Ex. US Alpha (%)") + 
    theme(
      legend.position = "none",
      legend.title = element_blank()
    )
}

plot_is_oos_post <- function(is_oos, type) { # type in c("GLS", "OLS")
  plot_list <- c("pre", "post", "pre_post") %>% lapply(function(period) {
    data <- is_oos[[period]]$regs
    if (type == "OLS") {
      fit <- lm(oos ~ is, data = data)
      # Output
      int <- unname(fit$coefficients[1])
      int_se <- summary(fit)$coefficients[1, "Std. Error"]
      slope <- unname(fit$coefficients[2])
      slope_se <- summary(fit)$coefficients[2, "Std. Error"]
      r2 <- summary(fit)$adj.r.squared # Adjusted R2
      # Label
      eq_lbl <- substitute(
        italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,
        list(
          a = formatC(int, digits = 2, format = "f"),
          b = formatC(slope, digits = 2, format = "f"),
          r2 = formatC(r2, digits = 2, format = "f"))
      )
    }
    if (type == "GLS") {
      x <- cbind(rep(1, nrow(data)), data$is)
      y <- data$oos
      chars <- str_c(data$characteristic, "__us")
      
      gls_cov <- eb_est$us$factor_cov[chars, chars]
      gls_est <- solve(t(x) %*% solve(gls_cov) %*% x) %*% t(x) %*% solve(gls_cov) %*% y
      gls_res <- y - x %*% gls_est
      gls_e_var <- 1/(nrow(x)-ncol(x)) * t(gls_res) %*% solve(gls_cov) %*% (gls_res)
      gls_se <- sqrt(diag(drop(gls_e_var)* solve(t(x) %*% solve(gls_cov) %*% x)))
      # Output
      int <- gls_est[1, 1]
      int_se <- gls_se[1]
      slope <- gls_est[2, 1]
      slope_se <- gls_se[2]
      # Label (R2 doesn't really translate to GLS, because the mean prediction is no longer a good baseline)
      eq_lbl <- substitute(
        italic(y) == a + b %.% italic(x),
        list(
          a = formatC(int, digits = 2, format = "f"),
          b = formatC(slope, digits = 2, format = "f"))
      )
    }
    
    min_y <- min(data$is, data$oos)
    max_y <- max(data$is, data$oos)
    
    t_int <- formatC(round(int/int_se, 2), format='f', digits=2)
    t_is <- formatC(round(slope/slope_se, 2), format='f', digits=2)
    t_lbl <- paste0("       (", t_int, ")   (", t_is, ")")
    
    data %>%
        ggplot(aes(is, oos)) +
        geom_point(colour = colours_theme[1]) +
        ylim(c(min_y, max_y)) +
        xlim(c(min_y, max_y)) +
        geom_hline(yintercept = 0, linetype = "solid") +
        geom_vline(xintercept = 0, linetype = "solid") +
        geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
        ggtitle(label = eq_lbl, subtitle = t_lbl) +
        labs(x = "In-Sample", y = "Out-of-Sample")
  })
  plot_list
}

plot_is_oos_post_quad <- function(is_oos, type) { # type in c("OLS", "GLS")
  plot_list <- c("pre", "post", "pre_post") %>% lapply(function(period) {
    data <- is_oos[[period]]$regs
    if (type == "OLS") {
      fit <- lm(oos ~ is + I(is^2), data = data)
      # Output
      int <- unname(fit$coefficients[1])
      int_se <- summary(fit)$coefficients[1, "Std. Error"]
      is <- unname(fit$coefficients[2])
      is_se <- summary(fit)$coefficients[2, "Std. Error"]
      issq <- unname(fit$coefficients[3])
      issq_se <- summary(fit)$coefficients[3, "Std. Error"]
      r2 <- summary(fit)$adj.r.squared # Adjusted R2
      # Label 
      eq_lbl <- substitute(
        italic(y) == a + b %.% italic(x)* ~ s ~ c %.% italic(x)^2*","~~italic(R)^2~"="~r2,
        list(
          a = formatC(int, digits = 2, format = "f"),
          b = formatC(is, digits = 2, format = "f"),
          s = ifelse(sign(issq)==1, "+", "-"),
          c = formatC(unname(abs(issq)), digits = 2, format = "f"),
          r2 = formatC(r2, digits = 2, format = "f"))
      )
    }
    if (type == "GLS") {
      x <- cbind(rep(1, nrow(data)), data$is, data$is^2)
      y <- data$oos
      chars <- str_c(data$characteristic, "__us")
      
      gls_cov <- eb_est$us$factor_cov[chars, chars]
      gls_est <- solve(t(x) %*% solve(gls_cov) %*% x) %*% t(x) %*% solve(gls_cov) %*% y
      gls_res <- y - x %*% gls_est
      gls_e_var <- 1/(nrow(x)-ncol(x)) * t(gls_res) %*% solve(gls_cov) %*% (gls_res)
      gls_se <- sqrt(diag(drop(gls_e_var)* solve(t(x) %*% solve(gls_cov) %*% x)))
      # Output
      int <- gls_est[1, 1]
      int_se <- gls_se[1]
      is <- gls_est[2, 1]
      is_se <- gls_se[2]
      issq <- gls_est[3, 1]
      issq_se <- gls_se[3]
      # Label
      eq_lbl <- substitute(
        italic(y) == a + b %.% italic(x)* ~ s ~ c %.% italic(x)^2,
        list(
          a = formatC(int, digits = 2, format = "f"),
          b = formatC(is, digits = 2, format = "f"),
          s = ifelse(sign(issq)==1, "+", "-"),
          c = formatC(unname(abs(issq)), digits = 2, format = "f"))
      )
    }
    
    min_y <- min(data$is, data$oos)
    max_y <- max(data$is, data$oos)
    
    t_int <- formatC(round(int/int_se, 2), format='f', digits=2)
    t_is <- formatC(round(is/is_se, 2), format='f', digits=2)
    t_issq <- formatC(round(issq/issq_se, 2), format='f', digits=2)
    t_lbl <- paste0("      (", t_int, ")    (", t_is, ")         (", t_issq, ")")
    
    data %>%
      ggplot(aes(is, oos)) +
      geom_point(colour = colours_theme[1]) +
      ylim(c(min_y, max_y)) +
      xlim(c(min_y, max_y)) +
      geom_hline(yintercept = 0, linetype = "solid") +
      geom_vline(xintercept = 0, linetype = "solid") +
      geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
      geom_smooth(method = "loess", span = 1, formula = "y~x") +
      ggtitle(label = eq_lbl, subtitle = t_lbl) +
      labs(x = "In-Sample", y = "Out-of-Sample")
  })
  plot_list
}

# Effect Size Plot
plot_effects <- function(type, orig_sig, cluster_order) {  # type in c("ols", "eb")
  if (orig_sig) {
    orig_sig_values <- T
  } else {
    orig_sig_values <- c(T, F)
  }
  if (type == "ols") {
    alpha_est = "ols_est"
  }
  if (type == "eb") {
    alpha_est = "post_mean"
  }
  (effect_world <- eb_est$world$factors %>%
      left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>%
      filter(orig_sig %in% orig_sig_values) %>%
      mutate(hcl_label = hcl_label %>% factor(levels = cluster_order)) %>%
      group_by(hcl_label) %>%
      summarise(mean_alpha = mean(get(alpha_est))) %>%
      ggplot(aes(reorder(hcl_label, mean_alpha), mean_alpha, fill = hcl_label)) + 
      geom_col() +
      coord_flip() +
      labs(y = paste(str_to_upper(type), "Alpha Estimate (%)")) +
      theme(
        axis.title.y = element_blank(),
        legend.position = "none"
      ))
  
  (effect_regions <- eb_est$all$factors %>%
      left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>%
      filter(orig_sig %in% orig_sig_values) %>%
      mutate(hcl_label = hcl_label %>% factor(levels = cluster_order)) %>%
      group_by(region, hcl_label) %>%
      summarise(mean_alpha = mean(get(alpha_est))) %>%
      group_by(hcl_label) %>%
      mutate(
        sort_var = mean_alpha[region == "us"],
        region_pretty = case_when(
          region == "us" ~ "US",
          region == "developed" ~ "Developed",
          region == "emerging" ~ "Emerging"
        ),
        region_pretty = region_pretty %>% factor(levels = c("US", "Developed", "Emerging"))
      ) %>%
      ggplot(aes(reorder(hcl_label, sort_var), mean_alpha, fill = hcl_label)) + 
      geom_col() +
      coord_flip() +
      scale_y_continuous(breaks = seq(-0.2, 1, 0.2)) +
      facet_wrap(~region_pretty, scales = "free_x") +
      labs(y = paste("Average", str_to_upper(type) ,"Alpha (%)")) +
      theme(
        axis.title.y = element_blank(),
        legend.position = "none"
      ))
  
  (effect_size <- eb_us_size %>%
      left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>%
      filter(orig_sig %in% orig_sig_values) %>%
      mutate(hcl_label = hcl_label %>% factor(levels = cluster_order)) %>%
      group_by(size_grp, hcl_label) %>%
      summarise(mean_alpha = mean(get(alpha_est))) %>%
      group_by(hcl_label) %>%
      mutate(sort_var = mean_alpha[size_grp == "Mega"]) %>%
      ggplot(aes(reorder(hcl_label, sort_var), mean_alpha, fill = hcl_label)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(breaks = seq(0, 1.5, 0.50)) +
      facet_wrap(~size_grp, nrow = 1, scales = "free_x") +
      labs(y = paste("Average", str_to_upper(type) ,"Alpha (%)")) +
      theme(
        axis.title.y = element_blank(),
        legend.position = "none"
      ))
  
  list(effect_world, effect_regions, effect_size)
}

# Replication Rate by Cluster
plot_repl_cluster <- function(eb_factors, orig_sig, cluster_order) {
  if (orig_sig) {
    factor_subset <- eb_factors %>%
      left_join(char_info %>% select(characteristic, significance), by = "characteristic") %>%
      filter(significance == T)
  } else {
    factor_subset <- copy(eb_factors)
  }
  if (uniqueN(factor_subset$region) != 1) {
    warning("!!!MULTIPLE REGIONS INCLUDED!!!")
  }
  
  factor_subset %>%
    mutate(hcl_label = hcl_label %>% factor(levels = cluster_order)) %>%
    group_by(hcl_label) %>%
    summarise(
      n = n(),
      repl_rate = mean(p025 > 0),
      sort_var = repl_rate + n / 100000
    )  %>%
    ggplot(aes(reorder(hcl_label, sort_var), repl_rate * 100, fill = hcl_label)) +
    geom_col() +
    labs(y = "Replication Rate (%)") +
    coord_flip() +
    theme(
      axis.title.y = element_blank(),
      legend.position = "none"
    )
}

# Plot Replication Rate as a Function of Tau - Benchmark against Harvey et al. (2016)
plot_harvey <- function(harvey_base_res, harvey_worst_res, tau_ws, act_rr) {
  mle_est_base <- harvey_base_res$sim[[1]] %>% 
    lapply(function(x) x$mle) %>% 
    bind_rows() %>%
    mutate(type = "baseline")
  
  mle_est_worst <- harvey_worst_res$sim[[1]] %>% 
    lapply(function(x) x$mle) %>% 
    bind_rows() %>%
    mutate(type = "worst_case")
  
  mle_est <- mle_est_base %>% bind_rows(mle_est_worst)
  
  mle_summary <- mle_est %>%
    group_by(type, coef) %>%
    summarise(
      n = n(),
      coef_mean = mean(mle)
    )
  
  tc_harvey_base <- mle_summary %>% filter(type == "baseline" & coef == "tc") %>% pull(coef_mean)
  tc_harvey_worst <- mle_summary %>% filter(type == "worst_case" & coef == "tc") %>% pull(coef_mean)
  
  # Replication Rate under alternative Tau's
  m <- eb_est$us$factors %>%
    select(characteristic, ols_est, hcl_label) %>%
    mutate(cm = 1) %>%
    select(characteristic, hcl_label, cm) %>%
    spread(key = hcl_label, value = cm) %>% 
    select(-characteristic) %>% 
    as.matrix()
  m[is.na(m)] <- 0
  mm <- m %*% t(m)
  
  alpha_hat <- eb_est$us$factors %>% pull(ols_est)
  alpha_0_vec <- rep(0, 153)
  sigma <- eb_est$us$sigma
  
  tc_act <- eb_est$us$mle %>% filter(estimate == "tau_c") %>% pull(ml_est) %>% round(2)
  
  search_grid <- expand.grid(
    tau_c = c(seq(0.15, 0.46, by = 0.01), tc_act, tc_harvey_base, tc_harvey_worst),
    tau_w = tau_ws
  )
  
  repl_by_tau <- 1:nrow(search_grid) %>% lapply(function(i) {
    tw <- search_grid[i, "tau_w"]
    tc <- search_grid[i, "tau_c"]
    omega <- diag(153) * tw^2 + mm * tc^2
    post_cov <- solve(solve(omega) + solve(sigma))
    post_alpha <- post_cov %*% (solve(omega) %*% alpha_0_vec + solve(sigma) %*% alpha_hat)
    
    tibble(characteristic = eb_est$us$factors$characteristic, post_mean = drop(post_alpha), post_sd = sqrt(diag(post_cov))) %>%
      left_join(char_info %>% select(characteristic, "orig_sig" = significance), by = "characteristic") %>%
      filter(orig_sig == T) %>%
      summarise(repl_rate = mean(post_mean - 1.96 * post_sd > 0)) %>%
      mutate(tau_c = tc, tau_w = tw)
  }) %>% bind_rows()
  
  # Set TW labels 
  n_tw <- length(tau_ws) 
  tau_w_names <- vector(mode = "expression", length = n_tw)
  for (i in 1:n_tw) {
    tau_w_names[i] <- c(bquote(bold(tau[w])  ~ "=" ~ .(unname(tau_ws[i])) ~ "%"))
  }
  names(tau_w_names) <- tau_ws
  
  # Generate Important Points
  tau_points <- repl_by_tau %>%
    filter(tau_c %in% c(tc_act, tc_harvey_base, tc_harvey_worst)) %>%
    distinct() %>%
    mutate(
      tau_w_title = tau_w %>% factor(labels = tau_w_names),
      type = case_when(
        tau_c == tc_act ~ "Estimated from Data",
        tau_c == tc_harvey_base ~ "Harvey, Liu, and Zhu (2016): Baseline",
        tau_c == tc_harvey_worst ~ "Harvey, Liu, and Zhu (2016): Conservative",
        TRUE ~ "Other"
      ),
      type = type %>% factor(levels = c("Harvey, Liu, and Zhu (2016): Conservative", "Harvey, Liu, and Zhu (2016): Baseline", "Estimated from Data"))
    )
  print(tau_points)
  
  plot <- repl_by_tau %>%
    mutate(
      tau_w_title = tau_w %>% factor(labels = tau_w_names),
      type = case_when(
        tau_c == tc_act ~ "Estimated from Data",
        tau_c == tc_harvey_base ~ "Harvey, Liu, and Zhu (2016): Baseline",
        tau_c == tc_harvey_worst ~ "Harvey, Liu, and Zhu (2016): Conservative",
        TRUE ~ " "
      ),
      type = type %>% factor(levels = c("Estimated from Data", "Harvey, Liu, and Zhu (2016): Baseline", "Harvey et al. (2016): Conservative", " "))
    ) %>%
    ggplot(aes(tau_c, repl_rate * 100)) +
    geom_point(data = tau_points, aes(colour = type, shape = type, stroke = 1), size = 3) +
    geom_line(alpha = 1, size = 0.6) +
    geom_hline(yintercept = act_rr*100, linetype = "dotted") +
    scale_x_continuous(breaks = seq(0.05, max(search_grid$tau_c), 0.05)) +
    ylim(c(0, 100)) +
    theme(legend.title = element_blank(), legend.position = "top") +
    labs(y = "Replication Rate (%)", x = bquote(bold(tau[c])~"(%)"), colour = expression(tau[c]), shape = expression(tau[c]))
  if (n_tw > 1) {
    plot <- plot + facet_wrap(~tau_w_title, labeller = label_parsed)
  }
  return(plot)
}

# Single Factor TPF --
# Plot TPF Factor: Cluster + Market
plot_tpf_one_cluster <- function(data_wide, cluster_labels, s) {
  mkt_sr <- mean(data_wide$market)/sd(data_wide$market)
  one_cluster <- unique(cluster_labels$hcl_label) %>% lapply(function(c) {
    cl_chars <- cluster_labels %>%
      filter(characteristic %in% colnames(data_wide) & hcl_label == c) %>%
      pull(characteristic)
    # Cluster SR
    cl <- data_wide %>% select(market, all_of(cl_chars))
    w <- cl %>% epo_tpf(s = s)  
    sr_all <- cl %>% sr_func(w = w)
    # Average SR
    sr_single_avg <- cl_chars %>% sapply(function(char) {
      cl_sub <- data_wide %>% select(market, all_of(char))
      w <- cl_sub %>% epo_tpf(s = opt_s)
      cl_sub %>% sr_func(w = w)
    }) %>%
      mean()
    
    tibble(hcl_label=c, sr_all=sr_all, sr_single_avg=sr_single_avg)
  }) %>%
    bind_rows()
  
  one_cluster %>%
    mutate(hcl_label = hcl_label %>% factor(levels = cluster_order)) %>%
    ggplot(aes(reorder(hcl_label, sr_all), sr_all, fill = hcl_label)) +
    geom_col() +
    coord_flip() +
    geom_hline(yintercept = mkt_sr, linetype = "dashed") +
    theme(legend.position = "none", axis.title.y = element_blank()) +
    labs(y = "Monthly Sharpe Ratio: Market + Cluster")
}

# Plot TPF Factor: Exclude one cluster
plot_tpf_excl_cl <- function(data_wide, cluster_labels, s) {
  epo_w <- data_wide %>% epo_tpf(s = s)
  full_sr <- data_wide %>% sr_func(w = epo_w)
  
  excl_one <- c("Market", unique(cluster_labels$hcl_label)) %>% lapply(function(c) {
    cl_chars <- cluster_labels %>%
      filter(characteristic %in% colnames(data_wide) & hcl_label != c) %>%
      pull(characteristic)
    if (c != "Market") {
      cl_chars <- c(cl_chars, "market")
    }
    # All minus Cluster SR
    data <- data_wide %>% select(all_of(cl_chars))
    w <- data %>% epo_tpf(s = s)  
    sr <- data %>% sr_func(w = w)
    
    tibble(hcl_label=c, sr=sr)
  }) %>%
    bind_rows()
  
  excl_one %>%
    mutate(hcl_label = hcl_label %>% factor(levels = c(cluster_order, "Market"))) %>%
    ggplot(aes(reorder(hcl_label, -sr), (full_sr-sr) / full_sr, fill = hcl_label)) +
    geom_col() +
    coord_flip() +
    theme(legend.position = "none", axis.title.y = element_blank()) +
    labs(y = "Percentage Drop in Monthly SR from Excluding Cluster")
}

# Plot TPF Factor: Single Factor Importance
plot_tpf_factor_imp <- function(data_wide, cluster_labels, s) {
  epo_w <- data_wide %>% epo_tpf(s = s)
  full_sr <- data_wide %>% sr_func(w = epo_w)
  each_factor <- colnames(data_wide) %>% lapply(function(c) {
    # SR excluding char
    data <- data_wide %>% select(-all_of(c))
    w <- data %>% epo_tpf(s = opt_s)  
    sr <- data %>% sr_func(w = w)
    # Output
    tibble(characteristic = c, sr = sr)
  }) %>%
    bind_rows()
  
  each_factor %>%
    left_join(cluster_labels, by = "characteristic") %>%
    mutate(
      hcl_label = if_else(characteristic == "market", "Market", hcl_label),
      hcl_label = hcl_label %>% factor(levels = c(cluster_order, "Market")),
      drop = full_sr - sr,
      drop_prop = drop/full_sr
    ) %>%
    arrange(drop_prop) %>%
    tail(10) %>%
    ggplot(aes(reorder(characteristic, drop_prop), drop_prop*100, fill = hcl_label)) +
    geom_col() +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    labs(y = "Drop in TPF SR (% of full)", fill = "Cluster")
  
  each_factor %>%
    left_join(cluster_labels, by = "characteristic") %>%
    mutate(
      hcl_label = if_else(characteristic == "market", "Market", hcl_label),
      hcl_label = hcl_label %>% factor(levels = c(cluster_order, "Market")),
      drop = full_sr - sr,
      drop_prop = drop/full_sr,
      rank = frank(-drop_prop)
    ) %>%
    group_by(hcl_label) %>%
    filter(characteristic != "market") %>%
    # filter(drop_prop == max(drop_prop)) %>%
    ggplot(aes(reorder(characteristic, drop_prop), drop_prop*100, fill = hcl_label)) +
    geom_col() +
    coord_flip() +
    theme(axis.title.y = element_blank(), legend.position = "none") +
    labs(y = "Drop in TPF SR (% of full)", fill = "Cluster")+
    facet_wrap(~hcl_label, scales = "free_y")
}

# Plot TPF Factor: Single Factor Importance within Cluster
plot_tpf_factor_imp_cluster <- function(data_wide, cluster_labels, s) {
  within_cluster <- unique(cluster_labels$hcl_label) %>% lapply(function(c) {
    cl_chars <- cluster_labels %>%
      filter(characteristic %in% colnames(data_wide) & hcl_label == c) %>%
      pull(characteristic)
    # Full Cluster tpf
    data <- data_wide %>% select(market, all_of(cl_chars))
    w_all <- data %>% epo_tpf(s = opt_s)  
    sr_all <- data %>% sr_func(w = w_all)
    # Individual
    sr_chars <- cl_chars %>% lapply(function(char) {
      sub <- data %>% select(-all_of(char))
      w_sub <- sub %>% epo_tpf(s = s)  
      sr_sub <- sub %>% sr_func(w = w_sub)
      tibble(excl_char = char, sr = sr_sub)
    }) %>% bind_rows()
    
    sr_chars %>% mutate(hcl_label = c, sr_all = sr_all)
  }) %>%
    bind_rows()
  
  within_cluster %>%
      mutate(
        drop = sr_all - sr,
        drop_prop = drop/sr_all,
        hcl_label = hcl_label %>% factor(levels = cluster_order)
      ) %>%
      group_by(hcl_label) %>%
      # filter(drop == max(drop)) %>%
      ggplot(aes(reorder(excl_char, drop_prop), drop_prop*100, fill = hcl_label)) +
      geom_col() +
      coord_flip() +
      theme(axis.title.y = element_blank(), legend.position = "none") +
      labs(y = "Drop in cluster TPF SR (% of full cluster)", fill = "Cluster") +
      facet_wrap(~hcl_label, scales = "free")
}

# Plot TPF Factor: The Evolution of the TPF
plot_tpf_evolution <- function(data, data_wide, char_info, orig_sig_values, s) {
  mkt_sr <- mean(data_wide$market) / sd(data_wide$market)
  
  years <- data %>% 
    filter(year(eom) > min(char_info$sample_end)) %>% 
    mutate(year = year(eom)) %>%
    pull(year) %>%
    unique()
  
  sr_over_time <- years %>% lapply(function(y) {
    discovered_chars <- char_info %>% filter(sample_end <= y & significance %in% orig_sig_values) %>% pull(characteristic)
    sub <- data_wide %>% select(all_of(discovered_chars), "market")  
    w <- sub %>% epo_tpf(s = s) 
    # tibble(w = w, char = colnames(sub)) %>% left_join(char_info %>% select("char"=characteristic, sample_end), by = "char") %>% arrange(-w)
    # 2002: seasonality coincides with noa_at that also gets a large weight
    
    tibble(year = y, tpf_sr = sr_func(sub, w), n = ncol(sub))  
  }) %>% bind_rows()
  
  sr_over_time <- sr_over_time %>%
    bind_rows(tibble(year = min(years)-1, tpf_sr = mkt_sr, n = 0))
  
  (sr_plot <- sr_over_time %>%
      ggplot(aes(year, tpf_sr)) +
      geom_point() +
      geom_line() +
      ylim(c(0, NA)) +
      # geom_hline(yintercept = full_sr) +
      annotate("text", x = 1971, 
               y = 0, label = "Market", colour='black') +
      geom_segment(aes(x = 1971, y = 0.02, xend = 1971, yend = 0.1), size=0.1,arrow = arrow(length = unit(0.2, "cm"))) +
      labs(y = "Ex-Post Tangency SR", x = "Year") +
      theme(axis.title.x = element_blank()) +
      annotate("text", x = 1972, 
               y = 0.34, label = "Beta", colour='black') +
      geom_segment(aes(x = 1972, y = 0.31, xend = 1972, yend = 0.23), size=0.1,arrow = arrow(length = unit(0.2, "cm"))) +
      annotate("text", x = 1979, 
               y = 0.45, label = "Earning-to-Price", colour='black') +
      geom_segment(aes(x = 1979, y = 0.42, xend = 1979, yend = 0.30), size=0.1,arrow = arrow(length = unit(0.2, "cm"))) +
      annotate("text", x = 1983, 
               y = 0.08, label = "Earnings Momentum", colour='black') +
      geom_segment(aes(x = 1981, y = 0.10, xend = 1981, yend = 0.25), size=0.1,arrow = arrow(length = unit(0.2, "cm"))) +
      annotate("text", x = 1989, 
               y = 0.25, label = "Price Momentum", colour='black') +
      geom_segment(aes(x = 1989, y = 0.27, xend = 1989, yend = 0.39), size=0.1,arrow = arrow(length = unit(0.2, "cm"))) +
      annotate("text", x = 1991, 
               y = 0.73, label = "Operating Accruals", colour='black') +
      geom_segment(aes(x = 1991, y = 0.69, xend = 1991, yend = 0.6), size=0.1,arrow = arrow(length = unit(0.2, "cm"))) +
      annotate("text", x = 2002, 
               y = 0.9, label = "Seasonality", colour='black') +
      geom_segment(aes(x = 2002, y = 0.87, xend = 2002, yend = 0.78), size=0.1,arrow = arrow(length = unit(0.2, "cm"))))
  
  n_plot <- sr_over_time %>%
    ggplot(aes(year, n)) +
    geom_point() +
    geom_line() +
    labs(y = "Factor Discovered", x = "Year of Discovery")
  
  list("plot"=cowplot::plot_grid(sr_plot, n_plot, ncol = 1, rel_heights = c(0.7, 0.3)), "data"=sr_over_time)
}

# Plot performance over time
plot_ts <- function(data, oos, alphas, scale, orig_sig, start = as.Date("1986-01-01")) {
  data[, region := case_when(
    region == "us" ~ "US",
    region == "world_ex_us" ~ "World ex. US"
  )]
  data <- data[eom >= start]
  data <- cluster_labels[data, on = "characteristic"]
  if (oos) {
    data <- setDT(char_info)[, .(characteristic, sample_end)][data, on = .(characteristic)]
    data <- data[year(eom) > sample_end][, sample_end := NULL]
  }
  if (orig_sig) {
    data <- setDT(char_info)[, .(characteristic, significance)][data, on = .(characteristic)]
    data <- data[significance==T][, significance := NULL]
  }
  y_axis <- paste0("Cumulative ", if_else(alphas==T, "Alpha ", "Excess Return "), if_else(oos==T, "(OOS)", "(IS)"))
  
  agg <- data[, .(ret = mean(ret), mkt = mean(mkt_vw_exc)), by = .(region, eom)]
  if (alphas) {
    agg[, ret := ret - cov(ret,mkt)/var(mkt)*mkt, by = .(region)]
  }
  if (scale) {
    agg[, ret := ret / (sd(ret)*sqrt(12)/0.1), by = .(region)]
  }
  agg %>% setorder(region, eom)
  agg[, cumret_app := cumsum(ret), by = region]
  plot <- agg %>%
    ggplot(aes(eom, cumret_app, colour = region)) +
    geom_line() +
    labs(y = y_axis) +
    theme(
      legend.position = c(0.85, 0.35),
      legend.title = element_blank(),
      axis.title.x = element_blank()
    )
  # Table
  tbl <- agg %>%
    group_by(region) %>%
    summarise(
      n = n(),
      meanret = mean(ret),
      vol = sd(ret),
      ret_vol = meanret/vol*sqrt(12),
      t = meanret/(vol/sqrt(n))
    ) %>%
    mutate(meanret = meanret*12)
  tbl %>% 
    select(region, ret_vol, t) %>%
    pivot_longer(c(ret_vol, t)) %>%
    mutate(
      value = formatC(value, digits=2, format = "f"),
      value = if_else(name == "t", paste0("(", value, ")"), value)
    ) %>%
    mutate(
      region = if_else(name == "t", "", region)
    ) %>%
    select(-name) %>%
    rename("Region"=region, "Full sample"=value) %>%
    xtable(align = "llc") %>%
    print(include.rownames = F)
  # Output
  return(plot)
}

# Plot OOS performance of significant factors
plot_sig_oos <- function(sig_oos_pfs, sig_type, cutoff_2012, first_date, leg_pos) {
  full <- sig_oos_pfs %>%
    filter(eom >= first_date) %>% 
    group_by(region, type, significant) %>%
    mutate(
      a = ret - cov(mkt,ret)/var(mkt)*mkt
    )  %>%
    summarise(
      n = n(),
      meanret = mean(ret),
      sd = sd(ret),
      sr = meanret/sd * sqrt(12),
      alpha = mean(a),
      resvol = sd(a),
      ir = alpha/resvol*sqrt(12),
      t_alpha = alpha/(resvol/sqrt(n))
    ) %>%
    mutate(alpha = alpha*12) %>%
    filter(region %in% c("us", "world_ex_us") & type == sig_type) %>%
    setDT()
  
  post_harvey <- sig_oos_pfs %>%
    filter(eom >= first_date) %>%
    group_by(region, type, significant) %>%
    mutate(
      a = ret - cov(mkt,ret)/var(mkt)*mkt 
    )  %>%
    filter(eom >= as.Date("2013-01-01")) %>%
    summarise(
      n = n(),
      meanret = mean(ret),
      sd = sd(ret),
      sr = meanret/sd * sqrt(12),
      alpha = mean(a),
      resvol = sd(a),
      ir = alpha/resvol*sqrt(12),
      t_alpha = alpha/(resvol/sqrt(n))
    ) %>%
    mutate(alpha = alpha*12) %>%
    filter(region %in% c("us", "world_ex_us") & type == sig_type) %>%
    setDT()
  
  cumret <- sig_oos_pfs %>%
    filter(eom >= first_date) %>%
    group_by(region, type, significant) %>%
    arrange(region, type, significant, eom) %>%
    filter(type == sig_type & region %in% c("us", "world_ex_us")) %>%
    mutate(
      alpha = ret - cov(mkt,ret)/var(mkt)*mkt,
      alpha = alpha / (sd(alpha)*sqrt(12)/0.1),
      cum_alpha = cumsum(alpha),
      region_pretty = case_when(
        region == "us" ~ "U.S.",
        region == "world_ex_us" ~ "World ex. U.S."
      )
    )
  
  # Figure 
  sig_oos <- cumret %>%
      ggplot(aes(eom, cum_alpha, colour = region_pretty, linetype = region_pretty)) +
      geom_line() +
      labs(y = "Cumulative Alpha") +
      theme(
        axis.title.x = element_blank(),
        legend.position = leg_pos,
        legend.title = element_blank()
      )
  
  # Table for caption
  tbl <- rbind(
    full %>% select(region, ir, t_alpha) %>% mutate(sample = "Full sample"),
    post_harvey %>% select(region, ir, t_alpha) %>% mutate(sample = "Post Harvey et al")
  ) %>%
    pivot_longer(c(ir, t_alpha)) %>%
    mutate(
      value = formatC(value, digits=2, format = "f"),
      value = if_else(name == "t_alpha", paste0("(", value, ")"), value)
    ) %>%
    pivot_wider(names_from = sample, values_from = value) %>%
    mutate(
      region = case_when(
        region=="us" ~ "IR: US",
        region == "world_ex_us" ~ "IR: World ex. US"
      ),
      region = if_else(name == "t_alpha", "", region)
    ) %>%
    select(-name) %>%
    rename("Region"=region)
  
  if (cutoff_2012) {
    sig_oos <- sig_oos + geom_vline(xintercept = as.Date("2012-12-31"), linetype = "dotted", alpha = 1)
    tbl %>% xtable(align = "llcc") %>% print(include.rownames = F)
  } else {
    tbl %>% select(-`Post Harvey et al`) %>% xtable(align = "llc") %>% print(include.rownames = F)
  }
  # Output 
  return(sig_oos)
}


# EB Posterior checks
eb_plots <- function(eb, plot = "shrinkage") {
  if (plot == "cluster_distribution") {
    a <- eb$mle %>% filter(estimate == "alpha") %>% pull(ml_est)
    tb <- eb$mle %>% filter(estimate == "tau_bar") %>% pull(ml_est)
    
    op <- data.frame(x=c(a-3*tb, a+3*tb)) %>% # data.frame(x=c(-1.2, 1.2)) %>%
      ggplot(aes(x)) + 
      stat_function(fun=function(x) dnorm(x = x, mean = a, sd = tb)) +
      labs(x = "Alpha", y = "Density", title = "Population Cluster Distribution ~ N(a0, tau_bar)")
  }
  if (plot == "factor_distribution") {
    a <- eb$mle %>% filter(estimate == "alpha") %>% pull(ml_est)
    tb <- eb$mle %>% filter(estimate == "tau_bar") %>% pull(ml_est)
    tt <- eb$mle %>% filter(estimate == "tau_tilde") %>% pull(ml_est)
    factor_sd <- sqrt(tb^2 + tt^2)
    
    op <- data.frame(x=c(a-3*factor_sd, a+3*factor_sd)) %>% # data.frame(x=c(-1.2, 1.2)) %>%
      ggplot(aes(x)) + 
      stat_function(fun=function(x) dnorm(x = x, mean = a, sd = factor_sd)) +
      labs(x = "Alpha", y = "Density", title = "Population Factor Distribution ~ N(a0, tau_bar + tau_tilde)")
  }
  if (plot == "factor") {
    op <- eb$factors %>%
      ggplot(aes(reorder(characteristic, post_mean), post_mean)) +
      geom_point() +
      geom_errorbar(aes(ymin = p025, ymax = p975)) +
      facet_wrap(~region) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 5, angle = 90, vjust = 0, hjust = 1),
        text = element_text(size = 10)
      )
  }
  if (plot == "cluster") {
    op <- eb$clusters %>% 
      ggplot(aes(reorder(hcl_label, post_mean), post_mean)) +
      geom_point() +
      geom_errorbar(aes(ymin = post_mean - 1.96 * post_sd, ymax = post_mean + 1.96 * post_sd)) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      labs(y = "Posterior Distribution of Cluster Alpha") +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8)
      ) 
  }
  if (plot == "signal") {
    op <- eb$signal %>%
      ggplot(aes(characteristic, post_mean)) +
      geom_point() +
      geom_errorbar(aes(ymin = post_mean - 1.96 * post_sd, ymax = post_mean + 1.96 * post_sd))
  }
  if (plot == "factor_shrinkage") {
    op <- eb$factors %>%
      select(characteristic, region, "eb" = post_mean, "ols" = ols_est) %>%
      gather(eb, ols, key = "type", value = "alpha") %>%
      group_by(characteristic) %>%
      mutate(sort_var = alpha[region == "us" & type == "ols"]) %>%
      ggplot(aes(reorder(characteristic, sort_var), alpha, colour = region)) +
      geom_point() +
      facet_wrap(~type, ncol = 1) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 5, angle = 90, vjust = 0, hjust = 1)
      )
  }
  if (plot == "se") {
    op <- eb_act$factors %>% 
      mutate(ols_p025 = ols_est - 1.96 * ols_se) %>% 
      mutate(se_diff = (ols_se-post_sd) / ols_se) %>% 
      ggplot(aes(x = region, y = se_diff*100, colour = region)) +
      geom_boxplot() +
      expand_limits(y = 0) +
      theme(
        axis.title.x = element_blank(),
        legend.position = "none"
      ) +
      labs(y = "(SE_ols - SE_eb) / SE_ols * 100")
  }
  if (plot == "repl") {
    repl_table <- eb$factors %>%
      group_by(region) %>%
      summarise(
        repl_eb = mean(p025 > 0),
        repl_ols = mean(ols_est - 1.96*ols_se > 0)
      )
    print(repl_table)
    
    op <- eb$factors %>%
      group_by(region, hcl_label) %>%
      summarise(
        mean_alpha = mean(post_mean),
        rep_rate = mean(p025>0)
      ) %>%
      group_by(hcl_label) %>%
      mutate(sort_var = rep_rate[region == "US"]) %>%
      ggplot(aes(reorder(hcl_label, sort_var), rep_rate)) +
      geom_col() +
      facet_wrap(~region, ncol = 1) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7)
      )
  }
  if (plot == "significance") {
    op <- bind_rows(
      eb$factors %>% select(char_reg, characteristic, hcl_label, region, "alpha" = post_mean, "se" = post_sd) %>% mutate(type = "hlm"),
      eb$factors %>% select(char_reg, characteristic, hcl_label, region, "alpha" = ols_est, "se" = ols_se) %>% mutate(type = "ols")
    ) %>%
      mutate(significant = alpha - 1.96 * se > 0) %>%
      group_by(hcl_label) %>%
      mutate(
        sort_var = median(alpha[type == "ols" & region == "US"]),
        type = case_when(
          type == "hlm" ~ "Empirical Bayes",
          type == "ols" ~ "OLS"
        )
      ) %>%
      ggplot(aes(reorder(characteristic, sort_var), alpha, colour = significant)) +
      geom_point() +
      geom_errorbar(aes(ymin = alpha - 1.96*se, ymax = alpha + 1.96*se)) +
      geom_hline(yintercept = 0, colour = "black") +
      facet_wrap(region~type, ncol = 2) +
      labs(y = "Alpha", colour = "Significant") +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 5, angle = 90, vjust = 0, hjust = 1)
      )
  }
  if (plot == "shrinkage") {
    op <- eb$factors %>%
      select(region, characteristic, "hlm_alpha_mean" = post_mean, "ols_alpha" = ols_est) %>%
      gather(-characteristic, -region, key = "type", value = "alpha") %>%
      group_by(characteristic, region) %>%
      mutate(sort_var = sum(alpha * (type == "ols_alpha"))) %>%
      group_by(region) %>%
      mutate(
        rank = frank(sort_var, ties.method = "max") / 2,
        type = case_when(
          type == "hlm_alpha_mean" ~ "Empricial Bayes Posterior Mean",
          type == "ols_alpha" ~ "OLS Estimate"
        )
      ) %>%
      ggplot(aes(rank, alpha, shape = type, colour = type, group = type)) +
      geom_smooth(method = "lm", se = F, formula = "y ~ x") +
      geom_point() +
      facet_wrap(~region) +
      theme(
        axis.text.x = element_text(size = 7),
        text = element_text(size = 10)
      ) +
      labs(x = "Rank OLS Alpha", y = "Alpha", colour = "Type", shape = "Type")
  }
  
  if (plot == "comparison") {
    op <- bind_rows(
      eb$factors %>% select(char_reg, characteristic, hcl_label, region, "alpha" = post_mean, "se" = post_sd) %>% mutate(type = "hlm"),
      eb$factors %>% select(char_reg, characteristic, hcl_label, region, "alpha" = ols_est, "se" = ols_se) %>% mutate(type = "ols")
    ) %>%
      group_by(hcl_label) %>%
      mutate(
        sort_var = median(alpha[type == "ols" & region == "US"]),
        type = case_when(
          type == "hlm" ~ "Empirical Bayes",
          type == "ols" ~ "OLS"
        )
      ) %>%
      ggplot(aes(reorder(characteristic, sort_var), alpha, colour = hcl_label)) +
      geom_point() +
      geom_errorbar(aes(ymin = alpha - 1.96*se, ymax = alpha + 1.96*se)) +
      geom_hline(yintercept = 0, colour = "black") +
      facet_wrap(region~type, ncol = 2) +
      labs(y = "Alpha", colour = "Cluster") +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 5, angle = 90, vjust = 0, hjust = 1)
      )
  }
  if (plot == "cluster_density") {
    op <- eb$clusters %>% 
      group_by(hcl_label) %>% 
      nest() %>% 
      mutate(randoms = data%>% map(~rnorm(1000, mean = .x$post_mean, sd = .x$post_sd))) %>% 
      unnest(randoms) %>% 
      ggplot(aes(x = randoms, fill = hcl_label)) + 
      geom_density(alpha = 0.5)  #  + facet_wrap(~hcl_label)
  }
  print(op)
}
