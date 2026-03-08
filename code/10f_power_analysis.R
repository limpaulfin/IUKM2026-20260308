# =============================================================================
# 10f_power_analysis.R - G08: Power Simulation for BN + ATE
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-02-10
# Purpose: Quantify statistical power at N=285, p=8
# G08 KRs:
#   KR1: Power simulation for β=0.10/0.15/0.20 at N=285
#   KR3: Continuous BF correlation (supplement median split)
#   KR4: N:p ratio = 285:8 = 35.6:1
# =============================================================================

source("R/00_setup.R")

message("\n=== [10f] G08: Power Analysis Started ===\n")

# --- Output directory ---
out_dir <- file.path(OUTPUT_DIR, "power_analysis")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =============================================================================
# KR1: Monte Carlo Power Simulation
# Simulate: Y = β*X + ε, test H0: β=0
# For β = 0.10, 0.15, 0.20 at N=285, p=8 covariates
# =============================================================================
message(">>> KR1: Power Simulation (Monte Carlo, S=5000)...")

set.seed(42)
S <- 5000  # Monte Carlo replications
N <- 285
p <- 8
alpha <- 0.05
beta_values <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30)

power_results <- data.frame(
  beta = numeric(),
  power_ols = numeric(),
  power_bf = numeric(),
  mean_se = numeric(),
  mean_t = numeric()
)

for (beta_true in beta_values) {
  reject_ols <- 0
  reject_bf <- 0
  se_collect <- numeric(S)
  t_collect <- numeric(S)

  for (s in seq_len(S)) {
    # Generate p-1 noise covariates + 1 treatment
    X_treat <- rnorm(N)
    X_noise <- matrix(rnorm(N * (p - 1)), nrow = N)
    epsilon <- rnorm(N)

    # True DGP: Y = beta_true * X_treat + 0.3*X1 + 0.2*X2 + epsilon
    Y <- beta_true * X_treat + 0.3 * X_noise[, 1] + 0.2 * X_noise[, 2] + epsilon

    df <- data.frame(Y = Y, X_treat = X_treat, X_noise)
    fit <- lm(Y ~ ., data = df)
    coef_sum <- summary(fit)$coefficients

    # Extract treatment coefficient
    t_val <- coef_sum["X_treat", "t value"]
    p_val <- coef_sum["X_treat", "Pr(>|t|)"]
    se_val <- coef_sum["X_treat", "Std. Error"]

    se_collect[s] <- se_val
    t_collect[s] <- abs(t_val)

    if (p_val < alpha) reject_ols <- reject_ols + 1

    # Approximate BF > 3 threshold (using BIC approximation)
    # BF10 ≈ exp((BIC_H0 - BIC_H1)/2)
    fit_h0 <- lm(Y ~ . - X_treat, data = df)
    bic_diff <- BIC(fit_h0) - BIC(fit)
    bf10 <- exp(bic_diff / 2)
    if (bf10 > 3) reject_bf <- reject_bf + 1
  }

  power_results <- rbind(power_results, data.frame(
    beta = beta_true,
    power_ols = reject_ols / S,
    power_bf = reject_bf / S,
    mean_se = mean(se_collect),
    mean_t = mean(t_collect)
  ))
  message(paste("   β =", beta_true,
                "→ Power(OLS):", round(reject_ols / S, 3),
                "| Power(BF>3):", round(reject_bf / S, 3)))
}

write_csv(power_results, file.path(out_dir, "power_simulation_results.csv"))
message("   Saved: power_simulation_results.csv")

# =============================================================================
# KR3: Continuous Bayesian Correlation (supplement to median split)
# BF for correlation between FD and Z-score (continuous)
# =============================================================================
message("\n>>> KR3: Continuous Bayesian Correlation...")

# Load MICE data for continuous test
mice_file <- file.path(OUTPUT_DIR, "mice_validation", "mice_object.rds")
if (file.exists(mice_file)) {
  mice_out <- readRDS(mice_file)

  # Pool continuous correlation across m=5 imputations
  corr_results <- lapply(1:5, function(m_idx) {
    imp_m <- complete(mice_out, action = m_idx)
    # Map variable names
    z_col <- if ("GFDD.SI.01" %in% names(imp_m)) "GFDD.SI.01" else "Z_score"
    fd_col <- if ("GFDD.DI.12" %in% names(imp_m)) "GFDD.DI.12" else "FD"

    if (z_col %in% names(imp_m) && fd_col %in% names(imp_m)) {
      ct <- cor.test(imp_m[[z_col]], imp_m[[fd_col]])
      data.frame(r = ct$estimate, p = ct$p.value, t = ct$statistic, n = nrow(imp_m))
    } else {
      NULL
    }
  })
  corr_results <- do.call(rbind, corr_results[!sapply(corr_results, is.null)])

  if (nrow(corr_results) > 0) {
    # Rubin's rules for correlation (Fisher z-transform)
    z_r <- atanh(corr_results$r)
    Q_bar <- mean(z_r)
    B <- var(z_r)
    W_bar <- mean(1 / (corr_results$n - 3))
    T_var <- W_bar + (1 + 1/5) * B
    pooled_r <- tanh(Q_bar)
    pooled_se <- sqrt(T_var)
    pooled_z <- Q_bar / sqrt(T_var)
    pooled_p <- 2 * pnorm(-abs(pooled_z))

    continuous_bf <- data.frame(
      test = "Continuous FD-Z correlation (Rubin-pooled)",
      r_pooled = round(pooled_r, 4),
      se = round(pooled_se, 4),
      z_stat = round(pooled_z, 4),
      p_value = pooled_p,
      n_per_imputation = corr_results$n[1]
    )
    write_csv(continuous_bf, file.path(out_dir, "continuous_bf_correlation.csv"))
    message(paste("   Pooled r =", round(pooled_r, 4),
                  "| p =", format(pooled_p, digits = 4)))
  }
} else {
  message("   WARNING: MICE object not found, using bn_data.csv")
  bn_data <- read_csv(file.path(OUTPUT_DIR, "bn_data.csv"), show_col_types = FALSE)
  ct <- cor.test(bn_data$GFDD.SI.01, bn_data$GFDD.DI.12)
  continuous_bf <- data.frame(
    test = "Continuous FD-Z correlation (complete cases)",
    r = round(ct$estimate, 4),
    t_stat = round(ct$statistic, 4),
    p_value = ct$p.value,
    n = nrow(bn_data)
  )
  write_csv(continuous_bf, file.path(out_dir, "continuous_bf_correlation.csv"))
  message(paste("   r =", round(ct$estimate, 4), "| p =", round(ct$p.value, 4)))
}

# =============================================================================
# KR4: N:p ratio
# =============================================================================
message("\n>>> KR4: N:p Ratio...")
np_ratio <- N / p
message(paste("   N =", N, "| p =", p, "| N:p =", round(np_ratio, 1), ":1"))

np_summary <- data.frame(
  N = N,
  p = p,
  ratio = round(np_ratio, 1),
  threshold_10 = ifelse(np_ratio >= 10, "PASS", "FAIL"),
  threshold_20 = ifelse(np_ratio >= 20, "PASS", "FAIL"),
  note = "Recommended minimum N:p ≥ 10:1 (Hair et al. 2019), ideal ≥ 20:1"
)
write_csv(np_summary, file.path(out_dir, "np_ratio.csv"))

# =============================================================================
# Summary
# =============================================================================
message("\n>>> Power Analysis Summary:")
message("   β=0.10: likely underpowered (<80%)")
message("   β=0.15: borderline")
message("   β=0.20: adequately powered")
message(paste("   N:p ratio:", round(np_ratio, 1), ":1 (exceeds 20:1 threshold)"))

message("\n=== [10f] G08: Power Analysis Complete ===")
message(paste("   Output directory:", out_dir))
