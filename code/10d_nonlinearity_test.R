# =============================================================================
# 10d_nonlinearity_test.R - G11 KR2/KR3: FD² Quadratic Non-linearity Test
# Purpose: Test inverted-U (too much finance) hypothesis via FD² in ATE
# Author: Thanh-Phong Lam
# Created: 2026-02-10
# Ref: Arcand et al. (2015), Law & Singh (2014)
# G11 KRs: KR2 FD² tested in ATE, KR3 Non-linearity result reported
# =============================================================================

source("R/00_setup.R")

message("\n=== [10d] Non-linearity Test: FD² Quadratic ===\n")

# --- Output directory ---
NL_DIR <- file.path(OUTPUT_DIR, "nonlinearity")
if (!dir.exists(NL_DIR)) dir.create(NL_DIR, recursive = TRUE)

# --- Load MICE object ---
mice_out <- readRDS(file.path(OUTPUT_DIR, "mice_imputation.rds"))

# =====================================================================
# Model 1: Linear (baseline) — Z = β0 + β1·FD + β2·GDP + ε
# Model 2: Quadratic — Z = β0 + β1·FD + β2·FD² + β3·GDP + ε
# Model 3: Full controls — Z = β0 + β1·FD + β2·FD² + β3·GDP + β4·GovDebt + β5·FDI + β6·CPI + ε
# =====================================================================

# --- Model 1: Linear (Rubin pooled) ---
message(">>> Model 1: Linear Z ~ FD + GDP")
fit_linear <- with(mice_out, lm(Z_score ~ FD + GDP))
pooled_linear <- summary(pool(fit_linear))
message("   Pooled linear:")
print(pooled_linear)

# --- Add FD² to each imputed dataset ---
# mice::complete() for each m, add FD_sq, then re-run
message("\n>>> Model 2: Quadratic Z ~ FD + FD² + GDP")

# Manual pooling for quadratic (need to add FD_sq to mice object)
quad_fits <- lapply(1:5, function(m_idx) {
  imp_m <- complete(mice_out, action = m_idx)
  imp_m$FD_sq <- imp_m$FD^2
  lm(Z_score ~ FD + FD_sq + GDP, data = imp_m)
})

# Rubin's rules manually
coefs <- sapply(quad_fits, coef)
vars <- sapply(quad_fits, function(fit) diag(vcov(fit)))

# Within-imputation variance (average)
W_bar <- rowMeans(vars)
# Between-imputation variance
Q_bar <- rowMeans(coefs)
B <- apply(coefs, 1, var)
# Total variance (Rubin's formula)
m <- 5
T_var <- W_bar + (1 + 1/m) * B

# Pooled estimates
pooled_quad <- data.frame(
  term = names(Q_bar),
  estimate = Q_bar,
  std.error = sqrt(T_var),
  statistic = Q_bar / sqrt(T_var),
  row.names = NULL
)
# Approximate df (Barnard-Rubin)
r <- (1 + 1/m) * B / W_bar
nu <- (m - 1) * (1 + 1/r)^2
pooled_quad$df <- nu
pooled_quad$p.value <- 2 * pt(-abs(pooled_quad$statistic), df = pooled_quad$df)

message("   Pooled quadratic:")
print(pooled_quad)

# --- Model 3: Full controls with quadratic ---
message("\n>>> Model 3: Quadratic Z ~ FD + FD² + GDP + GovDebt + FDI + CPI")

full_fits <- lapply(1:5, function(m_idx) {
  imp_m <- complete(mice_out, action = m_idx)
  imp_m$FD_sq <- imp_m$FD^2
  lm(Z_score ~ FD + FD_sq + GDP + GovDebt + FDI + CPI, data = imp_m)
})

coefs_full <- sapply(full_fits, coef)
vars_full <- sapply(full_fits, function(fit) diag(vcov(fit)))
W_bar_full <- rowMeans(vars_full)
Q_bar_full <- rowMeans(coefs_full)
B_full <- apply(coefs_full, 1, var)
T_var_full <- W_bar_full + (1 + 1/m) * B_full

pooled_full <- data.frame(
  term = names(Q_bar_full),
  estimate = Q_bar_full,
  std.error = sqrt(T_var_full),
  statistic = Q_bar_full / sqrt(T_var_full),
  row.names = NULL
)
r_full <- (1 + 1/m) * B_full / W_bar_full
nu_full <- (m - 1) * (1 + 1/r_full)^2
pooled_full$df <- nu_full
pooled_full$p.value <- 2 * pt(-abs(pooled_full$statistic), df = pooled_full$df)

message("   Pooled full quadratic:")
print(pooled_full)

# =====================================================================
# Turning point calculation (if FD² significant)
# Z = β1·FD + β2·FD² → dZ/dFD = β1 + 2·β2·FD = 0 → FD* = -β1/(2·β2)
# =====================================================================
message("\n>>> Turning Point Analysis")

fd_coef <- pooled_quad$estimate[pooled_quad$term == "FD"]
fd_sq_coef <- pooled_quad$estimate[pooled_quad$term == "FD_sq"]
fd_sq_p <- pooled_quad$p.value[pooled_quad$term == "FD_sq"]

message(paste("   FD coefficient:", round(fd_coef, 6)))
message(paste("   FD² coefficient:", round(fd_sq_coef, 6)))
message(paste("   FD² p-value:", format.pval(fd_sq_p, digits = 4)))

if (fd_sq_coef != 0) {
  turning_point <- -fd_coef / (2 * fd_sq_coef)
  message(paste("   Turning point FD*:", round(turning_point, 2), "%"))

  # Compare with sample range
  fd_range <- range(complete(mice_out, 1)$FD)
  fd_mean <- mean(complete(mice_out, 1)$FD)
  message(paste("   FD range:", round(fd_range[1], 1), "to", round(fd_range[2], 1)))
  message(paste("   FD mean:", round(fd_mean, 1)))

  if (turning_point > fd_range[1] && turning_point < fd_range[2]) {
    message("   >> Turning point WITHIN sample range")
  } else {
    message("   >> Turning point OUTSIDE sample range")
  }
}

# =====================================================================
# Model comparison: Linear vs Quadratic (likelihood ratio proxy)
# =====================================================================
message("\n>>> Model Comparison: Linear vs Quadratic")

# Pool R² across imputations
r2_linear <- sapply(1:5, function(m_idx) {
  imp_m <- complete(mice_out, action = m_idx)
  summary(lm(Z_score ~ FD + GDP, data = imp_m))$r.squared
})

r2_quad <- sapply(1:5, function(m_idx) {
  imp_m <- complete(mice_out, action = m_idx)
  imp_m$FD_sq <- imp_m$FD^2
  summary(lm(Z_score ~ FD + FD_sq + GDP, data = imp_m))$r.squared
})

r2_full <- sapply(1:5, function(m_idx) {
  imp_m <- complete(mice_out, action = m_idx)
  imp_m$FD_sq <- imp_m$FD^2
  summary(lm(Z_score ~ FD + FD_sq + GDP + GovDebt + FDI + CPI, data = imp_m))$r.squared
})

message(paste("   R² linear (mean m=5):", round(mean(r2_linear), 4)))
message(paste("   R² quadratic (mean m=5):", round(mean(r2_quad), 4)))
message(paste("   R² full quadratic (mean m=5):", round(mean(r2_full), 4)))
message(paste("   R² improvement (quad - linear):", round(mean(r2_quad) - mean(r2_linear), 4)))

# F-test for FD² (per imputation, then pool)
f_tests <- sapply(1:5, function(m_idx) {
  imp_m <- complete(mice_out, action = m_idx)
  imp_m$FD_sq <- imp_m$FD^2
  m_lin <- lm(Z_score ~ FD + GDP, data = imp_m)
  m_quad <- lm(Z_score ~ FD + FD_sq + GDP, data = imp_m)
  a <- anova(m_lin, m_quad)
  c(F_stat = a$F[2], p_value = a$`Pr(>F)`[2])
})

message(paste("   F-test for FD² (mean F):", round(mean(f_tests["F_stat", ]), 4)))
message(paste("   F-test for FD² (mean p):", round(mean(f_tests["p_value", ]), 4)))

# =====================================================================
# Save all results
# =====================================================================
write_csv(pooled_linear, file.path(NL_DIR, "pooled_linear.csv"))
write_csv(pooled_quad, file.path(NL_DIR, "pooled_quadratic.csv"))
write_csv(pooled_full, file.path(NL_DIR, "pooled_full_quadratic.csv"))

comparison <- data.frame(
  model = c("Linear (FD + GDP)", "Quadratic (FD + FD² + GDP)", "Full Quadratic (+ controls)"),
  r2_mean = c(mean(r2_linear), mean(r2_quad), mean(r2_full)),
  fd_sq_coef = c(NA, fd_sq_coef, pooled_full$estimate[pooled_full$term == "FD_sq"]),
  fd_sq_p = c(NA, fd_sq_p, pooled_full$p.value[pooled_full$term == "FD_sq"]),
  turning_point = c(NA, turning_point,
                     -pooled_full$estimate[pooled_full$term == "FD"] /
                       (2 * pooled_full$estimate[pooled_full$term == "FD_sq"]))
)
write_csv(comparison, file.path(NL_DIR, "model_comparison.csv"))

message("\n=== [10d] Non-linearity Test Summary ===")
message(paste("   Output directory:", NL_DIR))
message("   Files: pooled_linear.csv, pooled_quadratic.csv, pooled_full_quadratic.csv, model_comparison.csv")
if (fd_sq_p < 0.05) {
  message("   >> CONCLUSION: FD² SIGNIFICANT — Non-linear relationship detected")
} else {
  message("   >> CONCLUSION: FD² NOT significant — Linear specification adequate")
}
message("\n=== [10d] Complete ===")
