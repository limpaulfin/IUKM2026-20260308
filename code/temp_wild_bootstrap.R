# temp_wild_bootstrap.R - Manual Wild Cluster Bootstrap-t + Ibragimov-Müller
# Cameron, Gelbach & Miller (2008); Webb (2013) weights
# Manual implementation (fwildclusterboot unavailable for R 4.5.2)
source("R/00_setup.R")

for (pkg in c("sandwich", "lmtest", "broom")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(sandwich)
library(lmtest)
library(broom)

out_dir <- file.path(OUTPUT_DIR, "cluster_bootstrap")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Load data (same as 10e)
panel_full <- read_csv(file.path(OUTPUT_DIR, "panel_merged.csv"), show_col_types = FALSE)
ate_vars <- c("country_code", "GFDD.SI.01", "GFDD.DI.12", "NY.GDP.MKTP.KD.ZG",
              "FB.AST.NPER.ZS", "GFDD.EI.01", "GC.DOD.TOTL.GD.ZS")
ate_data <- panel_full %>%
  select(all_of(ate_vars)) %>%
  na.omit() %>%
  rename(Z_score = GFDD.SI.01, FD = GFDD.DI.12, GDP = NY.GDP.MKTP.KD.ZG,
         NPL = FB.AST.NPER.ZS, NIM = GFDD.EI.01, GovDebt = GC.DOD.TOTL.GD.ZS)

cat("N =", nrow(ate_data), "\n")
cat("G =", length(unique(ate_data$country_code)), "\n")

# =====================================================
# Webb weights (6-point distribution for G<11)
# Webb (2013): {-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5)}
# =====================================================
webb_weights <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
clusters <- unique(ate_data$country_code)
G <- length(clusters)

# =====================================================
# Function: Wild Cluster Bootstrap-t (impose H0)
# Cameron, Gelbach & Miller (2008) Algorithm
# =====================================================
wild_cluster_bootstrap_t <- function(data, formula_unr, formula_res,
                                     param, cluster_var, B = 9999, seed = 42) {
  set.seed(seed)

  # Fit unrestricted + restricted models
  mod_unr <- lm(formula_unr, data = data)
  mod_res <- lm(formula_res, data = data)

  # Original t-stat (CRVE)
  cl_vcov <- vcovCL(mod_unr, cluster = data[[cluster_var]], type = "HC1")
  ct <- coeftest(mod_unr, vcov. = cl_vcov)
  t_orig <- ct[param, "t value"]
  p_crve <- ct[param, "Pr(>|t|)"]

  # i.i.d. t-stat for comparison
  ct_iid <- coeftest(mod_unr)
  t_iid <- ct_iid[param, "t value"]
  p_iid <- ct_iid[param, "Pr(>|t|)"]

  # Restricted residuals and fitted
  e_res <- residuals(mod_res)
  y_fit <- fitted(mod_res)

  cluster_ids <- data[[cluster_var]]

  # Bootstrap loop
  t_boot <- numeric(B)
  for (b in seq_len(B)) {
    # Draw one Webb weight per cluster
    w <- sample(webb_weights, G, replace = TRUE)
    w_obs <- w[match(cluster_ids, clusters)]

    # Bootstrap Y (impose H0)
    boot_data <- data
    boot_data$Z_score <- y_fit + w_obs * e_res

    # Fit unrestricted model on bootstrap Y
    mod_b <- lm(formula_unr, data = boot_data)
    cl_b <- tryCatch(
      vcovCL(mod_b, cluster = boot_data[[cluster_var]], type = "HC1"),
      error = function(e) NULL
    )
    if (!is.null(cl_b)) {
      t_boot[b] <- coeftest(mod_b, vcov. = cl_b)[param, "t value"]
    } else {
      t_boot[b] <- NA
    }
  }

  t_valid <- t_boot[!is.na(t_boot)]
  p_wild <- mean(abs(t_valid) >= abs(t_orig))

  list(
    t_orig = t_orig, p_crve = p_crve,
    t_iid = t_iid, p_iid = p_iid,
    p_wild = p_wild,
    B_valid = length(t_valid), B_total = B,
    estimate = coef(mod_unr)[param],
    t_boot_dist = t_valid
  )
}

# =====================================================
# Model 1: Simple (Z ~ FD + GDP) — main paper result
# =====================================================
cat("\n=== Model 1: Z ~ FD + GDP ===\n")
res1 <- wild_cluster_bootstrap_t(
  data = ate_data,
  formula_unr = Z_score ~ FD + GDP,
  formula_res = Z_score ~ GDP,
  param = "FD", cluster_var = "country_code", B = 9999
)
cat("FD estimate:", round(res1$estimate, 6), "\n")
cat("i.i.d.   t =", round(res1$t_iid, 4), " p =", round(res1$p_iid, 6), "\n")
cat("CRVE     t =", round(res1$t_orig, 4), " p =", round(res1$p_crve, 4), "\n")
cat("Wild bootstrap-t (Webb, B=9999)  p =", round(res1$p_wild, 4), "\n")
cat("Valid bootstrap iterations:", res1$B_valid, "/", res1$B_total, "\n")

# =====================================================
# Model 2: Full (Z ~ FD + GDP + NPL + NIM + GovDebt)
# =====================================================
cat("\n=== Model 2: Z ~ FD + GDP + NPL + NIM + GovDebt ===\n")
res2 <- wild_cluster_bootstrap_t(
  data = ate_data,
  formula_unr = Z_score ~ FD + GDP + NPL + NIM + GovDebt,
  formula_res = Z_score ~ GDP + NPL + NIM + GovDebt,
  param = "FD", cluster_var = "country_code", B = 9999
)
cat("FD estimate:", round(res2$estimate, 6), "\n")
cat("i.i.d.   t =", round(res2$t_iid, 4), " p =", round(res2$p_iid, 6), "\n")
cat("CRVE     t =", round(res2$t_orig, 4), " p =", round(res2$p_crve, 4), "\n")
cat("Wild bootstrap-t (Webb, B=9999)  p =", round(res2$p_wild, 4), "\n")
cat("Valid bootstrap iterations:", res2$B_valid, "/", res2$B_total, "\n")

# =====================================================
# Save combined results
# =====================================================
wild_results <- data.frame(
  model = c("Z ~ FD + GDP", "Z ~ FD + GDP + NPL + NIM + GovDebt"),
  FD_estimate = c(res1$estimate, res2$estimate),
  t_iid = c(res1$t_iid, res2$t_iid),
  p_iid = c(res1$p_iid, res2$p_iid),
  t_crve = c(res1$t_orig, res2$t_orig),
  p_crve = c(res1$p_crve, res2$p_crve),
  p_wild_boot = c(res1$p_wild, res2$p_wild),
  B = 9999, G = G, N = nrow(ate_data),
  weight_type = "Webb"
)
write_csv(wild_results, file.path(out_dir, "wild_bootstrap_results.csv"))
cat("\nSaved: wild_bootstrap_results.csv\n")

# =====================================================
# KR6: Ibragimov-Müller (2010) Test
# =====================================================
cat("\n=== KR6: Ibragimov-Mueller Test ===\n")

im_by_country <- ate_data %>%
  group_by(country_code) %>%
  filter(n() >= 3) %>%
  do(tidy(lm(Z_score ~ FD + GDP, data = .))) %>%
  filter(term == "FD") %>%
  ungroup()

cat("Per-country FD coefficients (simple model):\n")
for (i in seq_len(nrow(im_by_country))) {
  cat("  ", im_by_country$country_code[i], ":",
      round(im_by_country$estimate[i], 4),
      "(SE:", round(im_by_country$std.error[i], 4), ")\n")
}

im_coefs <- im_by_country$estimate
if (length(im_coefs) >= 3) {
  im_test <- t.test(im_coefs, mu = 0)
  cat("\nIM t-test on per-country FD coefficients:\n")
  cat("  Mean coef:", round(mean(im_coefs), 4), "\n")
  cat("  Median coef:", round(median(im_coefs), 4), "\n")
  cat("  t-stat:", round(im_test$statistic, 4), "\n")
  cat("  p-value:", round(im_test$p.value, 4), "\n")
  cat("  95% CI: [", round(im_test$conf.int[1], 4), ",",
      round(im_test$conf.int[2], 4), "]\n")
  cat("  df:", im_test$parameter, "\n")

  im_results <- data.frame(
    method = "Ibragimov-Mueller (2010)",
    mean_coef = mean(im_coefs), median_coef = median(im_coefs),
    t_stat = as.numeric(im_test$statistic), p_value = im_test$p.value,
    ci_lower = im_test$conf.int[1], ci_upper = im_test$conf.int[2],
    df = as.numeric(im_test$parameter), G_used = length(im_coefs)
  )
  write_csv(im_results, file.path(out_dir, "ibragimov_muller_results.csv"))
  cat("Saved: ibragimov_muller_results.csv\n")
}

cat("\n=== Wild Bootstrap + IM Test Complete ===\n")
