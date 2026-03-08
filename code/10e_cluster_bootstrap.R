# =============================================================================
# 10e_cluster_bootstrap.R - G03: Cluster Bootstrap + Fixed Effects + Clustered SE
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-02-10
# Purpose: Address I.I.D. violation in panel BN bootstrap
# G03 KRs:
#   KR1: Cluster bootstrap by country R=1000
#   KR2: Fixed-effects filtered BN (de-mean by country)
#   KR3: ATE with clustered SE (vcovCL)
#   KR4: i.i.d. vs cluster CI comparison table
# =============================================================================

source("R/00_setup.R")

# Additional packages
for (pkg in c("sandwich", "lmtest")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(sandwich)
library(lmtest)

message("\n=== [10e] G03: Cluster Bootstrap + Fixed Effects Started ===\n")

# --- Output directory ---
out_dir <- file.path(OUTPUT_DIR, "cluster_bootstrap")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- Load data WITH country codes ---
message(">>> Loading panel_merged.csv (with country_code)...")
panel_full <- read_csv(file.path(OUTPUT_DIR, "panel_merged.csv"), show_col_types = FALSE)

# Load BN structure to get node names
hc_structure <- readRDS(file.path(OUTPUT_DIR, "bn_structure_hc.rds"))
bn_nodes <- nodes(hc_structure)

# Filter to complete cases on BN variables
panel_cc <- panel_full %>%
  filter(complete.cases(across(all_of(bn_nodes)))) %>%
  select(country_code, all_of(bn_nodes))

countries <- unique(panel_cc$country_code)
n_countries <- length(countries)

message(paste("   Complete cases:", nrow(panel_cc)))
message(paste("   Countries:", n_countries))
message(paste("   Countries:", paste(countries, collapse = ", ")))

# --- Blacklist (same as 05b) ---
blacklist <- data.frame(
  from = setdiff(bn_nodes, "year"),
  to = "year"
)

# =============================================================================
# KR1: Cluster Bootstrap by Country (R=1000)
# Resample countries with replacement, take ALL years for each sampled country
# =============================================================================
message("\n>>> KR1: Cluster Bootstrap by Country (R=1000)...")

set.seed(42)
R_cluster <- 1000
bn_data_only <- as.data.frame(panel_cc %>% select(all_of(bn_nodes)))

# Storage for edge strengths
cluster_edge_list <- vector("list", R_cluster)

pb <- txtProgressBar(min = 0, max = R_cluster, style = 3)
for (b in seq_len(R_cluster)) {
  # Resample countries with replacement
  sampled_countries <- sample(countries, size = n_countries, replace = TRUE)

  # Collect all rows for sampled countries (may have duplicates)
  boot_rows <- do.call(rbind, lapply(sampled_countries, function(cc) {
    panel_cc %>% filter(country_code == cc) %>% select(all_of(bn_nodes))
  }))
  boot_data <- as.data.frame(boot_rows)

  # Learn BN structure on bootstrap sample
  boot_bn <- tryCatch({
    hc(boot_data, score = "bic-g", blacklist = blacklist)
  }, error = function(e) NULL)

  if (!is.null(boot_bn)) {
    edges <- arcs(boot_bn)
    if (nrow(edges) > 0) {
      cluster_edge_list[[b]] <- as.data.frame(edges)
    }
  }
  setTxtProgressBar(pb, b)
}
close(pb)

# Compute edge frequencies
all_edges <- do.call(rbind, cluster_edge_list)
if (nrow(all_edges) > 0) {
  cluster_strength <- all_edges %>%
    group_by(from, to) %>%
    summarise(strength = n() / R_cluster, .groups = "drop") %>%
    arrange(desc(strength))

  write_csv(cluster_strength, file.path(out_dir, "cluster_bootstrap_strength.csv"))
  message(paste("   Total unique edges found:", nrow(cluster_strength)))
  message(paste("   Strong edges (>=0.5):", sum(cluster_strength$strength >= 0.5)))
} else {
  message("   WARNING: No edges found in cluster bootstrap")
  cluster_strength <- data.frame(from = character(), to = character(), strength = numeric())
}

# =============================================================================
# KR2: Fixed-Effects Filtered BN (de-mean by country)
# Within transformation: x_it - mean(x_i.)
# =============================================================================
message("\n>>> KR2: Fixed-Effects De-meaned BN...")

# De-mean all BN variables by country (within transformation)
panel_demean <- panel_cc %>%
  group_by(country_code) %>%
  mutate(across(all_of(bn_nodes), ~ . - mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  select(all_of(bn_nodes))

fe_data <- as.data.frame(panel_demean)

# Learn BN on de-meaned data
fe_bn <- tryCatch({
  hc(fe_data, score = "bic-g", blacklist = blacklist)
}, error = function(e) {
  message(paste("   FE BN error:", e$message))
  NULL
})

if (!is.null(fe_bn)) {
  fe_edges <- as.data.frame(arcs(fe_bn))
  write_csv(fe_edges, file.path(out_dir, "fe_demeaned_edges.csv"))
  message(paste("   FE de-meaned edges:", nrow(fe_edges)))

  # Compare with original HC
  orig_edges <- as.data.frame(arcs(hc_structure))
  message(paste("   Original HC edges:", nrow(orig_edges)))

  # Find common edges
  orig_set <- paste(orig_edges$from, orig_edges$to, sep = "->")
  fe_set <- paste(fe_edges$from, fe_edges$to, sep = "->")
  common <- intersect(orig_set, fe_set)
  message(paste("   Common edges:", length(common)))

  fe_comparison <- data.frame(
    method = c("Original HC", "FE De-meaned HC"),
    n_edges = c(nrow(orig_edges), nrow(fe_edges)),
    common_with_original = c(nrow(orig_edges), length(common))
  )
  write_csv(fe_comparison, file.path(out_dir, "fe_comparison.csv"))
} else {
  message("   WARNING: FE BN failed")
}

# =============================================================================
# KR3: ATE with Clustered Standard Errors (vcovCL)
# =============================================================================
message("\n>>> KR3: ATE with Clustered SE...")

# Prepare ATE data from panel_full (has ALL variables, not just bn_nodes)
# Variable mapping (from paper): Z_score=GFDD.SI.01, FD=GFDD.DI.12, GDP=NY.GDP.MKTP.KD.ZG
# NPL=FB.AST.NPER.ZS, NIM=GFDD.EI.01, GovDebt=GC.DOD.TOTL.GD.ZS
ate_vars <- c("country_code", "GFDD.SI.01", "GFDD.DI.12", "NY.GDP.MKTP.KD.ZG",
              "FB.AST.NPER.ZS", "GFDD.EI.01", "GC.DOD.TOTL.GD.ZS")
ate_data <- panel_full %>%
  select(all_of(ate_vars)) %>%
  na.omit() %>%
  rename(
    Z_score = GFDD.SI.01,
    FD = GFDD.DI.12,
    GDP = NY.GDP.MKTP.KD.ZG,
    NPL = FB.AST.NPER.ZS,
    NIM = GFDD.EI.01,
    GovDebt = GC.DOD.TOTL.GD.ZS
  )

message(paste("   ATE obs:", nrow(ate_data)))

# OLS with i.i.d. SE
ate_ols <- lm(Z_score ~ FD + GDP + NPL + NIM + GovDebt, data = ate_data)

# Clustered SE by country
cl_vcov <- vcovCL(ate_ols, cluster = ate_data$country_code, type = "HC1")
ate_cl <- coeftest(ate_ols, vcov. = cl_vcov)

# i.i.d. SE for comparison
ate_iid <- coeftest(ate_ols)

# Build comparison table
coef_names <- rownames(ate_iid)
comparison_se <- data.frame(
  term = coef_names,
  estimate = ate_iid[, "Estimate"],
  se_iid = ate_iid[, "Std. Error"],
  t_iid = ate_iid[, "t value"],
  p_iid = ate_iid[, "Pr(>|t|)"],
  se_cluster = ate_cl[, "Std. Error"],
  t_cluster = ate_cl[, "t value"],
  p_cluster = ate_cl[, "Pr(>|t|)"],
  se_ratio = ate_cl[, "Std. Error"] / ate_iid[, "Std. Error"]
)

write_csv(comparison_se, file.path(out_dir, "ate_clustered_se_comparison.csv"))
message("   ATE Clustered vs I.I.D. SE:")
print(comparison_se)

# =============================================================================
# KR4: i.i.d. vs Cluster CI Comparison Table
# =============================================================================
message("\n>>> KR4: i.i.d. vs Cluster Bootstrap CI Comparison...")

# Load existing i.i.d. bootstrap strength
iid_strength_file <- file.path(OUTPUT_DIR, "bn_bootstrap_strength.csv")
if (file.exists(iid_strength_file)) {
  iid_strength <- read_csv(iid_strength_file, show_col_types = FALSE)

  # Merge i.i.d. and cluster bootstrap strengths
  ci_comparison <- iid_strength %>%
    rename(strength_iid = strength, direction_iid = direction) %>%
    full_join(
      cluster_strength %>% rename(strength_cluster = strength),
      by = c("from", "to")
    ) %>%
    mutate(
      strength_iid = ifelse(is.na(strength_iid), 0, strength_iid),
      strength_cluster = ifelse(is.na(strength_cluster), 0, strength_cluster),
      diff = strength_cluster - strength_iid,
      abs_diff = abs(diff)
    ) %>%
    arrange(desc(abs_diff))

  write_csv(ci_comparison, file.path(out_dir, "iid_vs_cluster_comparison.csv"))

  message("   Top differences (cluster - i.i.d.):")
  print(head(ci_comparison %>% select(from, to, strength_iid, strength_cluster, diff), 10))

  # Summary statistics
  strong_iid <- sum(iid_strength$strength >= 0.5, na.rm = TRUE)
  strong_cluster <- sum(cluster_strength$strength >= 0.5, na.rm = TRUE)

  summary_table <- data.frame(
    metric = c("Total unique edges", "Strong edges (>=0.5)", "R (resamples)",
               "Resampling unit", "Mean edge strength"),
    iid_bootstrap = c(
      nrow(iid_strength), strong_iid, 200, "row",
      round(mean(iid_strength$strength), 4)
    ),
    cluster_bootstrap = c(
      nrow(cluster_strength), strong_cluster, R_cluster, "country",
      round(mean(cluster_strength$strength), 4)
    )
  )
  write_csv(summary_table, file.path(out_dir, "bootstrap_summary_comparison.csv"))
  message("\n   Bootstrap Comparison Summary:")
  print(summary_table)
} else {
  message("   WARNING: i.i.d. bootstrap strength file not found")
}

# =============================================================================
# KR5: Wild Cluster Bootstrap-t (Webb weights, G=9)
# Cameron, Gelbach & Miller (2008); Webb (2023, CJE)
# Manual implementation (fwildclusterboot unavailable for R 4.5.2)
# Webb weights preferred for G<11 (6-point vs 2-point distribution)
# =============================================================================
message("\n>>> KR5: Wild Cluster Bootstrap-t (Webb weights, manual)...")

# Webb 6-point weights: avoids limited permutation space of Rademacher (2^G)
webb_weights <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
G_ate <- length(unique(ate_data$country_code))
clusters_ate <- unique(ate_data$country_code)

wild_cluster_bootstrap_t <- function(data, formula_unr, formula_res,
                                     param, cluster_var, B = 9999, seed = 42) {
  set.seed(seed)
  mod_unr <- lm(formula_unr, data = data)
  mod_res <- lm(formula_res, data = data)
  cl_vcov <- vcovCL(mod_unr, cluster = data[[cluster_var]], type = "HC1")
  ct <- coeftest(mod_unr, vcov. = cl_vcov)
  t_orig <- ct[param, "t value"]
  e_res <- residuals(mod_res)
  y_fit <- fitted(mod_res)
  cluster_ids <- data[[cluster_var]]
  t_boot <- numeric(B)
  for (b in seq_len(B)) {
    w <- sample(webb_weights, G_ate, replace = TRUE)
    w_obs <- w[match(cluster_ids, clusters_ate)]
    boot_data <- data
    boot_data$Z_score <- y_fit + w_obs * e_res
    mod_b <- lm(formula_unr, data = boot_data)
    cl_b <- tryCatch(vcovCL(mod_b, cluster = boot_data[[cluster_var]], type = "HC1"),
                     error = function(e) NULL)
    if (!is.null(cl_b)) {
      t_boot[b] <- coeftest(mod_b, vcov. = cl_b)[param, "t value"]
    } else { t_boot[b] <- NA }
  }
  t_valid <- t_boot[!is.na(t_boot)]
  list(t_orig = t_orig, p_wild = mean(abs(t_valid) >= abs(t_orig)),
       B_valid = length(t_valid), estimate = coef(mod_unr)[param])
}

# Simple model
mod_simple <- lm(Z_score ~ FD + GDP, data = ate_data)
message(paste("   Simple model: Z ~ FD + GDP, N =", nrow(ate_data),
              ", G =", G_ate))

res_simple <- wild_cluster_bootstrap_t(
  ate_data, Z_score ~ FD + GDP, Z_score ~ GDP,
  "FD", "country_code", B = 9999)
message(paste("   Simple: wild bootstrap p =", round(res_simple$p_wild, 4)))

# Full model
res_full <- wild_cluster_bootstrap_t(
  ate_data, Z_score ~ FD + GDP + NPL + NIM + GovDebt,
  Z_score ~ GDP + NPL + NIM + GovDebt, "FD", "country_code", B = 9999)
message(paste("   Full:   wild bootstrap p =", round(res_full$p_wild, 4)))

wild_results <- data.frame(
  model = c("Z ~ FD + GDP", "Z ~ FD + GDP + NPL + NIM + GovDebt"),
  FD_estimate = c(res_simple$estimate, res_full$estimate),
  wild_p = c(res_simple$p_wild, res_full$p_wild),
  B = 9999, G = G_ate, N = nrow(ate_data), weight_type = "Webb"
)
write_csv(wild_results, file.path(out_dir, "wild_bootstrap_results.csv"))
message("   Saved: wild_bootstrap_results.csv")

# =============================================================================
# KR6: Ibragimov-Müller (2010) Test
# Separate regression per cluster, t-test on coefficients
# Robust to heteroskedasticity + within-cluster dependence
# =============================================================================
message("\n>>> KR6: Ibragimov-Müller Test...")

library(broom)

im_by_country <- ate_data %>%
  group_by(country_code) %>%
  filter(n() >= 3) %>%
  do(tidy(lm(Z_score ~ FD + GDP, data = .))) %>%
  filter(term == "FD") %>%
  ungroup()

message(paste("   Countries with >= 3 obs:", nrow(im_by_country)))
message("   Per-country FD coefficients:")
for (i in seq_len(nrow(im_by_country))) {
  message(paste("    ", im_by_country$country_code[i], ":",
                round(im_by_country$estimate[i], 4)))
}

im_coefs <- im_by_country$estimate

if (length(im_coefs) >= 3) {
  im_test <- t.test(im_coefs, mu = 0)
  message(paste("   IM t-statistic:", round(im_test$statistic, 4)))
  message(paste("   IM p-value:", round(im_test$p.value, 4)))
  message(paste("   IM 95% CI: [", round(im_test$conf.int[1], 4),
                ",", round(im_test$conf.int[2], 4), "]"))
  message(paste("   IM df:", im_test$parameter))

  im_results <- data.frame(
    method = "Ibragimov-Mueller (2010)",
    mean_coef = mean(im_coefs),
    median_coef = median(im_coefs),
    t_stat = as.numeric(im_test$statistic),
    p_value = im_test$p.value,
    ci_lower = im_test$conf.int[1],
    ci_upper = im_test$conf.int[2],
    df = as.numeric(im_test$parameter),
    G_used = length(im_coefs)
  )
  write_csv(im_results, file.path(out_dir, "ibragimov_muller_results.csv"))
  message("   Saved: ibragimov_muller_results.csv")
} else {
  message("   WARNING: Not enough clusters (< 3) for IM test")
}

message("\n=== [10e] G03: Cluster Bootstrap Complete ===")
message(paste("   Output directory:", out_dir))
