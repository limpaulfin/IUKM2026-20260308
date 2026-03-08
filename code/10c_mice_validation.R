# =============================================================================
# 10c_mice_validation.R - G02 MICE Validation & Diagnostics
# Purpose: Convergence, MCAR test, missingness map, complete-case comparison
# Author: Thanh-Phong Lam
# Created: 2026-02-10
# Ref: van Buuren (2018) Flexible Imputation of Missing Data, Ch.6
# G02 KRs: KR1 Rubin pooling, KR2 complete-case, KR3 MCAR, KR4 missingness map
# =============================================================================

source("R/00_setup.R")

# Install naniar if missing (for Little's MCAR test)
if (!requireNamespace("naniar", quietly = TRUE)) {
  install.packages("naniar", repos = "https://cloud.r-project.org")
}
library(naniar)

message("\n=== [10c] MICE Validation & Diagnostics ===\n")

# --- Output directory for validation ---
VAL_DIR <- file.path(OUTPUT_DIR, "mice_validation")
if (!dir.exists(VAL_DIR)) dir.create(VAL_DIR, recursive = TRUE)

# --- Load MICE object and raw panel ---
mice_out <- readRDS(file.path(OUTPUT_DIR, "mice_imputation.rds"))
bn_raw <- read_csv(file.path(OUTPUT_DIR, "panel_merged.csv"), show_col_types = FALSE)

# Variable mapping (same as 10b)
var_map <- c(
  "Z_score" = "GFDD.SI.01", "NPL" = "GFDD.SI.02",
  "NIM" = "GFDD.EI.01", "FD" = "GFDD.DI.12",
  "GDP" = "NY.GDP.PCAP.CD", "GovDebt" = "GC.DOD.TOTL.GD.ZS",
  "FDI" = "BX.KLT.DINV.WD.GD.ZS", "CPI" = "FP.CPI.TOTL.ZG"
)

# Reconstruct pre-imputation data (same filter as 10b)
pathway_pre_all <- bn_raw %>% select(all_of(var_map))
n_observed <- rowSums(!is.na(pathway_pre_all))
pathway_pre <- pathway_pre_all %>% filter(n_observed >= 4)

message(paste("   Pre-imputation rows:", nrow(pathway_pre)))
message(paste("   Complete cases:", sum(complete.cases(pathway_pre))))

# =====================================================================
# KR3: Little's MCAR Test
# =====================================================================
message("\n>>> KR3: Little's MCAR Test")

mcar_result <- tryCatch({
  naniar::mcar_test(pathway_pre)
}, error = function(e) {
  message(paste("   MCAR test error:", e$message))
  NULL
})

if (!is.null(mcar_result)) {
  message(paste("   Test statistic:", round(mcar_result$statistic, 2)))
  message(paste("   df:", mcar_result$df))
  message(paste("   p-value:", format.pval(mcar_result$p.value, digits = 4)))
  if (mcar_result$p.value < 0.05) {
    message("   >> MCAR REJECTED: Data is NOT missing completely at random")
    message("   >> MAR assumption justified for MICE")
  } else {
    message("   >> MCAR not rejected: Data may be MCAR")
  }
  # Save result
  mcar_df <- data.frame(
    statistic = mcar_result$statistic,
    df = mcar_result$df,
    p_value = mcar_result$p.value,
    conclusion = ifelse(mcar_result$p.value < 0.05, "MCAR rejected -> MAR", "MCAR not rejected")
  )
  write_csv(mcar_df, file.path(VAL_DIR, "mcar_test_result.csv"))
}

# =====================================================================
# KR4: Missingness Map (variable × proportion)
# =====================================================================
message("\n>>> KR4: Missingness Map")

# Per-variable missingness
miss_by_var <- data.frame(
  variable = names(pathway_pre),
  n_total = nrow(pathway_pre),
  n_missing = colSums(is.na(pathway_pre)),
  pct_missing = round(colSums(is.na(pathway_pre)) / nrow(pathway_pre) * 100, 1)
)
miss_by_var <- miss_by_var %>% arrange(desc(pct_missing))
message("   Missingness by variable:")
print(miss_by_var)
write_csv(miss_by_var, file.path(VAL_DIR, "missingness_by_variable.csv"))

# Missingness pattern plot
png(file.path(VAL_DIR, "missingness_pattern.png"), width = 800, height = 600)
md.pattern(pathway_pre, rotate.names = TRUE)
dev.off()
message("   Saved: missingness_pattern.png")

# Per-country missingness (need country info)
# Add country_code back for this analysis
pathway_with_country <- bn_raw %>%
  select(country_code, all_of(var_map)) %>%
  filter(rowSums(!is.na(select(., -country_code))) >= 4)

pathway_with_country$is_complete <- complete.cases(
  pathway_with_country %>% select(-country_code)
)
miss_by_country <- pathway_with_country %>%
  group_by(country_code) %>%
  summarise(
    n_obs = n(),
    n_complete = sum(is_complete),
    pct_complete = round(n_complete / n_obs * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(pct_complete)
message("   Missingness by country:")
print(miss_by_country)
write_csv(miss_by_country, file.path(VAL_DIR, "missingness_by_country.csv"))

# =====================================================================
# CONVERGENCE: Trace plots
# =====================================================================
message("\n>>> Convergence Trace Plots")

png(file.path(VAL_DIR, "mice_convergence_traces.png"), width = 1200, height = 800)
plot(mice_out)
dev.off()
message("   Saved: mice_convergence_traces.png")

# =====================================================================
# KR2: Complete-Case Robustness Check
# =====================================================================
message("\n>>> KR2: Complete-Case vs MICE Comparison")

# Complete-case data
cc_data <- pathway_pre %>% filter(complete.cases(.))
n_cc <- nrow(cc_data)
message(paste("   Complete-case N:", n_cc))

# MICE imputed (first dataset)
mice_data <- complete(mice_out, action = 1)
n_mice <- nrow(mice_data)
message(paste("   MICE N:", n_mice))

# Compare summary statistics
compare_stats <- data.frame(variable = names(pathway_pre))
for (v in names(pathway_pre)) {
  compare_stats$cc_mean[compare_stats$variable == v] <- mean(cc_data[[v]], na.rm = TRUE)
  compare_stats$cc_sd[compare_stats$variable == v] <- sd(cc_data[[v]], na.rm = TRUE)
  compare_stats$mice_mean[compare_stats$variable == v] <- mean(mice_data[[v]])
  compare_stats$mice_sd[compare_stats$variable == v] <- sd(mice_data[[v]])
}
compare_stats$mean_diff_pct <- round((compare_stats$mice_mean - compare_stats$cc_mean) /
                                       compare_stats$cc_mean * 100, 2)
compare_stats$sd_diff_pct <- round((compare_stats$mice_sd - compare_stats$cc_sd) /
                                     compare_stats$cc_sd * 100, 2)

message("   Mean comparison (MICE vs complete-case):")
print(compare_stats %>% select(variable, cc_mean, mice_mean, mean_diff_pct))
write_csv(compare_stats, file.path(VAL_DIR, "cc_vs_mice_comparison.csv"))

# Compare correlations
cc_cor <- cor(cc_data)
mice_cor <- cor(mice_data)
cor_diff <- mice_cor - cc_cor
max_cor_diff <- max(abs(cor_diff))
message(paste("   Max correlation difference:", round(max_cor_diff, 4)))

# Save correlation comparison
write_csv(as.data.frame(cor_diff), file.path(VAL_DIR, "correlation_diff_mice_vs_cc.csv"))

# Complete-case BN structure (PC algorithm)
message("   Running PC algorithm on complete-case data...")
cc_std <- cc_data %>% mutate(across(everything(), ~ scale(.x)[, 1]))
pc_cc <- tryCatch({
  pc.stable(cc_std, undirected = FALSE, alpha = 0.05, test = "cor")
}, error = function(e) {
  message(paste("   PC on cc failed:", e$message))
  NULL
})

# MICE BN structure (PC algorithm)
mice_std <- mice_data %>% mutate(across(everything(), ~ scale(.x)[, 1]))
pc_mice <- tryCatch({
  pc.stable(mice_std, undirected = FALSE, alpha = 0.05, test = "cor")
}, error = function(e) {
  message(paste("   PC on mice failed:", e$message))
  NULL
})

if (!is.null(pc_cc) && !is.null(pc_mice)) {
  n_edges_cc <- narcs(pc_cc)
  n_edges_mice <- narcs(pc_mice)
  message(paste("   PC edges (complete-case N=", n_cc, "):", n_edges_cc))
  message(paste("   PC edges (MICE N=", n_mice, "):", n_edges_mice))

  # Compare edge sets
  edges_cc <- arcs(pc_cc)
  edges_mice <- arcs(pc_mice)

  edges_cc_str <- paste(edges_cc[, 1], "->", edges_cc[, 2])
  edges_mice_str <- paste(edges_mice[, 1], "->", edges_mice[, 2])

  shared <- intersect(edges_cc_str, edges_mice_str)
  only_cc <- setdiff(edges_cc_str, edges_mice_str)
  only_mice <- setdiff(edges_mice_str, edges_cc_str)

  message(paste("   Shared edges:", length(shared)))
  message(paste("   Only in complete-case:", length(only_cc)))
  if (length(only_cc) > 0) message(paste("     ", only_cc))
  message(paste("   Only in MICE:", length(only_mice)))
  if (length(only_mice) > 0) message(paste("     ", only_mice))

  bn_comparison <- data.frame(
    metric = c("N", "PC_edges", "shared_edges", "only_this_method"),
    complete_case = c(n_cc, n_edges_cc, length(shared), length(only_cc)),
    mice_imputed = c(n_mice, n_edges_mice, length(shared), length(only_mice))
  )
  write_csv(bn_comparison, file.path(VAL_DIR, "bn_structure_cc_vs_mice.csv"))
}

# =====================================================================
# KR1: Rubin's Rules Pooled Estimates (BF t-test proxy)
# =====================================================================
message("\n>>> KR1: Rubin's Rules Pooled Regression (ATE proxy)")

# Run regression on each imputed dataset and pool
# ATE model: Z_score ~ FD + GDP + GovDebt + FDI + CPI
pooled_result <- tryCatch({
  fit_list <- with(mice_out, lm(Z_score ~ FD + GDP + GovDebt + FDI + CPI))
  pooled <- pool(fit_list)
  summary(pooled)
}, error = function(e) {
  message(paste("   Pooling error:", e$message))
  NULL
})

if (!is.null(pooled_result)) {
  message("   Rubin's Rules pooled regression (Z ~ FD + GDP + ...):")
  print(pooled_result)
  write_csv(as.data.frame(pooled_result), file.path(VAL_DIR, "rubin_pooled_regression.csv"))

  # Extract FD coefficient for ATE comparison
  fd_row <- pooled_result[pooled_result$term == "FD", ]
  if (nrow(fd_row) > 0) {
    message(paste("\n   FD coefficient (pooled):", round(fd_row$estimate, 4)))
    message(paste("   SE:", round(fd_row$std.error, 4)))
    message(paste("   t:", round(fd_row$statistic, 4)))
    message(paste("   p:", format.pval(fd_row$p.value, digits = 4)))
    if ("fmi" %in% names(fd_row)) {
      message(paste("   FMI (fraction missing info):", round(fd_row$fmi, 4)))
    }
  }
}

# =====================================================================
# KR5: Sensitivity — 30/50/70% missingness thresholds
# =====================================================================
message("\n>>> KR5: Sensitivity — Variable exclusion thresholds")

thresholds <- c(0.30, 0.50, 0.70)
sensitivity_results <- list()

for (thresh in thresholds) {
  vars_above <- miss_by_var$variable[miss_by_var$pct_missing / 100 > thresh]
  vars_kept <- setdiff(names(pathway_pre), vars_above)
  n_vars_kept <- length(vars_kept)

  if (n_vars_kept >= 4) {
    sub_data <- pathway_pre %>% select(all_of(vars_kept)) %>% filter(complete.cases(.))
    n_sub <- nrow(sub_data)
  } else {
    n_sub <- NA
  }

  sensitivity_results[[as.character(thresh)]] <- data.frame(
    threshold_pct = thresh * 100,
    vars_excluded = paste(vars_above, collapse = ", "),
    n_vars_excluded = length(vars_above),
    n_vars_kept = n_vars_kept,
    n_complete_obs = n_sub
  )
  message(paste("   Threshold", thresh * 100, "% -> excluded:",
                paste(vars_above, collapse = ", "),
                "| N complete:", n_sub))
}

sens_df <- bind_rows(sensitivity_results)
write_csv(sens_df, file.path(VAL_DIR, "sensitivity_thresholds.csv"))

# =====================================================================
# KR1 continued: Compare BN across m=5 imputations
# =====================================================================
message("\n>>> KR1: BN stability across m=5 imputations")

edge_counts <- list()
for (m_idx in 1:5) {
  imp_m <- complete(mice_out, action = m_idx)
  imp_std <- imp_m %>% mutate(across(everything(), ~ scale(.x)[, 1]))

  pc_m <- tryCatch({
    pc.stable(imp_std, undirected = FALSE, alpha = 0.05, test = "cor")
  }, error = function(e) NULL)

  if (!is.null(pc_m)) {
    edges_m <- arcs(pc_m)
    edge_str <- paste(edges_m[, 1], "->", edges_m[, 2])
    edge_counts[[m_idx]] <- data.frame(
      imputation = m_idx,
      n_edges = narcs(pc_m),
      edges = paste(edge_str, collapse = "; ")
    )
    message(paste("   m=", m_idx, ": PC edges =", narcs(pc_m)))
  }
}

if (length(edge_counts) > 0) {
  edge_stability <- bind_rows(edge_counts)
  write_csv(edge_stability, file.path(VAL_DIR, "bn_stability_across_imputations.csv"))

  # Count how often each edge appears across m=5
  all_edge_strs <- unlist(lapply(edge_counts, function(x) strsplit(x$edges, "; ")[[1]]))
  edge_freq <- as.data.frame(table(all_edge_strs))
  names(edge_freq) <- c("edge", "frequency")
  edge_freq <- edge_freq %>% arrange(desc(frequency))
  message("\n   Edge frequency across m=5 imputations:")
  print(edge_freq)
  write_csv(edge_freq, file.path(VAL_DIR, "edge_frequency_m5.csv"))
}

# =====================================================================
# Summary
# =====================================================================
message("\n=== G02 MICE Validation Summary ===")
message(paste("   Output directory:", VAL_DIR))
message("   Files generated:")
message("   - mcar_test_result.csv")
message("   - missingness_by_variable.csv")
message("   - missingness_by_country.csv")
message("   - missingness_pattern.png")
message("   - mice_convergence_traces.png")
message("   - cc_vs_mice_comparison.csv")
message("   - correlation_diff_mice_vs_cc.csv")
message("   - bn_structure_cc_vs_mice.csv")
message("   - rubin_pooled_regression.csv")
message("   - sensitivity_thresholds.csv")
message("   - bn_stability_across_imputations.csv")
message("   - edge_frequency_m5.csv")
message("\n=== [10c] MICE Validation Complete ===")
