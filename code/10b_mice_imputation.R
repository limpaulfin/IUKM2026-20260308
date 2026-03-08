# =============================================================================
# 10b_mice_imputation.R - MICE Multiple Imputation for Pathway Data
# Purpose: Replace na.omit() with MICE to recover missing observations
# Author: Thanh-Phong Lam
# Created: 2026-02-09
# Ref: van Buuren & Groothuis-Oudshoorn (2011), mice package
# =============================================================================

source("R/00_setup.R")

message("\n=== [10b] MICE Imputation Started ===\n")

# --- Load panel_merged (BEFORE na.omit, maximum observations) ---
bn_raw <- read_csv(file.path(OUTPUT_DIR, "panel_merged.csv"), show_col_types = FALSE)
message(paste("   Raw panel data:", nrow(bn_raw), "obs x", ncol(bn_raw), "vars"))

# --- Select 8 key variables (same as script 10) ---
# Pathway: FD -> NIM -> FDI -> NPL -> Z_score (control: GDP, GovDebt, CPI)
var_map <- c(
  "Z_score"  = "GFDD.SI.01",
  "NPL"      = "GFDD.SI.02",
  "NIM"      = "GFDD.EI.01",
  "FD"       = "GFDD.DI.12",
  "GDP"      = "NY.GDP.PCAP.CD",
  "GovDebt"  = "GC.DOD.TOTL.GD.ZS",
  "FDI"      = "BX.KLT.DINV.WD.GD.ZS",
  "CPI"      = "FP.CPI.TOTL.ZG"
)

# Check all variables exist
missing_vars <- setdiff(var_map, colnames(bn_raw))
if (length(missing_vars) > 0) {
  stop("Missing variables: ", paste(missing_vars, collapse = ", "))
}

# Select and rename (DO NOT na.omit yet)
pathway_pre_all <- bn_raw %>% select(all_of(var_map))
message(paste("   All rows:", nrow(pathway_pre_all), "obs"))

# Filter: keep rows with at least 4/8 vars non-missing (50% threshold)
# MICE requires sufficient observed data per row for reliable imputation
n_vars <- ncol(pathway_pre_all)
n_observed <- rowSums(!is.na(pathway_pre_all))
pathway_pre <- pathway_pre_all %>% filter(n_observed >= ceiling(n_vars / 2))
message(paste("   After 50% filter:", nrow(pathway_pre), "obs (kept rows with >=4/8 vars)"))
message(paste("   Missing pattern:"))
print(colSums(is.na(pathway_pre)))

n_complete <- sum(complete.cases(pathway_pre))
message(paste("   Complete cases (na.omit equivalent):", n_complete))

# --- MICE imputation (m=5, predictive mean matching) ---
message(">>> Running MICE (m=5, method=pmm)... this may take a moment")
set.seed(42)
mice_out <- mice(pathway_pre, m = 5, method = "pmm", maxit = 50,
                 printFlag = FALSE)

# --- Diagnostics ---
message(">>> MICE diagnostics:")
message(paste("   Iterations:", mice_out$iteration))
message(paste("   Methods:", paste(mice_out$method, collapse = ", ")))

# --- Pool: use first completed dataset for BN (deterministic) ---
# For BN structure learning, we use the first imputed dataset
# (pooling across m datasets is for regression, not BN)
pathway_imputed <- complete(mice_out, action = 1)
message(paste("   After MICE:", nrow(pathway_imputed), "obs (0 missing)"))
message(paste("   Recovered:", nrow(pathway_imputed) - n_complete, "obs from imputation"))

# --- Standardize (z-scores) ---
pathway_std <- pathway_imputed %>%
  mutate(across(everything(), ~ scale(.x)[, 1]))

# --- Save outputs (same names as script 10 for pipeline compatibility) ---
write_csv(pathway_imputed, file.path(OUTPUT_DIR, "pathway_data_raw.csv"))
write_csv(pathway_std, file.path(OUTPUT_DIR, "pathway_data_std.csv"))
saveRDS(pathway_imputed, file.path(OUTPUT_DIR, "pathway_data.rds"))
saveRDS(pathway_std, file.path(OUTPUT_DIR, "pathway_std.rds"))

# --- Save MICE object for reproducibility ---
saveRDS(mice_out, file.path(OUTPUT_DIR, "mice_imputation.rds"))

# --- Summary stats ---
message("\n--- Summary Statistics (imputed) ---")
summary_stats <- pathway_imputed %>%
  summarise(across(everything(), list(
    mean = ~mean(.x), sd = ~sd(.x), min = ~min(.x), max = ~max(.x)
  ))) %>%
  pivot_longer(everything(), names_to = c("var", "stat"), names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value)

print(summary_stats)
write_csv(summary_stats, file.path(OUTPUT_DIR, "pathway_summary_stats.csv"))

# --- Compare: na.omit vs MICE ---
message("\n--- Comparison: na.omit vs MICE ---")
message(paste("   na.omit N:", n_complete))
message(paste("   MICE N:   ", nrow(pathway_imputed)))
message(paste("   Recovered:", nrow(pathway_imputed) - n_complete, "observations"))

message("\n=== [10b] MICE Imputation Complete ===")
