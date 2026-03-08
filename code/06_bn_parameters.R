# =============================================================================
# 06_bn_parameters.R - Bayesian Network Parameter Learning
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-01-27
# =============================================================================

source("R/00_setup.R")

message("\n=== [06] BN Parameter Learning Started ===\n")

# --- Load BN data and structure ---
message(">>> Loading data and structure...")
bn_data <- read_csv(file.path(OUTPUT_DIR, "bn_data.csv"), show_col_types = FALSE) %>%
  select(where(is.numeric)) %>%
  na.omit()

# Load best structure (prefer Hill-Climbing)
structure_file <- file.path(OUTPUT_DIR, "bn_structure_hc.rds")
if (!file.exists(structure_file)) {
  structure_file <- file.path(OUTPUT_DIR, "bn_structure_tabu.rds")
}
if (!file.exists(structure_file)) {
  stop("ERROR: No BN structure found. Run 05_bn_structure.R first.")
}

bn_structure <- readRDS(structure_file)
message(paste("   Loaded structure from:", basename(structure_file)))
message(paste("   Edges:", nrow(arcs(bn_structure))))

# Ensure data columns match structure nodes
bn_nodes <- nodes(bn_structure)
bn_data <- bn_data %>% select(any_of(bn_nodes))
message(paste("   Matching variables:", ncol(bn_data)))

# --- Parameter Learning ---
# For continuous data, bnlearn uses Gaussian BN (linear regression)
# No need to specify method for continuous data - it auto-detects
message(">>> Fitting parameters (Gaussian BN for continuous data)...")
bn_fitted <- tryCatch({
  # Convert to data.frame (bnlearn requires this)
  bn_data_df <- as.data.frame(bn_data)
  bn.fit(bn_structure, bn_data_df)
}, error = function(e) {
  message(paste("   Fitting error:", e$message))
  NULL
})

if (!is.null(bn_fitted)) {
  # Save fitted model
  saveRDS(bn_fitted, file.path(OUTPUT_DIR, "bn_fitted.rds"))
  message("   Saved: bn_fitted.rds")

  # Extract and save coefficients
  message(">>> Extracting model coefficients...")
  coef_list <- list()
  for (node in names(bn_fitted)) {
    node_coef <- coef(bn_fitted[[node]])
    if (length(node_coef) > 0) {
      coef_list[[node]] <- data.frame(
        node = node,
        coefficient = names(node_coef),
        value = as.numeric(node_coef)
      )
    }
  }

  if (length(coef_list) > 0) {
    coef_df <- bind_rows(coef_list)
    write_csv(coef_df, file.path(OUTPUT_DIR, "bn_coefficients.csv"))
    message("   Saved: bn_coefficients.csv")
    message(paste("   Total coefficients:", nrow(coef_df)))
  }

  # Extract conditional probability summaries
  message(">>> Extracting CPD summaries...")
  cpd_summary <- list()
  for (node in names(bn_fitted)) {
    fit_node <- bn_fitted[[node]]
    cpd_summary[[node]] <- data.frame(
      node = node,
      parents = paste(fit_node$parents, collapse = ", "),
      n_params = length(coef(fit_node)),
      residual_sd = ifelse(is.null(fit_node$sd), NA, fit_node$sd)
    )
  }
  cpd_df <- bind_rows(cpd_summary)
  write_csv(cpd_df, file.path(OUTPUT_DIR, "bn_cpd_summary.csv"))
  message("   Saved: bn_cpd_summary.csv")
}

message("\n=== [06] BN Parameter Learning Complete ===")
