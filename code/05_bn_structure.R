# =============================================================================
# 05_bn_structure.R - Bayesian Network Structure Learning
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-01-27
# Source: bnlearn package - score-based and constraint-based methods
# Reference: [Low-Rank Models in Visual Analysis, 2020] - BN structure learning
# =============================================================================

source("R/00_setup.R")

message("\n=== [05] BN Structure Learning Started ===\n")

# --- Load BN data ---
message(">>> Loading bn_data.csv...")
bn_data <- read_csv(file.path(OUTPUT_DIR, "bn_data.csv"), show_col_types = FALSE)

# Ensure numeric and complete cases
bn_data <- bn_data %>%
  select(where(is.numeric)) %>%
  na.omit()

message(paste("   Complete cases:", nrow(bn_data)))
message(paste("   Variables:", ncol(bn_data)))

# Check minimum sample size
if (nrow(bn_data) < 30) {
  stop("ERROR: Insufficient data for BN structure learning (need >= 30 rows)")
}

# Limit variables for computational feasibility
MAX_VARS <- 15
if (ncol(bn_data) > MAX_VARS) {
  message(paste(">>> Selecting top", MAX_VARS, "variables by variance..."))
  top_vars <- bn_data %>%
    summarise(across(everything(), var, na.rm = TRUE)) %>%
    pivot_longer(everything(), names_to = "var", values_to = "variance") %>%
    arrange(desc(variance)) %>%
    head(MAX_VARS) %>%
    pull(var)
  bn_data <- bn_data %>% select(all_of(top_vars))
  message(paste("   Selected variables:", paste(top_vars, collapse = ", ")))
}

# --- Method 1: Hill-Climbing (Score-based) ---
message(">>> Learning structure with Hill-Climbing (BIC score)...")
hc_bn <- tryCatch({
  hc(bn_data, score = "bic-g")
}, error = function(e) {
  message(paste("   Hill-climbing error:", e$message))
  NULL
})

if (!is.null(hc_bn)) {
  # Save structure
  saveRDS(hc_bn, file.path(OUTPUT_DIR, "bn_structure_hc.rds"))

  # Export edges as CSV
  hc_edges <- arcs(hc_bn)
  write_csv(as.data.frame(hc_edges), file.path(OUTPUT_DIR, "bn_edges_hc.csv"))
  message(paste("   HC edges found:", nrow(hc_edges)))

  # Plot DAG
  png(file.path(OUTPUT_DIR, "bn_dag_hc.png"), width = 1000, height = 800)
  graphviz.plot(hc_bn, main = "BN Structure (Hill-Climbing)")
  dev.off()
  message("   Saved: bn_dag_hc.png")
}

# --- Method 2: PC Algorithm (Constraint-based) ---
message(">>> Learning structure with PC algorithm...")
pc_bn <- tryCatch({
  pc.stable(bn_data, alpha = 0.05)
}, error = function(e) {
  message(paste("   PC algorithm error:", e$message))
  NULL
})

if (!is.null(pc_bn)) {
  saveRDS(pc_bn, file.path(OUTPUT_DIR, "bn_structure_pc.rds"))
  pc_edges <- arcs(pc_bn)
  write_csv(as.data.frame(pc_edges), file.path(OUTPUT_DIR, "bn_edges_pc.csv"))
  message(paste("   PC edges found:", nrow(pc_edges)))
}

# --- Method 3: Tabu Search ---
message(">>> Learning structure with Tabu search...")
tabu_bn <- tryCatch({
  tabu(bn_data, score = "bic-g")
}, error = function(e) {
  message(paste("   Tabu search error:", e$message))
  NULL
})

if (!is.null(tabu_bn)) {
  saveRDS(tabu_bn, file.path(OUTPUT_DIR, "bn_structure_tabu.rds"))
  tabu_edges <- arcs(tabu_bn)
  write_csv(as.data.frame(tabu_edges), file.path(OUTPUT_DIR, "bn_edges_tabu.csv"))
  message(paste("   Tabu edges found:", nrow(tabu_edges)))
}

# --- Compare methods ---
message(">>> Comparing structure learning methods...")
comparison <- data.frame(
  method = c("Hill-Climbing", "PC Algorithm", "Tabu Search"),
  n_edges = c(
    ifelse(!is.null(hc_bn), nrow(arcs(hc_bn)), NA),
    ifelse(!is.null(pc_bn), nrow(arcs(pc_bn)), NA),
    ifelse(!is.null(tabu_bn), nrow(arcs(tabu_bn)), NA)
  )
)
write_csv(comparison, file.path(OUTPUT_DIR, "bn_structure_comparison.csv"))

message("\n=== [05] BN Structure Learning Complete ===")
print(comparison)
