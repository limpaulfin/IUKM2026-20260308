# =============================================================================
# 05b_bn_bootstrap.R - Bootstrap Validation for BN Structure
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-01-27
# Purpose: Validate structure learning with bootstrap resampling
# Reference: Scutari M. (2010). bnlearn R package
# =============================================================================

source("R/00_setup.R")

message("\n=== [05b] BN Bootstrap Validation Started ===\n")

# --- Load BN data ---
message(">>> Loading bn_data.csv...")
bn_data <- read_csv(file.path(OUTPUT_DIR, "bn_data.csv"), show_col_types = FALSE) %>%
  select(where(is.numeric)) %>%
  na.omit()

# Match variables with existing structure
hc_structure <- readRDS(file.path(OUTPUT_DIR, "bn_structure_hc.rds"))
bn_nodes <- nodes(hc_structure)
bn_data <- as.data.frame(bn_data %>% select(any_of(bn_nodes)))

message(paste("   Complete cases:", nrow(bn_data)))
message(paste("   Variables:", ncol(bn_data)))

# --- Define Blacklist (causally impossible edges) ---
message(">>> Defining blacklist (impossible edges)...")
# Year cannot be caused by any financial variable
# Financial outcomes cannot cause temporal index
blacklist <- data.frame(
  from = setdiff(names(bn_data), "year"),
  to = "year"
)
message(paste("   Blacklisted edges:", nrow(blacklist)))

# --- Bootstrap Structure Learning ---
message(">>> Running bootstrap (R=200, may take time)...")
set.seed(42)  # Reproducibility

boot_strength <- tryCatch({
  boot.strength(
    data = bn_data,
    R = 200,
    algorithm = "hc",
    algorithm.args = list(score = "bic-g", blacklist = blacklist)
  )
}, error = function(e) {
  message(paste("   Bootstrap error:", e$message))
  NULL
})

if (!is.null(boot_strength)) {
  # Save bootstrap results
  write_csv(boot_strength, file.path(OUTPUT_DIR, "bn_bootstrap_strength.csv"))
  message("   Saved: bn_bootstrap_strength.csv")

  # --- Averaged Network ---
  message(">>> Computing averaged network (threshold 0.5)...")
  avg_net <- averaged.network(boot_strength, threshold = 0.5)

  saveRDS(avg_net, file.path(OUTPUT_DIR, "bn_structure_averaged.rds"))
  avg_edges <- arcs(avg_net)
  write_csv(as.data.frame(avg_edges), file.path(OUTPUT_DIR, "bn_edges_averaged.csv"))
  message(paste("   Averaged edges:", nrow(avg_edges)))

  # Plot averaged DAG
  png(file.path(OUTPUT_DIR, "bn_dag_averaged.png"), width = 1200, height = 1000, res = 120)
  graphviz.plot(avg_net, main = "BN Structure (Bootstrap Averaged, threshold=0.5)")
  dev.off()
  message("   Saved: bn_dag_averaged.png")

  # --- Edge Strength Summary ---
  message(">>> Summarizing edge strengths...")
  strong_edges <- boot_strength %>%
    filter(strength >= 0.5) %>%
    arrange(desc(strength))

  message(paste("   Strong edges (>=0.5):", nrow(strong_edges)))
  print(head(strong_edges, 10))

  # --- Comparison with original HC ---
  message(">>> Comparing with original HC structure...")
  hc_edges <- as.data.frame(arcs(hc_structure))
  comparison <- data.frame(
    method = c("Original HC", "Bootstrap Averaged"),
    n_edges = c(nrow(hc_edges), nrow(avg_edges))
  )
  write_csv(comparison, file.path(OUTPUT_DIR, "bn_bootstrap_comparison.csv"))
  print(comparison)
}

message("\n=== [05b] BN Bootstrap Validation Complete ===")
