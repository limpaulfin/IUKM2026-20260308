# =============================================================================
# 07_bn_inference.R - Bayesian Network Inference and Hypothesis Testing
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-01-27
# =============================================================================

source("R/00_setup.R")

message("\n=== [07] BN Inference Started ===\n")

# --- Load fitted model ---
message(">>> Loading fitted BN model...")
fitted_file <- file.path(OUTPUT_DIR, "bn_fitted.rds")
if (!file.exists(fitted_file)) {
  stop("ERROR: No fitted BN found. Run 06_bn_parameters.R first.")
}
bn_fitted <- readRDS(fitted_file)
message(paste("   Nodes:", length(names(bn_fitted))))

# --- Load original data for inference ---
bn_data <- read_csv(file.path(OUTPUT_DIR, "bn_data.csv"), show_col_types = FALSE) %>%
  select(where(is.numeric)) %>%
  na.omit() %>%
  select(any_of(names(bn_fitted)))

# --- Network Statistics ---
message(">>> Computing network statistics...")
bn_structure <- bn.net(bn_fitted)

# Calculate degree manually (in-degree + out-degree for each node)
node_degrees <- sapply(nodes(bn_structure), function(n) {
  length(parents(bn_structure, n)) + length(children(bn_structure, n))
})

network_stats <- data.frame(
  metric = c("n_nodes", "n_edges", "avg_degree", "max_in_degree", "max_out_degree"),
  value = c(
    length(nodes(bn_structure)),
    nrow(arcs(bn_structure)),
    mean(node_degrees),
    max(sapply(nodes(bn_structure), function(n) length(parents(bn_structure, n)))),
    max(sapply(nodes(bn_structure), function(n) length(children(bn_structure, n))))
  )
)
write_csv(network_stats, file.path(OUTPUT_DIR, "bn_network_stats.csv"))
message("   Saved: bn_network_stats.csv")

# --- Node Importance (Markov Blanket size) ---
message(">>> Computing node importance...")
node_importance <- data.frame(
  node = nodes(bn_structure),
  n_parents = sapply(nodes(bn_structure), function(n) length(parents(bn_structure, n))),
  n_children = sapply(nodes(bn_structure), function(n) length(children(bn_structure, n))),
  mb_size = sapply(nodes(bn_structure), function(n) length(mb(bn_structure, n)))
) %>%
  arrange(desc(mb_size))

write_csv(node_importance, file.path(OUTPUT_DIR, "bn_node_importance.csv"))
message("   Saved: bn_node_importance.csv")
message("   Top 5 important nodes (by Markov Blanket):")
print(head(node_importance, 5))

# --- Conditional Independence Tests ---
message(">>> Running d-separation tests...")
# Test key relationships relevant to moral hazard hypotheses
dsep_results <- list()
node_list <- names(bn_fitted)

# Sample d-separation tests (first 3 pairs)
if (length(node_list) >= 3) {
  test_pairs <- combn(node_list[1:min(5, length(node_list))], 2, simplify = FALSE)

  for (i in seq_along(test_pairs)) {
    pair <- test_pairs[[i]]
    # Check d-separation given all other variables
    other_vars <- setdiff(node_list, pair)
    is_dsep <- dsep(bn_structure, pair[1], pair[2], other_vars[1:min(2, length(other_vars))])
    dsep_results[[i]] <- data.frame(
      var1 = pair[1],
      var2 = pair[2],
      d_separated = is_dsep
    )
  }

  dsep_df <- bind_rows(dsep_results)
  write_csv(dsep_df, file.path(OUTPUT_DIR, "bn_dsep_tests.csv"))
  message("   Saved: bn_dsep_tests.csv")
}

# --- Model Fit Statistics ---
message(">>> Computing model fit statistics...")
# BIC score
bic_score <- tryCatch({
  score(bn_structure, bn_data, type = "bic-g")
}, error = function(e) NA)

# Log-likelihood
loglik <- tryCatch({
  logLik(bn_fitted, bn_data)
}, error = function(e) NA)

fit_stats <- data.frame(
  metric = c("BIC", "Log-Likelihood", "n_observations", "n_parameters"),
  value = c(
    bic_score,
    as.numeric(loglik),
    nrow(bn_data),
    sum(sapply(names(bn_fitted), function(n) length(coef(bn_fitted[[n]]))))
  )
)
write_csv(fit_stats, file.path(OUTPUT_DIR, "bn_fit_statistics.csv"))
message("   Saved: bn_fit_statistics.csv")

# --- Final Visualization ---
message(">>> Generating final DAG visualization...")
png(file.path(OUTPUT_DIR, "bn_dag_final.png"), width = 1200, height = 1000)
graphviz.plot(bn_structure,
              main = "Bayesian Network: Moral Hazard in ASEAN Banking",
              shape = "ellipse")
dev.off()
message("   Saved: bn_dag_final.png")

message("\n=== [07] BN Inference Complete ===")
message("\nAll output files:")
list.files(OUTPUT_DIR, pattern = "^bn_")
