# =============================================================================
# 13_mediation_bootstrap.R - Bootstrap Indirect Effect + Edge Stability
# Purpose: Bootstrap CI for indirect pathway + edge inclusion probability
# Author: Thanh-Phong Lam
# Created: 2026-02-08, Updated: 2026-02-09 (DI→FD rename)
# Ref: Friedman et al. (1999), Scutari (2010), Colombo & Maathuis (2014)
# =============================================================================

source("R/00_setup.R")

message("\n=== [13] Mediation Bootstrap Started ===\n")

# --- Load data ---
pathway_std <- readRDS(file.path(OUTPUT_DIR, "pathway_std.rds"))
dag_expert  <- readRDS(file.path(OUTPUT_DIR, "dag_expert.rds"))

# --- Whitelist (same as script 11) ---
wl <- data.frame(
  from = c("FD", "NIM", "FDI", "NPL", "GDP", "GDP", "GDP", "GDP", "GDP"),
  to   = c("NIM", "FDI", "NPL", "Z_score", "FD", "NIM", "FDI", "NPL", "Z_score")
)
bl <- data.frame(
  from = c("Z_score", "Z_score", "NPL", "FDI", "CPI"),
  to   = c("FD", "GDP", "FD", "FD", "Z_score")
)

# --- Bootstrap Structure Stability (R=1000) ---
message(">>> Bootstrap structure (R=1000)... this may take a moment")
set.seed(42)
str_boot <- boot.strength(pathway_std, R = 1000, algorithm = "hc",
                          algorithm.args = list(
                            score = "bic-g",
                            whitelist = wl,
                            blacklist = bl
                          ))

# --- Edge Inclusion Probabilities (EIP) ---
message(">>> Edge Inclusion Probabilities for pathway edges:")
pathway_edges <- data.frame(
  from = c("FD", "NIM", "FDI", "NPL"),
  to   = c("NIM", "FDI", "NPL", "Z_score")
)

eip_results <- list()
for (i in 1:nrow(pathway_edges)) {
  f <- pathway_edges$from[i]
  t <- pathway_edges$to[i]
  row <- str_boot[str_boot$from == f & str_boot$to == t, ]
  if (nrow(row) > 0) {
    eip_results[[i]] <- data.frame(
      from = f, to = t,
      strength = row$strength, direction = row$direction
    )
    message(paste("  ", f, "->", t, ": strength=", round(row$strength, 3),
                  "direction=", round(row$direction, 3)))
  }
}
eip_df <- do.call(rbind, eip_results)
write_csv(eip_df, file.path(OUTPUT_DIR, "pathway_eip.csv"))

# --- Bootstrap Indirect Effect (R=1000) ---
message(">>> Bootstrap indirect effect (R=1000)...")
set.seed(42)
n_boot <- 1000
indirect_effects <- numeric(n_boot)

for (b in 1:n_boot) {
  idx <- sample(1:nrow(pathway_std), replace = TRUE)
  boot_data <- pathway_std[idx, ]

  tryCatch({
    boot_fit <- bn.fit(dag_expert, boot_data)
    c1 <- coef(boot_fit$NIM)["FD"]
    c2 <- coef(boot_fit$FDI)["NIM"]
    c3 <- coef(boot_fit$NPL)["FDI"]
    c4 <- coef(boot_fit$Z_score)["NPL"]
    indirect_effects[b] <- c1 * c2 * c3 * c4
  }, error = function(e) {
    indirect_effects[b] <<- NA
  })
}

# Remove NAs
indirect_effects <- indirect_effects[!is.na(indirect_effects)]
message(paste("   Valid bootstrap samples:", length(indirect_effects)))

# --- Bootstrap CI (percentile method) ---
ci_95 <- quantile(indirect_effects, c(0.025, 0.975))
ci_90 <- quantile(indirect_effects, c(0.05, 0.95))

boot_summary <- data.frame(
  statistic = c("mean", "median", "sd", "ci95_lower", "ci95_upper",
                "ci90_lower", "ci90_upper", "n_valid"),
  value = c(mean(indirect_effects), median(indirect_effects),
            sd(indirect_effects), ci_95[1], ci_95[2],
            ci_90[1], ci_90[2], length(indirect_effects))
)

message("\n--- Bootstrap Indirect Effect Summary ---")
print(boot_summary)
write_csv(boot_summary, file.path(OUTPUT_DIR, "bootstrap_indirect_effect.csv"))

# --- Save full bootstrap results ---
saveRDS(str_boot, file.path(OUTPUT_DIR, "bootstrap_structure.rds"))
saveRDS(indirect_effects, file.path(OUTPUT_DIR, "bootstrap_indirect_vector.rds"))

# --- Averaged network (threshold=0.85) ---
avg_net <- averaged.network(str_boot, threshold = 0.85)
saveRDS(avg_net, file.path(OUTPUT_DIR, "averaged_network_085.rds"))
write_csv(as.data.frame(arcs(avg_net)),
          file.path(OUTPUT_DIR, "averaged_network_edges.csv"))

message("\n=== [13] Mediation Bootstrap Complete ===")
