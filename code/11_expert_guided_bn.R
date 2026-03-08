# =============================================================================
# 11_expert_guided_bn.R - Expert-Guided BN with Whitelist
# Purpose: BN structure learning with theoretical priors (whitelist)
# Author: Thanh-Phong Lam
# Created: 2026-02-08, Updated: 2026-02-09 (DI→FD rename)
# Ref: Scutari (2010), Nagarajan et al. (2013), Gemini recommendation
# =============================================================================

source("R/00_setup.R")

message("\n=== [11] Expert-Guided BN Started ===\n")

# --- Load pathway data ---
pathway_std <- readRDS(file.path(OUTPUT_DIR, "pathway_std.rds"))
message(paste("   Data:", nrow(pathway_std), "obs x", ncol(pathway_std), "vars"))

# --- Define Expert Whitelist (forced edges) ---
# Hypothesis: FD -> NIM -> FDI -> NPL -> Z_score
# GDP confounds all pathway variables
wl <- data.frame(
  from = c("FD",  "NIM", "FDI", "NPL",
           "GDP", "GDP", "GDP", "GDP", "GDP"),
  to   = c("NIM", "FDI", "NPL", "Z_score",
           "FD",  "NIM", "FDI", "NPL", "Z_score")
)

message("   Whitelist edges (forced):")
print(wl)

# --- Define Blacklist (forbidden edges) ---
# Z_score cannot cause FD (reverse causality forbidden)
bl <- data.frame(
  from = c("Z_score", "Z_score", "NPL",  "FDI", "CPI"),
  to   = c("FD",      "GDP",     "FD",   "FD",  "Z_score")
)

message("   Blacklist edges (forbidden):")
print(bl)

# --- Structure Learning: Hill-Climbing with constraints ---
message(">>> HC with expert priors...")
dag_expert <- hc(pathway_std, score = "bic-g",
                 whitelist = wl, blacklist = bl)

# --- Save structure ---
saveRDS(dag_expert, file.path(OUTPUT_DIR, "dag_expert.rds"))
expert_edges <- as.data.frame(arcs(dag_expert))
write_csv(expert_edges, file.path(OUTPUT_DIR, "dag_expert_edges.csv"))
message(paste("   Expert DAG edges:", nrow(expert_edges)))
print(expert_edges)

# --- Plot DAG ---
png(file.path(OUTPUT_DIR, "dag_expert_pathway.png"), width = 2000, height = 1600, res = 300)
graphviz.plot(dag_expert, main = "Expert-Guided BN: Financial Depth Pathway")
dev.off()
message("   Saved: dag_expert_pathway.png")

# --- Compare with unconstrained HC ---
message(">>> Unconstrained HC for comparison...")
dag_free <- hc(pathway_std, score = "bic-g")
free_edges <- as.data.frame(arcs(dag_free))
write_csv(free_edges, file.path(OUTPUT_DIR, "dag_free_edges.csv"))

# --- BIC comparison ---
fitted_expert <- bn.fit(dag_expert, pathway_std)
fitted_free   <- bn.fit(dag_free, pathway_std)

bic_expert <- BIC(fitted_expert, pathway_std)
bic_free   <- BIC(fitted_free, pathway_std)

message(paste("   BIC expert:", round(bic_expert, 2)))
message(paste("   BIC free:  ", round(bic_free, 2)))
message(paste("   Delta BIC: ", round(bic_expert - bic_free, 2)))

comparison <- data.frame(
  model = c("Expert-Guided", "Unconstrained"),
  n_edges = c(nrow(expert_edges), nrow(free_edges)),
  BIC = c(bic_expert, bic_free)
)
write_csv(comparison, file.path(OUTPUT_DIR, "dag_comparison.csv"))

message("\n=== [11] Expert-Guided BN Complete ===")
