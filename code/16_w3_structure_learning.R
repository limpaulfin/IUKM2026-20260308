# =============================================================================
# 16_w3_structure_learning.R
# Purpose: Unconstrained (no whitelist / no blacklist) Bayesian-network
#          structure learning on the six Figure-1(b) variables, to test whether
#          the data-driven DAG/CPDAG resembles the proposed indirect pathway
#          FD -> NIM -> FDI -> NPL -> Z (with GDP as confounder).
# Inputs:  data/processed/pathway_std.rds       (285 complete cases, z-scored)
#          data/processed/mice_imputation.rds    (mice mids object, m = 5)
#          Both are produced by the earlier pipeline steps (10*/11* MICE +
#          pathway standardisation). Place them under data/processed/ before
#          running this script.
# Learners: PC (pc.stable, alpha = 0.05) + Hill-Climbing (bic-g) + boot.strength.
# Author:  Thanh-Phong Lam, 2026-07-02. Real merged panel; no simulated data.
# =============================================================================

suppressMessages({
  library(bnlearn)
  library(mice)
})

set.seed(42)

BASE   <- "."
RDIR   <- file.path(BASE, "data", "processed")
OUTDIR <- file.path(BASE, "output", "w3")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

SIX <- c("FD", "NIM", "FDI", "NPL", "Z_score", "GDP")
PROPOSED <- list(c("FD","NIM"), c("NIM","FDI"), c("FDI","NPL"), c("NPL","Z_score"))
proposed_key <- function(a,b) paste(sort(c(a,b)), collapse="~")
PROP_KEYS <- vapply(PROPOSED, function(e) proposed_key(e[1],e[2]), character(1))

ALPHA <- 0.05
R_BOOT <- 500

cc <- readRDS(file.path(RDIR, "pathway_std.rds"))[, SIX]
cc <- as.data.frame(lapply(cc, as.numeric))
message("Complete-case N = ", nrow(cc))

mids <- readRDS(file.path(RDIR, "mice_imputation.rds"))
long <- mice::complete(mids, action = "long")
stacked <- as.data.frame(lapply(long[, SIX], as.numeric))
message("Stacked-MICE N = ", nrow(stacked), " (m=", mids$m, ")")

# --- helper: run PC + HC + boot on a dataset, return structures --------------
run_one <- function(dat, label) {
  message("\n===== ", label, " (N=", nrow(dat), ") =====")

  # PC (constraint-based) -> CPDAG
  pc <- pc.stable(dat, alpha = ALPHA, test = "cor")
  pc_arcs <- as.data.frame(arcs(pc))
  message("PC arcs (directed+undirected listed as pairs): ", nrow(pc_arcs))

  # HC (score-based, BIC-Gaussian) -> DAG
  hcdag <- hc(dat, score = "bic-g")
  hc_arcs <- as.data.frame(arcs(hcdag))
  bic_hc <- BIC(bn.fit(hcdag, dat), dat)
  message("HC arcs: ", nrow(hc_arcs), "  BIC=", round(bic_hc,2))

  # Bootstrap edge strength for HC family (score-based)
  boot <- boot.strength(dat, R = R_BOOT, algorithm = "hc",
                        algorithm.args = list(score = "bic-g"))
  boot_sig <- boot[boot$strength > 0, c("from","to","strength","direction")]

  list(pc = pc, pc_arcs = pc_arcs, hc = hcdag, hc_arcs = hc_arcs,
       bic = bic_hc, boot = boot)
}

# undirected skeleton key set from an arcs data.frame
skel_keys <- function(arcs_df) {
  if (nrow(arcs_df)==0) return(character(0))
  unique(apply(arcs_df, 1, function(r) proposed_key(r[["from"]], r[["to"]])))
}

res_cc <- run_one(cc, "COMPLETE-CASE")
res_st <- run_one(stacked, "STACKED-MICE")

# --- Build EIP table from boot.strength (undirected inclusion) ---------------
# collapse boot to undirected edge inclusion prob = strength (bnlearn strength
# is P(edge present in either direction) already for boot.strength).
eip_undirected <- function(boot) {
  b <- boot
  b$key <- apply(b[,c("from","to")], 1, function(r) proposed_key(r[1], r[2]))
  agg <- aggregate(strength ~ key, data = b, FUN = max)  # max over 2 directions
  agg
}
eip_cc <- eip_undirected(res_cc$boot)
eip_st <- eip_undirected(res_st$boot)

get_eip <- function(agg, key) {
  v <- agg$strength[agg$key == key]
  if (length(v)==0) 0 else v[1]
}

# --- Assemble learned-dag-edges.csv (use complete-case as primary honest) ----
primary <- res_cc
prim_eip <- eip_cc
prim_skel <- skel_keys(primary$hc_arcs)

edge_rows <- data.frame(from=character(), to=character(),
                        EIP=numeric(), in_proposed_pathway=logical(),
                        stringsAsFactors = FALSE)
for (i in seq_len(nrow(primary$hc_arcs))) {
  f <- primary$hc_arcs$from[i]; t <- primary$hc_arcs$to[i]
  k <- proposed_key(f,t)
  edge_rows <- rbind(edge_rows, data.frame(
    from=f, to=t, EIP=get_eip(prim_eip,k),
    in_proposed_pathway = k %in% PROP_KEYS))
}
write.csv(edge_rows, file.path(OUTDIR, "learned-dag-edges.csv"), row.names=FALSE)

# --- Proposed-edge presence table across both samples ------------------------
prop_tbl <- data.frame(
  proposed_edge = vapply(PROPOSED, function(e) paste0(e[1],"->",e[2]), character(1)),
  key = PROP_KEYS,
  in_HC_cc  = PROP_KEYS %in% skel_keys(res_cc$hc_arcs),
  EIP_cc    = vapply(PROP_KEYS, function(k) get_eip(eip_cc,k), numeric(1)),
  in_PC_cc  = PROP_KEYS %in% skel_keys(res_cc$pc_arcs),
  in_HC_st  = PROP_KEYS %in% skel_keys(res_st$hc_arcs),
  EIP_st    = vapply(PROP_KEYS, function(k) get_eip(eip_st,k), numeric(1)),
  in_PC_st  = PROP_KEYS %in% skel_keys(res_st$pc_arcs),
  stringsAsFactors = FALSE
)

# --- Save everything to an RDS + text dump for results.md build --------------
dump <- list(
  N_cc = nrow(cc), N_st = nrow(stacked), m = mids$m, alpha = ALPHA, R = R_BOOT,
  pc_cc = res_cc$pc_arcs, hc_cc = res_cc$hc_arcs, bic_cc = res_cc$bic,
  pc_st = res_st$pc_arcs, hc_st = res_st$hc_arcs, bic_st = res_st$bic,
  eip_cc = eip_cc, eip_st = eip_st, prop_tbl = prop_tbl,
  boot_cc = res_cc$boot[order(-res_cc$boot$strength),],
  boot_st = res_st$boot[order(-res_st$boot$strength),]
)
saveRDS(dump, file.path(OUTDIR, "w3_dump.rds"))

cat("\n\n########## RESULTS DUMP ##########\n")
cat("N_cc=", dump$N_cc, " N_st=", dump$N_st, " m=", dump$m,
    " alpha=", ALPHA, " R_boot=", R_BOOT, "\n\n")

cat("### PC CPDAG arcs (complete-case) ###\n"); print(res_cc$pc_arcs)
cat("\n### HC DAG arcs (complete-case)  BIC=", round(res_cc$bic,3), " ###\n"); print(res_cc$hc_arcs)
cat("\n### PC CPDAG arcs (stacked-MICE) ###\n"); print(res_st$pc_arcs)
cat("\n### HC DAG arcs (stacked-MICE)   BIC=", round(res_st$bic,3), " ###\n"); print(res_st$hc_arcs)

cat("\n### Proposed-edge presence table ###\n")
print(prop_tbl, row.names = FALSE)

cat("\n### Top boot EIP (complete-case, undirected max) ###\n")
print(eip_cc[order(-eip_cc$strength),], row.names = FALSE)
cat("\n### Top boot EIP (stacked-MICE, undirected max) ###\n")
print(eip_st[order(-eip_st$strength),], row.names = FALSE)

cat("\n### Full boot.strength complete-case (strength>0) ###\n")
print(res_cc$boot[res_cc$boot$strength>0,], row.names = FALSE)

# --- try figure (Rgraphviz optional) -----------------------------------------
fig_ok <- FALSE
try({
  if (requireNamespace("Rgraphviz", quietly = TRUE)) {
    pdf(file.path(OUTDIR, "learned-dag-6var.pdf"), width=6, height=5)
    graphviz.plot(res_cc$hc, main="Unconstrained HC DAG (6 vars, complete-case)")
    dev.off(); fig_ok <- TRUE
  }
}, silent = TRUE)
cat("\nFigure rendered (Rgraphviz):", fig_ok, "\n")
cat("\n########## END DUMP ##########\n")
