# =============================================================================
# 12_path_coefficients.R - Path Coefficient Estimation
# Purpose: Extract path coefficients along FD->NIM->FDI->NPL->Z_score
# Author: Thanh-Phong Lam
# Created: 2026-02-08, Updated: 2026-02-09 (DI→FD rename)
# Ref: Pearl (2009), Nagarajan et al. (2013), Gemini code
# =============================================================================

source("R/00_setup.R")

message("\n=== [12] Path Coefficients Started ===\n")

# --- Load data and expert DAG ---
pathway_std <- readRDS(file.path(OUTPUT_DIR, "pathway_std.rds"))
dag_expert  <- readRDS(file.path(OUTPUT_DIR, "dag_expert.rds"))

# --- Fit parameters (Gaussian BN = linear regression coefficients) ---
fitted_bn <- bn.fit(dag_expert, pathway_std)
saveRDS(fitted_bn, file.path(OUTPUT_DIR, "fitted_bn_expert.rds"))

# --- Extract path coefficients for indirect pathway ---
message(">>> Extracting path coefficients...")

# Each node's coefficients (partial regression)
get_coef <- function(node, parent) {
  coefs <- coef(fitted_bn[[node]])
  if (parent %in% names(coefs)) {
    return(coefs[[parent]])
  }
  return(NA)
}

# Indirect pathway: FD -> NIM -> FDI -> NPL -> Z_score
c_FD_NIM  <- get_coef("NIM", "FD")
c_NIM_FDI <- get_coef("FDI", "NIM")
c_FDI_NPL <- get_coef("NPL", "FDI")
c_NPL_Z   <- get_coef("Z_score", "NPL")

# Direct effect (if exists): FD -> Z_score
c_FD_Z_direct <- get_coef("Z_score", "FD")

# Total indirect effect = product of path segments
indirect_effect <- c_FD_NIM * c_NIM_FDI * c_FDI_NPL * c_NPL_Z

message(paste("   FD -> NIM:      ", round(c_FD_NIM, 4)))
message(paste("   NIM -> FDI:     ", round(c_NIM_FDI, 4)))
message(paste("   FDI -> NPL:     ", round(c_FDI_NPL, 4)))
message(paste("   NPL -> Z_score: ", round(c_NPL_Z, 4)))
message(paste("   Direct FD->Z:   ", round(c_FD_Z_direct, 4)))
message(paste("   INDIRECT EFFECT:", round(indirect_effect, 6)))

# --- All coefficients table ---
path_coefs <- data.frame(
  from = c("FD", "NIM", "FDI", "NPL", "FD"),
  to   = c("NIM", "FDI", "NPL", "Z_score", "Z_score"),
  type = c("indirect", "indirect", "indirect", "indirect", "direct"),
  coefficient = c(c_FD_NIM, c_NIM_FDI, c_FDI_NPL, c_NPL_Z, c_FD_Z_direct)
)

# Add total indirect
path_coefs <- rbind(path_coefs, data.frame(
  from = "FD", to = "Z_score", type = "total_indirect",
  coefficient = indirect_effect
))

print(path_coefs)
write_csv(path_coefs, file.path(OUTPUT_DIR, "path_coefficients.csv"))

# --- Full coefficient table for all nodes ---
all_coefs <- list()
for (node in names(fitted_bn)) {
  coefs <- coef(fitted_bn[[node]])
  if (length(coefs) > 1) {  # skip intercept-only
    parents <- names(coefs)[-1]  # remove intercept
    for (p in parents) {
      all_coefs[[length(all_coefs) + 1]] <- data.frame(
        parent = p, child = node, coefficient = coefs[[p]]
      )
    }
  }
}
all_coefs_df <- do.call(rbind, all_coefs)
write_csv(all_coefs_df, file.path(OUTPUT_DIR, "all_bn_coefficients.csv"))

message("\n=== [12] Path Coefficients Complete ===")
