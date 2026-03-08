# =============================================================================
# 10_indirect_pathway_setup.R - Setup for Indirect Financial Depth Analysis
# Purpose: Delegates to 10b_mice_imputation.R (MICE replaces na.omit)
# Author: Thanh-Phong Lam
# Created: 2026-02-08, Updated: 2026-02-09 (MICE + DI→FD rename)
# Ref: van Buuren & Groothuis-Oudshoorn (2011), Scutari & Denis (2021)
# =============================================================================

message("\n=== [10] Indirect Pathway Setup → Delegating to 10b (MICE) ===\n")

# 10b handles: variable selection, MICE imputation, standardization, saving
source("R/10b_mice_imputation.R")

message("\n=== [10] Indirect Pathway Setup Complete (via 10b MICE) ===")
