# =============================================================================
# 02_merge_panel.R - Merge cleaned data into panel dataset
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-01-27
# =============================================================================

source("R/00_setup.R")

message("\n=== [02] Merge Panel Data Started ===\n")

# --- Load cleaned data ---
message(">>> Loading cleaned datasets...")

gfdd <- read_csv(file.path(OUTPUT_DIR, "gfdd_clean.csv"), show_col_types = FALSE)
wdi <- read_csv(file.path(OUTPUT_DIR, "wdi_clean.csv"), show_col_types = FALSE)
fsi <- read_csv(file.path(OUTPUT_DIR, "fsi_clean.csv"), show_col_types = FALSE)
depins <- read_csv(file.path(OUTPUT_DIR, "depins_clean.csv"), show_col_types = FALSE)

message(paste("   GFDD rows:", nrow(gfdd)))
message(paste("   WDI rows:", nrow(wdi)))
message(paste("   FSI rows:", nrow(fsi)))
message(paste("   DepIns rows:", nrow(depins)))

# --- Standardize column names ---
message(">>> Standardizing column names...")

# Helper function to reshape long format
reshape_to_wide <- function(df, id_cols, indicator_col, value_col) {
  df %>%
    select(all_of(c(id_cols, indicator_col, value_col))) %>%
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = all_of(indicator_col),
      values_from = all_of(value_col),
      values_fn = list(value = mean)
    )
}

# --- Process GFDD ---
gfdd_wide <- tryCatch({
  if (all(c("country_code", "year", "indicator_code", "value") %in% names(gfdd))) {
    gfdd %>%
      reshape_to_wide(c("country_code", "year"), "indicator_code", "value")
  } else {
    message("   GFDD: keeping original structure")
    gfdd
  }
}, error = function(e) {
  message(paste("   GFDD reshape error:", e$message))
  gfdd
})

# --- Process WDI ---
wdi_wide <- tryCatch({
  if (all(c("country_code", "year", "indicator_code", "value") %in% names(wdi))) {
    wdi %>%
      reshape_to_wide(c("country_code", "year"), "indicator_code", "value")
  } else {
    message("   WDI: keeping original structure")
    wdi
  }
}, error = function(e) {
  message(paste("   WDI reshape error:", e$message))
  wdi
})

# --- Merge datasets ---
message(">>> Merging datasets...")

# Start with GFDD as base
panel <- gfdd_wide

# Left join WDI
if ("country_code" %in% names(wdi_wide) && "year" %in% names(wdi_wide)) {
  common_cols <- intersect(names(panel), names(wdi_wide))
  common_cols <- setdiff(common_cols, c("country_code", "year"))

  if (length(common_cols) > 0) {
    wdi_wide_renamed <- wdi_wide %>%
      rename_with(~ paste0(.x, "_wdi"), all_of(common_cols))
  } else {
    wdi_wide_renamed <- wdi_wide
  }

  panel <- panel %>%
    left_join(wdi_wide_renamed, by = c("country_code", "year"))
}

message(paste("   Panel rows after merge:", nrow(panel)))
message(paste("   Panel cols after merge:", ncol(panel)))

# --- Export merged panel ---
write_csv(panel, file.path(OUTPUT_DIR, "panel_merged.csv"))

message("\n=== [02] Merge Panel Complete ===")
message(paste("Output: panel_merged.csv"))
message(paste("Rows:", nrow(panel), "| Cols:", ncol(panel)))
