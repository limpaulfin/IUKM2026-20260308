# =============================================================================
# 03_export_json.R - Export panel data to JSON for BN analysis
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-01-27
# Source: jsonlite workflow [R for Data Science]
# =============================================================================

source("R/00_setup.R")

message("\n=== [03] Export JSON Started ===\n")

# --- Load merged panel ---
message(">>> Loading panel_merged.csv...")
panel <- read_csv(file.path(OUTPUT_DIR, "panel_merged.csv"), show_col_types = FALSE)
message(paste("   Rows:", nrow(panel), "| Cols:", ncol(panel)))

# --- Prepare JSON structure ---
message(">>> Preparing JSON structure...")

# Convert to list format for better JSON hierarchy
# Structure: {data: [{country_code, year, indicators: {...}}, ...]}

# Get indicator columns (all except country_code and year)
id_cols <- c("country_code", "year")
indicator_cols <- setdiff(names(panel), id_cols)

# Create nested structure
panel_nested <- panel %>%
  rowwise() %>%
  mutate(
    indicators = list(as.list(across(all_of(indicator_cols))))
  ) %>%
  ungroup() %>%
  select(country_code, year, indicators)

# Create final JSON object
json_object <- list(
  meta = list(
    project = "BN Moral Hazard Banking ASEAN",
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    countries = ASEAN_COUNTRIES,
    n_rows = nrow(panel),
    n_indicators = length(indicator_cols),
    indicator_names = indicator_cols
  ),
  data = panel_nested
)

# --- Export JSON ---
message(">>> Exporting to JSON...")
json_path <- file.path(OUTPUT_DIR, "panel_data.json")
write_json(json_object, json_path, pretty = TRUE, auto_unbox = TRUE)

# Verify file size
file_size <- file.info(json_path)$size
message(paste("   File size:", round(file_size / 1024, 2), "KB"))

# --- Also export simple CSV version for BN ---
# BN algorithms prefer wide format with numeric columns
message(">>> Exporting BN-ready CSV...")
bn_data <- panel %>%
  select(where(is.numeric)) %>%
  na.omit()  # BN requires complete cases

bn_path <- file.path(OUTPUT_DIR, "bn_data.csv")
write_csv(bn_data, bn_path)
message(paste("   BN data rows (complete cases):", nrow(bn_data)))
message(paste("   BN data cols:", ncol(bn_data)))

message("\n=== [03] Export JSON Complete ===")
message(paste("JSON output:", json_path))
message(paste("BN CSV output:", bn_path))
