# =============================================================================
# 01_clean_data.R - Clean all 6 data sources
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-01-27
# Source: tidyverse workflow [Wickham 2023, R for Data Science]
# =============================================================================

source("R/00_setup.R")

message("\n=== [01] Data Cleaning Started ===\n")

# --- 1. Harvard Vietnamese Banks (2002-2021) ---
message(">>> Cleaning Harvard_VN_banks_2002_2021.xlsx...")
harvard_raw <- read_excel(file.path(DATA_DIR, "Harvard_VN_banks_2002_2021.xlsx"))
harvard_clean <- harvard_raw %>%
  clean_names() %>%
  mutate(
    country_code = "VNM",
    source = "Harvard"
  )
write_csv(harvard_clean, file.path(OUTPUT_DIR, "harvard_clean.csv"))
message(paste("   Rows:", nrow(harvard_clean), "| Cols:", ncol(harvard_clean)))

# --- 2. GFDD (Global Financial Development Database) ---
message(">>> Cleaning GFDD_raw_20260127.csv...")
gfdd_raw <- read_csv(file.path(DATA_DIR, "GFDD_raw_20260127.csv"), show_col_types = FALSE)
gfdd_clean <- gfdd_raw %>%
  clean_names() %>%
  filter_asean() %>%
  mutate(source = "GFDD")
write_csv(gfdd_clean, file.path(OUTPUT_DIR, "gfdd_clean.csv"))
message(paste("   Rows:", nrow(gfdd_clean), "| Cols:", ncol(gfdd_clean)))

# --- 3. WDI (World Development Indicators) ---
message(">>> Cleaning WDI_raw_20260127.csv...")
wdi_raw <- read_csv(file.path(DATA_DIR, "WDI_raw_20260127.csv"), show_col_types = FALSE)
wdi_clean <- wdi_raw %>%
  clean_names() %>%
  filter_asean() %>%
  mutate(source = "WDI")
write_csv(wdi_clean, file.path(OUTPUT_DIR, "wdi_clean.csv"))
message(paste("   Rows:", nrow(wdi_clean), "| Cols:", ncol(wdi_clean)))

# --- 4. IMF FSI (Financial Soundness Indicators) ---
message(">>> Cleaning IMF_FSI_raw_20260127.csv (large file, may take time)...")
fsi_raw <- read_csv(file.path(DATA_DIR, "IMF_FSI_raw_20260127.csv"), show_col_types = FALSE)
fsi_clean <- fsi_raw %>%
  clean_names() %>%
  # Filter for ASEAN countries (check column name)
  {
    if ("ref_area" %in% names(.)) {
      filter(., ref_area %in% ASEAN_COUNTRIES)
    } else if ("country_code" %in% names(.)) {
      filter_asean(.)
    } else {
      message("   WARNING: Cannot find country column in FSI. Keeping all rows.")
      .
    }
  } %>%
  mutate(source = "IMF_FSI")
write_csv(fsi_clean, file.path(OUTPUT_DIR, "fsi_clean.csv"))
message(paste("   Rows:", nrow(fsi_clean), "| Cols:", ncol(fsi_clean)))

# --- 5. IMF COFER (Currency Composition of Foreign Exchange Reserves) ---
message(">>> Cleaning IMF_COFER_raw_20260127.csv...")
cofer_raw <- read_csv(file.path(DATA_DIR, "IMF_COFER_raw_20260127.csv"), show_col_types = FALSE)
cofer_clean <- cofer_raw %>%
  clean_names() %>%
  mutate(source = "IMF_COFER")
# Note: COFER is aggregate data, not country-specific
write_csv(cofer_clean, file.path(OUTPUT_DIR, "cofer_clean.csv"))
message(paste("   Rows:", nrow(cofer_clean), "| Cols:", ncol(cofer_clean)))

# --- 6. Deposit Insurance Database ---
message(">>> Cleaning DepositInsurance_raw_20260127.xlsx...")
depins_raw <- read_excel(file.path(DATA_DIR, "DepositInsurance_raw_20260127.xlsx"))
depins_clean <- depins_raw %>%
  clean_names() %>%
  mutate(source = "DepositInsurance")
write_csv(depins_clean, file.path(OUTPUT_DIR, "depins_clean.csv"))
message(paste("   Rows:", nrow(depins_clean), "| Cols:", ncol(depins_clean)))

# --- Summary ---
message("\n=== [01] Data Cleaning Complete ===")
message(paste("Output directory:", OUTPUT_DIR))
message("Clean files created:")
list.files(OUTPUT_DIR, pattern = "_clean.csv")
