# =============================================================================
# 00_setup.R - Install and load required packages
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-01-27
# =============================================================================

# --- Package Installation ---
# NOTE: Using individual tidyverse packages instead of meta-package
# (meta-package requires curl/httr which need system libs)
required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "tibble",
  "stringr",
  "purrr",
  "readxl",
  "jsonlite",
  "bnlearn",
  "igraph",
  "ggplot2",
  "janitor",
  "countrycode",
  "lubridate",
  "corrplot",
  "mice"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing:", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(required_packages, install_if_missing))

# --- Load Packages ---
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(stringr)
library(purrr)
library(readxl)
library(jsonlite)
library(bnlearn)
library(igraph)
library(ggplot2)
library(janitor)
library(countrycode)
library(lubridate)
library(corrplot)
library(mice)

# --- Project Paths ---
PROJECT_ROOT <- "/home/fong/Projects/TRAM2026-BN-Banking"
DATA_DIR <- file.path(PROJECT_ROOT, "data")
R_DIR <- file.path(PROJECT_ROOT, "R")
OUTPUT_DIR <- file.path(PROJECT_ROOT, "R", "output")

# Create output directory if not exists
if (!dir.exists(OUTPUT_DIR)) {
dir.create(OUTPUT_DIR, recursive = TRUE)
}

# --- ASEAN-10 Country Codes (core sample) ---
ASEAN_10 <- c(
  "BRN", "KHM", "IDN", "LAO", "MYS",
  "MMR", "PHL", "SGP", "THA", "VNM"
)

# --- Extended Sample (South Asia for robustness) ---
EXTENDED_COUNTRIES <- c("IND", "BGD", "LKA")

# --- All Countries (backward compatible) ---
ASEAN_COUNTRIES <- c(ASEAN_10, EXTENDED_COUNTRIES)

# --- Utility Functions ---
filter_asean <- function(df, country_col = "country_code") {
df %>%
    filter(!!sym(country_col) %in% ASEAN_COUNTRIES)
}

# Print setup complete
message("=== Setup Complete ===")
message(paste("Project root:", PROJECT_ROOT))
message(paste("Data directory:", DATA_DIR))
message(paste("Output directory:", OUTPUT_DIR))
message(paste("ASEAN-10:", length(ASEAN_10)))
message(paste("Extended sample:", length(EXTENDED_COUNTRIES)))
message(paste("Total countries:", length(ASEAN_COUNTRIES)))
