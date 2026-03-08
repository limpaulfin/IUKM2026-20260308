# =============================================================================
# 04_bn_eda.R - Exploratory Data Analysis for Bayesian Network
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-01-27
# =============================================================================

source("R/00_setup.R")

message("\n=== [04] EDA for BN Started ===\n")

# --- Load BN data ---
message(">>> Loading bn_data.csv...")
bn_data <- read_csv(file.path(OUTPUT_DIR, "bn_data.csv"), show_col_types = FALSE)
message(paste("   Rows:", nrow(bn_data), "| Cols:", ncol(bn_data)))

# --- Summary Statistics ---
message(">>> Computing summary statistics...")
summary_stats <- bn_data %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    n_na = ~sum(is.na(.))
  ))) %>%
  pivot_longer(everything(), names_to = "stat", values_to = "value")

write_csv(summary_stats, file.path(OUTPUT_DIR, "eda_summary_stats.csv"))
message("   Saved: eda_summary_stats.csv")

# --- Correlation Matrix ---
message(">>> Computing correlation matrix...")
cor_matrix <- cor(bn_data, use = "pairwise.complete.obs")

# Save correlation matrix
write.csv(cor_matrix, file.path(OUTPUT_DIR, "eda_correlation_matrix.csv"))
message("   Saved: eda_correlation_matrix.csv")

# --- Correlation Plot ---
message(">>> Generating correlation plot...")
png(file.path(OUTPUT_DIR, "eda_correlation_plot.png"), width = 1600, height = 1600, res = 150)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 1.0,
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         title = "Correlation Matrix - BN Variables",
         mar = c(0,0,2,0))
dev.off()
message("   Saved: eda_correlation_plot.png")

# --- Missing Data Analysis ---
message(">>> Analyzing missing data...")
missing_summary <- bn_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = n_missing / nrow(bn_data) * 100) %>%
  arrange(desc(n_missing))

write_csv(missing_summary, file.path(OUTPUT_DIR, "eda_missing_data.csv"))
message("   Saved: eda_missing_data.csv")

# --- Distribution Plots ---
message(">>> Generating distribution plots...")
# Select top 9 variables by variance
top_vars <- bn_data %>%
  summarise(across(everything(), var, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "var", values_to = "variance") %>%
  arrange(desc(variance)) %>%
  head(9) %>%
  pull(var)

if (length(top_vars) > 0) {
  p <- bn_data %>%
    select(all_of(top_vars)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    facet_wrap(~variable, scales = "free") +
    theme_minimal() +
    labs(title = "Distribution of Top Variables (by variance)")

  ggsave(file.path(OUTPUT_DIR, "eda_distributions.png"), p, width = 12, height = 10)
  message("   Saved: eda_distributions.png")
}

message("\n=== [04] EDA Complete ===")
message("Output files:")
list.files(OUTPUT_DIR, pattern = "^eda_")
