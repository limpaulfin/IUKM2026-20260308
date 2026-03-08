# =============================================================================
# 15_indirect_visualization.R - CUD Visualization for Indirect Pathway
# Purpose: Visualize path coefficients, bootstrap CI, pathway diagram
# Author: Thanh-Phong Lam
# Created: 2026-02-08, Updated: 2026-02-09 (DI→FD rename)
# Ref: Okabe-Ito CUD palette, instructions-R.json
# =============================================================================

source("R/00_setup.R")

message("\n=== [15] Indirect Pathway Visualization Started ===\n")

# --- CUD Palette (Okabe-Ito, colorblind-safe) ---
CUD <- list(
  Black = "#000000", Orange = "#E69F00", SkyBlue = "#56B4E9",
  BluishGreen = "#009E73", Yellow = "#F0E442", Blue = "#0072B2",
  Vermilion = "#D55E00", ReddishPurple = "#CC79A7"
)

# --- Load results ---
path_coefs <- read_csv(file.path(OUTPUT_DIR, "path_coefficients.csv"),
                       show_col_types = FALSE)
boot_summary <- read_csv(file.path(OUTPUT_DIR, "bootstrap_indirect_effect.csv"),
                         show_col_types = FALSE)
boot_vec <- readRDS(file.path(OUTPUT_DIR, "bootstrap_indirect_vector.rds"))

# --- 1. Path Coefficient Bar Plot ---
message(">>> Plotting path coefficients...")
path_indirect <- path_coefs %>% filter(type == "indirect")

p1 <- ggplot(path_indirect, aes(x = paste(from, "->", to), y = coefficient,
                                 fill = coefficient > 0)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c("TRUE" = CUD$Blue, "FALSE" = CUD$Vermilion),
                    guide = "none") +
  labs(title = "Path Coefficients: FD -> NIM -> FDI -> NPL -> Z-score",
       subtitle = "Standardized coefficients from expert-guided Gaussian BN",
       x = "Path Segment", y = "Standardized Coefficient") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(file.path(OUTPUT_DIR, "viz_path_coefficients.png"), p1,
       width = 8, height = 5, dpi = 300)

# --- 2. Bootstrap Distribution of Indirect Effect ---
message(">>> Plotting bootstrap indirect effect distribution...")

boot_df <- data.frame(indirect_effect = boot_vec)
ci95 <- quantile(boot_vec, c(0.025, 0.975))

p2 <- ggplot(boot_df, aes(x = indirect_effect)) +
  geom_histogram(bins = 50, fill = CUD$SkyBlue, color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = CUD$Black, linewidth = 1) +
  geom_vline(xintercept = ci95[1], linetype = "dotted", color = CUD$Vermilion) +
  geom_vline(xintercept = ci95[2], linetype = "dotted", color = CUD$Vermilion) +
  geom_vline(xintercept = mean(boot_vec), color = CUD$Blue, linewidth = 1) +
  labs(title = "Bootstrap Distribution: Total Indirect Effect (R=1000)",
       subtitle = paste0("Mean=", round(mean(boot_vec), 5),
                         " | 95% CI [", round(ci95[1], 5), ", ",
                         round(ci95[2], 5), "]"),
       x = "Indirect Effect (FD -> NIM -> FDI -> NPL -> Z-score)",
       y = "Frequency") +
  theme_minimal(base_size = 13)

ggsave(file.path(OUTPUT_DIR, "viz_bootstrap_indirect.png"), p2,
       width = 8, height = 5, dpi = 300)

# --- 3. Partial Correlation Comparison ---
message(">>> Plotting partial correlations...")
pcor <- tryCatch(
  read_csv(file.path(OUTPUT_DIR, "partial_correlations.csv"),
           show_col_types = FALSE),
  error = function(e) NULL
)

if (!is.null(pcor)) {
  pcor_long <- pcor %>%
    pivot_longer(cols = starts_with("r_"), names_to = "control",
                 values_to = "correlation") %>%
    mutate(pair = paste(var1, "-", var2),
           control = gsub("r_", "", control))

  p3 <- ggplot(pcor_long, aes(x = pair, y = correlation, fill = control)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(values = c("raw" = CUD$Orange,
                                  "control_GDP" = CUD$Blue,
                                  "control_GDP_GovDebt" = CUD$BluishGreen)) +
    labs(title = "Partial Correlations: Raw vs GDP-Controlled",
         subtitle = "How GDP confounding changes correlations",
         x = "Variable Pair", y = "Correlation", fill = "Control") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  ggsave(file.path(OUTPUT_DIR, "viz_partial_correlations.png"), p3,
         width = 10, height = 5, dpi = 300)
}

message("\n=== [15] Indirect Pathway Visualization Complete ===")
