# =============================================================================
# 08_visualization_cud.R - Enhanced Visualization with CUD Palette
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-01-27
# Palette: Okabe-Ito (Color Universal Design)
# Reference: Wong, B. (2011). Nature Methods; Okabe & Ito (Jikei Medical School)
# =============================================================================

source("R/00_setup.R")

message("\n=== [08] CUD Visualization Started ===\n")

# --- CUD Okabe-Ito Palette (Colorblind-friendly) ---
CUD_PALETTE <- c(
  "Orange"         = "#E69F00",
  "Sky Blue"       = "#56B4E9",
  "Bluish Green"   = "#009E73",
  "Yellow"         = "#F0E442",
  "Blue"           = "#0072B2",
  "Vermilion"      = "#D55E00",
  "Reddish Purple" = "#CC79A7",
  "Black"          = "#000000"
)

# Functional palette for ggplot2 (with recycling if n > 8)
cud_colors <- function(n = 7) {
  base <- unname(CUD_PALETTE[1:8])
  if (n <= 8) {
    return(base[1:n])
  } else {
    return(rep_len(base, n))
  }
}

# --- Load Data ---
message(">>> Loading data...")
bn_data <- read_csv(file.path(OUTPUT_DIR, "bn_data.csv"), show_col_types = FALSE)
panel_data <- read_csv(file.path(OUTPUT_DIR, "panel_merged.csv"), show_col_types = FALSE)
cor_matrix <- read.csv(file.path(OUTPUT_DIR, "eda_correlation_matrix.csv"), row.names = 1)

# --- Plot 1: Enhanced Correlation Heatmap with CUD ---
message(">>> [1/4] Generating CUD correlation heatmap...")
png(file.path(OUTPUT_DIR, "viz_correlation_cud.png"), width = 2800, height = 2400, res = 300)
corrplot(
  as.matrix(cor_matrix),
  method = "color",
  type = "upper",
  order = "hclust",
  tl.cex = 0.7,
  tl.col = CUD_PALETTE["Black"],
  col = colorRampPalette(c(CUD_PALETTE["Blue"], "white", CUD_PALETTE["Vermilion"]))(100),
  title = "Correlation Matrix - BN Variables",
  mar = c(0, 0, 2, 0),
  addCoef.col = CUD_PALETTE["Black"],
  number.cex = 0.5
)
dev.off()
message("   Saved: viz_correlation_cud.png")

# --- Plot 2: Distribution Facets with CUD ---
message(">>> [2/4] Generating distribution plots with CUD...")
top_vars <- bn_data %>%
  summarise(across(everything(), \(x) var(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "variance") %>%
  arrange(desc(variance)) %>%
  head(8) %>%
  pull(var)

p_dist <- bn_data %>%
  select(all_of(top_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(bins = 30, alpha = 0.8, color = "white", linewidth = 0.2) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  scale_fill_manual(values = cud_colors(9)) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Distribution of Key Variables",
    subtitle = "Top 8 variables by variance",
    x = "Value",
    y = "Frequency"
  )

ggsave(file.path(OUTPUT_DIR, "viz_distributions_cud.png"), p_dist,
       width = 12, height = 10, dpi = 300)
message("   Saved: viz_distributions_cud.png")

# --- Plot 3: Time Series Panel by Country ---
message(">>> [3/4] Generating time series panel...")

# Select key indicator: GFDD.SI.01 (Bank Z-score - stability)
if ("country_code" %in% names(panel_data) && "year" %in% names(panel_data)) {

  # Get ASEAN countries
  asean_countries <- c("IDN", "MYS", "PHL", "SGP", "THA", "VNM", "BRN", "KHM", "LAO", "MMR")

  ts_data <- panel_data %>%
    filter(country_code %in% asean_countries) %>%
    filter(!is.na(GFDD.SI.01)) %>%
    select(country_code, year, GFDD.SI.01, GFDD.DI.01) %>%
    pivot_longer(cols = c(GFDD.SI.01, GFDD.DI.01),
                 names_to = "indicator", values_to = "value")

  if (nrow(ts_data) > 0) {
    p_ts <- ts_data %>%
      ggplot(aes(x = year, y = value, color = country_code, group = country_code)) +
      geom_line(linewidth = 1, alpha = 0.8) +
      geom_point(size = 1.5, alpha = 0.7) +
      facet_wrap(~indicator, scales = "free_y", ncol = 1,
                 labeller = labeller(indicator = c(
                   "GFDD.SI.01" = "Bank Z-Score (Stability)",
                   "GFDD.DI.01" = "Private Credit/GDP (Depth)"
                 ))) +
      scale_color_manual(values = cud_colors(length(unique(ts_data$country_code)))) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold")
      ) +
      labs(
        title = "Banking Indicators: ASEAN Countries",
        subtitle = "Time series 2005-2020",
        x = "Year",
        y = "Value"
      ) +
      guides(color = guide_legend(nrow = 2))

    ggsave(file.path(OUTPUT_DIR, "viz_timeseries_cud.png"), p_ts,
           width = 12, height = 8, dpi = 300)
    message("   Saved: viz_timeseries_cud.png")
  } else {
    message("   WARNING: No ASEAN data found for time series")
  }
} else {
  message("   WARNING: country_code or year column missing")
}

# --- Plot 4: BN Edge Comparison (HC vs PC vs Tabu) ---
message(">>> [4/4] Generating BN method comparison...")

comparison <- read_csv(file.path(OUTPUT_DIR, "bn_structure_comparison.csv"), show_col_types = FALSE)

p_compare <- comparison %>%
  ggplot(aes(x = reorder(method, n_edges), y = n_edges, fill = method)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_text(aes(label = n_edges), vjust = -0.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = cud_colors(3)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "BN Structure Learning: Method Comparison",
    subtitle = "Number of edges discovered",
    x = "Method",
    y = "Number of Edges"
  ) +
  coord_cartesian(ylim = c(0, max(comparison$n_edges, na.rm = TRUE) * 1.15))

ggsave(file.path(OUTPUT_DIR, "viz_bn_comparison_cud.png"), p_compare,
       width = 8, height = 6, dpi = 300)
message("   Saved: viz_bn_comparison_cud.png")

# --- Plot 5: Boxplot by Country ---
message(">>> [5/8] Generating boxplot by country...")

if ("country_code" %in% names(panel_data)) {
  asean_countries <- c("IDN", "MYS", "PHL", "SGP", "THA", "VNM")

  box_data <- panel_data %>%
    filter(country_code %in% asean_countries) %>%
    filter(!is.na(GFDD.SI.01)) %>%
    select(country_code, GFDD.SI.01)

  if (nrow(box_data) > 0) {
    p_box <- box_data %>%
      ggplot(aes(x = reorder(country_code, GFDD.SI.01, FUN = median),
                 y = GFDD.SI.01, fill = country_code)) +
      geom_boxplot(alpha = 0.8, outlier.shape = 21) +
      geom_jitter(width = 0.1, alpha = 0.4, size = 1.5) +
      scale_fill_manual(values = cud_colors(6)) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none") +
      labs(
        title = "Bank Z-Score by Country",
        subtitle = "ASEAN-6 comparison (2005-2020)",
        x = "Country",
        y = "Bank Z-Score (Stability)"
      ) +
      coord_flip()

    ggsave(file.path(OUTPUT_DIR, "viz_boxplot_country.png"), p_box,
           width = 10, height = 6, dpi = 300)
    message("   Saved: viz_boxplot_country.png")
  }
}

# --- Plot 6: Scatter - Deposit Insurance vs Stability ---
message(">>> [6/8] Generating scatter plot...")

scatter_data <- bn_data %>%
  select(GFDD.SI.01, GFDD.DI.01, GFDD.DI.12) %>%
  na.omit()

if (nrow(scatter_data) > 0) {
  p_scatter <- scatter_data %>%
    ggplot(aes(x = GFDD.DI.01, y = GFDD.SI.01)) +
    geom_point(aes(color = GFDD.DI.12), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = CUD_PALETTE["Vermilion"],
                fill = CUD_PALETTE["Orange"], alpha = 0.2) +
    scale_color_gradient(low = CUD_PALETTE["Sky Blue"],
                         high = CUD_PALETTE["Blue"],
                         name = "Bank Deposits\n(% GDP)") +
    theme_minimal(base_size = 12) +
    labs(
      title = "Banking Stability vs Private Credit",
      subtitle = "Moral hazard relationship",
      x = "Private Credit / GDP (%)",
      y = "Bank Z-Score (Stability)"
    )

  ggsave(file.path(OUTPUT_DIR, "viz_scatter_stability.png"), p_scatter,
         width = 10, height = 7, dpi = 300)
  message("   Saved: viz_scatter_stability.png")
}

# --- Plot 7: Heatmap Missing Data ---
message(">>> [7/8] Generating missing data heatmap...")

missing_data <- panel_data %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), \(x) mean(is.na(x)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  filter(pct_missing > 0) %>%
  arrange(desc(pct_missing))

if (nrow(missing_data) > 0) {
  p_missing <- missing_data %>%
    head(15) %>%
    ggplot(aes(x = reorder(variable, pct_missing), y = pct_missing, fill = pct_missing)) +
    geom_col(alpha = 0.85) +
    geom_text(aes(label = sprintf("%.1f%%", pct_missing)), hjust = -0.1, size = 3.5) +
    scale_fill_gradient(low = CUD_PALETTE["Sky Blue"], high = CUD_PALETTE["Vermilion"]) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none") +
    labs(
      title = "Missing Data by Variable",
      subtitle = "Top 15 variables with highest missingness",
      x = NULL,
      y = "Missing (%)"
    ) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(missing_data$pct_missing) * 1.15))

  ggsave(file.path(OUTPUT_DIR, "viz_missing_data.png"), p_missing,
         width = 10, height = 7, dpi = 300)
  message("   Saved: viz_missing_data.png")
} else {
  message("   No missing data in panel_data")
}

# --- Plot 8: Panel Heatmap (Country x Year) ---
message(">>> [8/8] Generating panel heatmap...")

if ("country_code" %in% names(panel_data) && "year" %in% names(panel_data)) {
  asean_countries <- c("IDN", "MYS", "PHL", "SGP", "THA", "VNM", "BRN", "KHM", "LAO", "MMR")

  heatmap_data <- panel_data %>%
    filter(country_code %in% asean_countries) %>%
    select(country_code, year, GFDD.SI.01) %>%
    filter(!is.na(GFDD.SI.01))

  if (nrow(heatmap_data) > 0) {
    p_heatmap <- heatmap_data %>%
      ggplot(aes(x = factor(year), y = country_code, fill = GFDD.SI.01)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_gradient2(
        low = CUD_PALETTE["Vermilion"],
        mid = "white",
        high = CUD_PALETTE["Bluish Green"],
        midpoint = median(heatmap_data$GFDD.SI.01, na.rm = TRUE),
        name = "Z-Score"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      ) +
      labs(
        title = "Bank Stability Heatmap: ASEAN",
        subtitle = "Z-Score by country and year",
        x = "Year",
        y = "Country"
      )

    ggsave(file.path(OUTPUT_DIR, "viz_heatmap_panel.png"), p_heatmap,
           width = 14, height = 6, dpi = 300)
    message("   Saved: viz_heatmap_panel.png")
  }
}

# --- Summary ---
message("\n=== [08] CUD Visualization Complete ===")
message("Output files:")
list.files(OUTPUT_DIR, pattern = "^viz_")
message("\nCUD Palette Reference:")
message("  Orange (#E69F00) - Primary accent")
message("  Sky Blue (#56B4E9) - Secondary")
message("  Blue (#0072B2) - Links, primary series")
message("  Vermilion (#D55E00) - Errors, warnings")
message("  Bluish Green (#009E73) - Success, positive")
