# =============================================================================
# 14_sensitivity_analysis.R - Sensitivity Analysis for GDP Confounding
# Purpose: ci.test, leave-one-country-out, partial correlations
# Author: Thanh-Phong Lam
# Created: 2026-02-08, Updated: 2026-02-09 (DI→FD rename)
# Ref: Pearl (2009), Rosenbaum (2002), Colombo & Maathuis (2014)
# =============================================================================

source("R/00_setup.R")

message("\n=== [14] Sensitivity Analysis Started ===\n")

# --- Load data ---
pathway_std <- readRDS(file.path(OUTPUT_DIR, "pathway_std.rds"))
pathway_raw <- readRDS(file.path(OUTPUT_DIR, "pathway_data.rds"))
dag_expert  <- readRDS(file.path(OUTPUT_DIR, "dag_expert.rds"))

# --- 1. Conditional Independence Tests ---
message(">>> Conditional Independence Tests (ci.test)...")

ci_tests <- list(
  list(x = "FD", y = "Z_score", z = c("NIM", "FDI", "NPL", "GDP"),
       desc = "DI _||_ Z | NIM,FDI,NPL,GDP (full mediation?)"),
  list(x = "FD", y = "Z_score", z = c("GDP"),
       desc = "DI _||_ Z | GDP (GDP confounding only)"),
  list(x = "FD", y = "NPL", z = c("NIM", "FDI"),
       desc = "DI _||_ NPL | NIM,FDI (partial mediation?)"),
  list(x = "NIM", y = "Z_score", z = c("FDI", "NPL"),
       desc = "NIM _||_ Z | FDI,NPL")
)

ci_results <- list()
for (i in seq_along(ci_tests)) {
  t <- ci_tests[[i]]
  result <- ci.test(t$x, t$y, t$z, data = pathway_std)
  ci_results[[i]] <- data.frame(
    x = t$x, y = t$y, z = paste(t$z, collapse = ","),
    statistic = result$statistic, p_value = result$p.value,
    description = t$desc
  )
  message(paste("  ", t$desc))
  message(paste("     stat=", round(result$statistic, 4),
                "p=", round(result$p.value, 4)))
}
ci_df <- do.call(rbind, ci_results)
write_csv(ci_df, file.path(OUTPUT_DIR, "ci_tests_sensitivity.csv"))

# --- 2. Partial Correlations (controlling GDP) ---
message("\n>>> Partial correlations controlling GDP...")

partial_cor <- function(x, y, z, data) {
  rx <- residuals(lm(as.formula(paste(x, "~", paste(z, collapse = "+"))),
                      data = data))
  ry <- residuals(lm(as.formula(paste(y, "~", paste(z, collapse = "+"))),
                      data = data))
  cor(rx, ry)
}

pairs <- list(
  c("FD", "Z_score"), c("FD", "NIM"), c("NIM", "FDI"),
  c("FDI", "NPL"), c("NPL", "Z_score"), c("FD", "NPL")
)

pcor_results <- list()
for (p in pairs) {
  r_raw   <- cor(pathway_std[[p[1]]], pathway_std[[p[2]]])
  r_gdp   <- partial_cor(p[1], p[2], "GDP", pathway_std)
  r_full  <- partial_cor(p[1], p[2], c("GDP", "GovDebt"), pathway_std)
  pcor_results[[length(pcor_results) + 1]] <- data.frame(
    var1 = p[1], var2 = p[2],
    r_raw = r_raw, r_control_GDP = r_gdp, r_control_GDP_GovDebt = r_full
  )
}
pcor_df <- do.call(rbind, pcor_results)
write_csv(pcor_df, file.path(OUTPUT_DIR, "partial_correlations.csv"))
message("   Partial correlations:")
print(pcor_df)

# --- 3. Leave-One-Country-Out (LOCO) ---
message("\n>>> Leave-One-Country-Out analysis...")

panel_data <- read_csv(file.path(OUTPUT_DIR, "panel_merged.csv"),
                       show_col_types = FALSE)

# Get unique countries
if ("country_code" %in% colnames(panel_data)) {
  countries <- unique(panel_data$country_code)
} else {
  countries <- c("FULL_SAMPLE")
  message("   WARNING: No country_code column. Skipping LOCO.")
}

if (length(countries) > 1) {
  loco_results <- list()
  for (ctry in countries) {
    sub <- panel_data %>% filter(country_code != ctry)
    sub_std <- sub %>%
      select(any_of(c("GFDD.SI.01", "GFDD.SI.02", "GFDD.EI.01",
                       "GFDD.DI.12", "NY.GDP.PCAP.CD",
                       "GC.DOD.TOTL.GD.ZS", "BX.KLT.DINV.WD.GD.ZS",
                       "FP.CPI.TOTL.ZG"))) %>%
      na.omit()

    if (nrow(sub_std) >= 20) {
      colnames(sub_std) <- c("Z_score", "NPL", "NIM", "FD", "GDP",
                              "GovDebt", "FDI", "CPI")[1:ncol(sub_std)]
      sub_std <- sub_std %>% mutate(across(everything(), ~ scale(.x)[, 1]))

      tryCatch({
        fit_loco <- bn.fit(dag_expert, sub_std)
        ie <- coef(fit_loco$NIM)["FD"] * coef(fit_loco$FDI)["NIM"] *
              coef(fit_loco$NPL)["FDI"] * coef(fit_loco$Z_score)["NPL"]
        loco_results[[ctry]] <- data.frame(
          excluded = ctry, n = nrow(sub_std), indirect_effect = ie
        )
      }, error = function(e) {
        message(paste("   Skipping", ctry, ":", e$message))
      })
    }
  }
  if (length(loco_results) > 0) {
    loco_df <- do.call(rbind, loco_results)
    write_csv(loco_df, file.path(OUTPUT_DIR, "loco_sensitivity.csv"))
    message("   LOCO results:")
    print(loco_df)
  }
}

message("\n=== [14] Sensitivity Analysis Complete ===")
