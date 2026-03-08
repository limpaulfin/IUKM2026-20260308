# Bayesian Network Inference for Resolving Uncertainty in Financial Depth--Stability Links

Replication package for the paper submitted to **IUKM 2026** (International Symposium on Integrated Uncertainty in Knowledge Modelling and Decision Making).

## Version

| Item | Version | Date |
|------|---------|------|
| Manuscript PDF | v1.0 | 2026-03-08 |
| Supplementary PDF | v1.0 | 2026-03-08 |
| R analysis code | v1.0 | 2026-02-10 |
| Raw data | Snapshot 2026-01-27 | 2026-01-27 |
| Processed data | Pipeline output 2026-02-10 | 2026-02-10 |

**Source commit reference:** `manu3-IUKM2026/` and `manu1-2026-02/R/` from private repo.

## Authors

- **Thanh-Phong Lam** -- Faculty of Management Information Systems, Ho Chi Minh City University of Banking (ORCID: 0009-0001-7790-5671)
- **Thi-Linh Ho** (Corresponding) -- NLP & Knowledge Discovery Research Group, Faculty of IT, Ton Duc Thang University (ORCID: 0009-0009-8187-8854)

## Repository Structure

```
.
├── output/                          # Compiled manuscripts
│   ├── IUKM2026-BN-FD-Stability-Linh-Phong.pdf   # Main paper (12 pages, LNCS)
│   └── IUKM2026-Supplementary.pdf                 # Supplementary material
├── code/                            # R analysis pipeline
│   ├── init-R.sh                    # Pipeline orchestrator (run this first)
│   ├── 00_setup.R                   # Package installation
│   ├── 01_clean_data.R              # Raw data cleaning
│   ├── 02_merge_panel.R             # Panel data construction
│   ├── 03_export_json.R             # JSON export
│   ├── 04_bn_eda.R                  # Exploratory data analysis
│   ├── 05_bn_structure.R            # BN structure learning (PC, HC, Tabu)
│   ├── 05b_bn_bootstrap.R           # Bootstrap structure stability
│   ├── 06_bn_parameters.R           # BN parameter estimation
│   ├── 07_bn_inference.R            # BN inference and queries
│   ├── 08_visualization_cud.R       # CUD colorblind-friendly plots
│   ├── 10_indirect_pathway_setup.R  # Mediation pathway setup
│   ├── 10b_mice_imputation.R        # Multiple imputation (MICE)
│   ├── 10c_mice_validation.R        # MICE validation
│   ├── 10d_nonlinearity_test.R      # Nonlinearity diagnostics
│   ├── 10e_cluster_bootstrap.R      # Cluster-robust bootstrap
│   ├── 10f_power_analysis.R         # Statistical power analysis
│   ├── 11_expert_guided_bn.R        # Expert-guided BN structure
│   ├── 12_path_coefficients.R       # Path coefficient estimation
│   ├── 13_mediation_bootstrap.R     # Mediation bootstrap analysis
│   ├── 14_sensitivity_analysis.R    # LOCO sensitivity analysis
│   └── 15_indirect_visualization.R  # Indirect effect visualization
├── data/
│   ├── raw/                         # Original data sources
│   │   ├── GFDD_raw_20260127.csv           # World Bank GFDD
│   │   ├── WDI_raw_20260127.csv            # World Bank WDI
│   │   ├── IMF_COFER_raw_20260127.csv      # IMF COFER
│   │   ├── DepositInsurance_raw_20260127.xlsx  # IADI Deposit Insurance
│   │   └── Harvard_VN_banks_2002_2021.xlsx    # Harvard Dataverse
│   └── processed/                   # Pipeline output
│       ├── bn_data.csv              # BN-ready dataset (62 obs x 21 vars)
│       └── panel_merged.csv         # Merged panel (285 obs)
```

## Data Sources

| Source | URL | Variables |
|--------|-----|-----------|
| World Bank GFDD | https://www.worldbank.org/en/publication/gfdr/data/global-financial-development-database | Financial depth indicators |
| World Bank WDI | https://databank.worldbank.org/source/world-development-indicators | GDP, inflation, trade |
| IMF FSI | https://data.imf.org/regular.aspx?key=61404590 | NPL ratio, Z-score, CAR |
| IMF COFER | https://data.imf.org/?sk=e6a5f467-c14b-4aa8-9f6d-5a09ec4e62a4 | Foreign reserves |
| IADI | https://www.iadi.org/en/deposit-insurance-systems/ | Deposit insurance coverage |
| Harvard Dataverse | https://doi.org/10.7910/DVN/IO2FJJ | Vietnam bank-level data |

**Note:** IMF FSI raw data (86 MB) is excluded from this repository due to size. Download directly from the IMF link above and place as `data/raw/IMF_FSI_raw_20260127.csv`.

## How to Reproduce

### Prerequisites
- R >= 4.3.0
- Required packages: `bnlearn`, `mice`, `lavaan`, `sensemakr`, `ggplot2` (see `code/00_setup.R`)

### Run Pipeline
```bash
cd code/
chmod +x init-R.sh
./init-R.sh
```

This executes all analysis steps sequentially (01 through 15).

## License

This work is submitted for review at IUKM 2026. Please cite appropriately if referencing this work.

## Sync Reference

To update this public repo from the private source:

```bash
# From private repo root
rsync -av --exclude='*.b' --exclude='*.bak*' \
  manu1-2026-02/R/*.R manu1-2026-02/R/init-R.sh \
  /home/fong/Projects/TRAM2026-BN-Banking-public/IUKM2026/code/

rsync -av \
  manu3-IUKM2026/LaTeX/IUKM2026-BN-FD-Stability-Linh-Phong.pdf \
  manu3-IUKM2026/LaTeX/IUKM2026-Supplementary.pdf \
  /home/fong/Projects/TRAM2026-BN-Banking-public/IUKM2026/output/

# Then clean AI traces and commit
```
