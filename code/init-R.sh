#!/bin/bash
# =============================================================================
# init-R.sh - Run complete R data pipeline
# Project: BN Moral Hazard Banking ASEAN
# Author: Thanh-Phong Lam
# Created: 2026-01-27
#
# Usage: ./R/init-R.sh [--clean] [--step N]
#   --clean     Remove output directory before running
#   --step N    Run only step N (0-9)
#
# Pipeline:
#   00_setup.R        → Install packages, set paths
#   01_clean_data.R   → Clean 6 data sources
#   02_merge_panel.R  → Merge into panel dataset
#   03_export_json.R  → Export to JSON + BN CSV
#   04_bn_eda.R       → Exploratory Data Analysis
#   05_bn_structure.R → BN Structure Learning
#   05b_bn_bootstrap.R → Bootstrap validation
#   06_bn_parameters.R → BN Parameter Estimation
#   07_bn_inference.R → BN Inference & Statistics
#   08_visualization_cud.R → CUD visualizations (8 plots)
#   10_indirect_pathway_setup.R → Prepare pathway variables
#   10b_mice_imputation.R → MICE multiple imputation (m=5, N=285)
#   10c_mice_validation.R → MICE validation + BN stability across imputations
#   10d_nonlinearity_test.R → Quadratic FD² inverted-U test
#   10e_cluster_bootstrap.R → Cluster bootstrap SE + FE de-meaned BN
#   10f_power_analysis.R → Monte Carlo power analysis (S=5000)
#   11_expert_guided_bn.R → Expert-guided DAG structure
#   12_path_coefficients.R → Standardized path coefficients
#   13_mediation_bootstrap.R → Bootstrap mediation test
#   14_sensitivity_analysis.R → CI tests, partial corr, LOCO
#   15_indirect_visualization.R → Pathway visualizations (3 plots)
# =============================================================================

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Project paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
OUTPUT_DIR="$SCRIPT_DIR/output"
LOG_FILE="$OUTPUT_DIR/pipeline_$(date +%Y%m%d_%H%M%S).log"

# Parse arguments
CLEAN_OUTPUT=false
STEP_ONLY=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --clean)
            CLEAN_OUTPUT=true
            shift
            ;;
        --step)
            STEP_ONLY="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Functions
log() {
    echo -e "${BLUE}[$(date '+%H:%M:%S')]${NC} $1"
    echo "[$(date '+%H:%M:%S')] $1" >> "$LOG_FILE" 2>/dev/null || true
}

success() {
    echo -e "${GREEN}[✓]${NC} $1"
    echo "[✓] $1" >> "$LOG_FILE" 2>/dev/null || true
}

error() {
    echo -e "${RED}[✗]${NC} $1"
    echo "[✗] $1" >> "$LOG_FILE" 2>/dev/null || true
}

warning() {
    echo -e "${YELLOW}[!]${NC} $1"
    echo "[!] $1" >> "$LOG_FILE" 2>/dev/null || true
}

run_step() {
    local step=$1
    local script=$2
    local description=$3

    log "Step $step: $description"

    if [[ ! -f "$SCRIPT_DIR/$script" ]]; then
        error "Script not found: $script"
        return 1
    fi

    cd "$PROJECT_ROOT"
    if Rscript "R/$script" >> "$LOG_FILE" 2>&1; then
        success "$script completed"
        return 0
    else
        error "$script failed. Check log: $LOG_FILE"
        return 1
    fi
}

# Banner
echo ""
echo "=============================================="
echo "  BN Moral Hazard Banking ASEAN - R Pipeline"
echo "=============================================="
echo ""

# Check R installation
if ! command -v Rscript &> /dev/null; then
    error "Rscript not found. Please install R."
    exit 1
fi
log "R version: $(R --version | head -1)"

# Clean output if requested
if [[ "$CLEAN_OUTPUT" == true ]]; then
    warning "Cleaning output directory..."
    rm -rf "$OUTPUT_DIR"
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"
log "Output directory: $OUTPUT_DIR"
log "Log file: $LOG_FILE"

# Start time
START_TIME=$(date +%s)

# Define pipeline steps
declare -a STEPS=(
    "00_setup.R:Install packages and setup"
    "01_clean_data.R:Clean 6 data sources"
    "02_merge_panel.R:Merge panel dataset"
    "03_export_json.R:Export JSON and BN CSV"
    "04_bn_eda.R:Exploratory Data Analysis"
    "05_bn_structure.R:BN Structure Learning"
    "05b_bn_bootstrap.R:Bootstrap validation"
    "06_bn_parameters.R:BN Parameter Estimation"
    "07_bn_inference.R:BN Inference and Statistics"
    "08_visualization_cud.R:CUD Visualizations"
    "10_indirect_pathway_setup.R:Indirect Pathway Setup"
    "10b_mice_imputation.R:MICE Multiple Imputation (N=285)"
    "10c_mice_validation.R:MICE Validation and BN Stability"
    "10d_nonlinearity_test.R:Quadratic Nonlinearity Test"
    "10e_cluster_bootstrap.R:Cluster Bootstrap and FE De-meaned BN"
    "10f_power_analysis.R:Monte Carlo Power Analysis"
    "11_expert_guided_bn.R:Expert-Guided BN Structure"
    "12_path_coefficients.R:Path Coefficients"
    "13_mediation_bootstrap.R:Bootstrap Mediation Test"
    "14_sensitivity_analysis.R:Sensitivity Analysis"
    "15_indirect_visualization.R:Indirect Pathway Viz"
)

# Run pipeline
echo ""
log "Starting pipeline..."
echo ""

FAILED=0
for i in "${!STEPS[@]}"; do
    IFS=':' read -r script description <<< "${STEPS[$i]}"

    # Check if running specific step only
    if [[ -n "$STEP_ONLY" ]] && [[ "$i" != "$STEP_ONLY" ]]; then
        continue
    fi

    if ! run_step "$i" "$script" "$description"; then
        FAILED=1
        if [[ -z "$STEP_ONLY" ]]; then
            error "Pipeline stopped at step $i"
            break
        fi
    fi
    echo ""
done

# End time
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# Summary
echo ""
echo "=============================================="
if [[ $FAILED -eq 0 ]]; then
    success "Pipeline completed successfully!"
else
    error "Pipeline failed!"
fi
echo "=============================================="
log "Duration: ${DURATION}s"
log "Log file: $LOG_FILE"

# List output files
if [[ -d "$OUTPUT_DIR" ]]; then
    echo ""
    log "Output files:"
    ls -lh "$OUTPUT_DIR" | tail -20
fi

exit $FAILED
