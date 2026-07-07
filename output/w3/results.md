# R2-W3: Unconstrained Bayesian-Network Structure Learning (6 variables)

Reviewer R2-W3: "do Bayesian network structure learning with the 6 variables in
Figure 1(b); see if the DAG learned from the data looks similar to the proposed
indirect pathway." Proposed pathway (whitelist-imposed in the current paper):
**FD -> NIM -> FDI -> NPL -> Z**, with **GDP** as confounder.

All numbers below come from an actual `Rscript` run of
`run-w3-structure-learning.R` (2026-07-02) on the REAL merged panel. No
simulation, no RNG-generated input data (the only randomness is the bootstrap
resampling `set.seed(42)`).

## Setup

| Item | Value |
|------|-------|
| Variables | FD, NIM, FDI, NPL, Z (= `Z_score`), GDP (6 vars from Fig 1(b)) |
| Data source | `data/processed/pathway_std.rds` (z-scored) + `mice_imputation.rds` (mids, m=5) |
| Complete-case N | **285** |
| Stacked-MICE N | **1425** (285 x m=5) |
| Constraint learner | PC (`pc.stable`, Gaussian `cor` test, **alpha = 0.05**) |
| Score learner | Hill-Climbing (`hc`, **BIC-Gaussian** `bic-g`), UNCONSTRAINED (no whitelist, no blacklist) |
| Bootstrap | `boot.strength`, **R = 500**, HC/bic-g; EIP = undirected edge inclusion prob (max over the two orientations) |

Note on the stacked-MICE sample: stacking 5 imputations into one 1425-row frame
inflates apparent N ~5x and therefore inflates significance/inclusion for weak
edges. The **complete-case (N=285) result is the statistically honest one**;
the stacked-MICE result is reported as the instruction-requested variant and as
an upper bound on edge support.

## Learned structures

### Complete-case (N=285)

- **PC CPDAG arcs (9):** FD-NIM, FDI-FD, FDI-GDP, NPL-FD, NPL-NIM, NPL-Z, GDP-NIM, GDP-FDI, GDP-Z.
- **HC DAG arcs (10), BIC = -2207.96:** FDI->GDP, GDP->Z, FD->NIM, FD->FDI, NPL->NIM, GDP->NIM, Z->NPL, FD->NPL, FDI->NIM, FD->Z.

### Stacked-MICE (N=1425)

- **PC CPDAG arcs (8):** FD-NIM, FD-FDI, FD-NPL, NIM-FDI, NIM-NPL, Z-FDI, Z-NPL (plus the undirected NIM-FD listing).
- **HC DAG arcs (10), BIC = -39239.43:** FDI->GDP, FD->FDI, GDP->NIM, Z->FDI, NPL->NIM, FD->NIM, FD->NPL, FDI->NIM, Z->GDP, Z->NPL.

## The 4 proposed pathway edges vs the data

| Proposed edge | in HC (cc) | EIP cc | in PC (cc) | in HC (st) | EIP st | in PC (st) |
|---------------|:----------:|:------:|:----------:|:----------:|:------:|:----------:|
| FD -> NIM   | YES | **1.000** | YES | YES | **1.000** | YES |
| NIM -> FDI  | YES (as FDI->NIM) | **0.470** | NO  | YES | **0.820** | YES |
| FDI -> NPL  | **NO** | **0.108** | **NO** | **NO** | **0.214** | **NO** |
| NPL -> Z    | YES (as Z->NPL) | **0.994** | YES | YES (as Z->NPL) | **1.000** | YES |

- **FD-NIM** and **NPL-Z**: strongly supported (EIP ~1.0) in both samples and both learners. Endpoints of the chain survive.
- **NIM-FDI**: WEAK. Complete-case EIP 0.470 (below the conventional 0.5 inclusion cut, and absent from the PC CPDAG). It only clears 0.5 in the N-inflated stacked sample (0.820).
- **FDI-NPL**: effectively **ABSENT** (EIP 0.108 complete-case, 0.214 stacked). This is the broken link that severs the serial chain.
- Orientation note: where present, the learner does NOT reproduce the proposed directions cleanly. FD-NIM direction is ~50/50 (FD->NIM 0.522 vs NIM->FD 0.478); NPL-Z is oriented **Z->NPL** (0.78), the REVERSE of the proposed NPL->Z; NIM-FDI appears as **FDI->NIM**.

## What the unconstrained learner finds INSTEAD

Top bootstrap edge-inclusion probabilities, complete-case (undirected max):

| Edge | EIP | Comment |
|------|----:|---------|
| FD-FDI      | 1.000 | direct FD effect, NOT via NIM |
| FD-NIM      | 1.000 | proposed (endpoint) |
| FDI-GDP     | 1.000 | GDP hub |
| GDP-NIM     | 1.000 | GDP hub |
| FD-NPL      | 0.996 | **shortcut FD->NPL** (bypasses NIM+FDI) |
| NPL-Z       | 0.994 | proposed (endpoint) |
| GDP-Z       | 0.988 | GDP hub |
| NIM-NPL     | 0.986 | NIM->NPL direct, bypasses FDI |
| FD-Z        | 0.758 | **direct FD->Z** (bypasses whole chain) |
| FDI-NIM     | 0.470 | proposed NIM-FDI, weak |
| ... | | FDI-NPL 0.108 (proposed, absent) |

Dominant unconstrained-only structure:
1. **GDP is a strong hub, not a passive background confounder** — GDP connects to FDI (1.0), NIM (1.0) and Z (0.988) with near-certain inclusion; in the HC DAG GDP is a parent of NIM and Z and a child of FDI.
2. **Direct FD shortcuts** — FD->NPL (0.996) and FD->Z (0.758) both dominate the indirect route; FD reaches NPL and Z without passing through NIM/FDI.
3. **NIM->NPL direct** (0.986) bypasses FDI.
4. The one link the proposed serial chain most needs, **FDI->NPL, is the single weakest edge in the whole set (0.108).**

## Verdict

**No.** The data-driven DAG does NOT resemble the proposed serial pathway
FD -> NIM -> FDI -> NPL -> Z. Only the two endpoint links (FD-NIM, NPL-Z) are
data-supported; the interior links are weak (NIM-FDI, EIP 0.47) to absent
(FDI-NPL, EIP 0.11), so the chain is severed in the middle. The unconstrained
learner instead promotes GDP to a central hub and prefers direct FD->NPL and
FD->Z shortcuts over the indirect route. This confirms the paper's own critique:
the whitelisted pathway (EIP = 1.0 by construction) is **largely an imposed
artifact**, not a structure the data would recover on its own.

## Blockers / environment notes

- `pcalg` NOT installed; PC run via `bnlearn::pc.stable` (Gaussian `cor` test). Not a blocker.
- `Rgraphviz` present -> figure `learned-dag-6var.pdf` rendered (complete-case HC DAG).
- PC emitted benign "vstructure ... not applicable" warnings (Meek-rule orientation conflicts) — expected on finite data, does not invalidate the CPDAG skeleton.

## Artifacts

- `run-w3-structure-learning.R` — the analysis script.
- `results.md` — this file.
- `learned-dag-edges.csv` — complete-case HC DAG edges with EIP + in_proposed_pathway flag.
- `learned-dag-6var.pdf` — rendered complete-case HC DAG.
- `w3_dump.rds` — full numeric dump (all arc sets, EIP tables, boot.strength).
