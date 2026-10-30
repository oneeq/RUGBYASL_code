# RUGBYASL — Analysis code

Analysis code for:

> Pann A, Nicolescu M, Pora J, Martin-Barbaz L, Hak J-F, Girard N,
> Testud B, Dufour H, Stellmann J-P.
> **Brain perfusion asymmetry as a biomarker of sport-related concussion
> in professional rugby players.**
> *Brain Communications* (manuscript BRAINCOM-2026-758.R1)

This repository contains the R code used to (1) compute surface-based
ASL perfusion and cortical-thickness asymmetry z-scores and the
individual asymmetry-rate metric, and (2) perform the statistical
analyses and generate the figures reported in the manuscript.

## Repository structure

```
RUGBYASL_code/
├── code/
│   ├── 01_asymmetry_zscores.R   # z-score pipeline + Figure 3
│   └── 02_statistics_figures.R  # group/association models + Figure 4
├── data/                        # input files (not included, see below)
├── figures/                     # generated figures
└── README.md
```

## Requirements

- **R** ≥ 4.2
- R packages: `tidyverse`, `fsbrain`, `freesurferformats`, `gghalves`,
  `lmerTest`, `effsize`, `pROC`, `simr`, `ggeffects`, `readxl`,
  `lubridate`, `wesanderson` (see `code/` headers for which script uses
  which package)
- FreeSurfer (v7.x) with the **xhemi** pipeline must be run beforehand:
  the scripts start from the vertex-wise asymmetry `.mgh` maps produced
  by `xhemi-registro` + `fscalc` (`lh.lh-rh.*.mgh`).

## Input data (not included)

The scripts read the following files from the working directory
(or `data/` if you adapt the paths):

| File | Content |
| --- | --- |
| `FSGD_ASLREF_PERF.fsgd` | FreeSurfer subject list, OpenNeuro ds000240 reference cohort |
| `ASL_participants.tsv` | Age / sex table for ds000240 |
| `diff.volperf.sm20.aslref.mgh` | Vertex-wise ASL perfusion asymmetry, reference cohort |
| `diff.volperf.sm20.rugby.mgh` | Vertex-wise ASL perfusion asymmetry, rugby cohort |
| `lh.lh-rh.thickness.sm20.aslref.mgh` | Vertex-wise cortical-thickness asymmetry, reference cohort |
| `lh.lh-rh.thickness.sm20.rugby.mgh` | Vertex-wise cortical-thickness asymmetry, rugby cohort |
| `Rugby_R_2025-03-04_1430.r` | REDCap export (clinical, HIA3, neuropsychology) |
| `rugby_analysis_outline_v2.xlsx` | Analysis outline / session inclusion list |

Raw clinical and imaging data are **not** distributed because of
patient-confidentiality restrictions; the reference cohort
(ds000240, "Resting State Perfusion in Healthy Aging") is openly
available on [OpenNeuro](https://openneuro.org/datasets/ds000240).

## Usage

```r
# 1) Asymmetry z-scores and rate metric (writes rugby_z_scores.rds
#    and the Figure 3 PDFs)
source("code/01_asymmetry_zscores.R")

# 2) Statistics, power analyses, ROC and Figure 4
source("code/02_statistics_figures.R")
```

Run order matters: script 02 needs `rugby_z_scores.rds` produced by
script 01. Set the working directory to the project root (or adjust
paths) before running.

## Notes

- Vertex-wise z-scores are computed against the **young female**
  subgroup of ds000240 (most conservative reference); the group
  comparison uses the **young male** subgroup (n = 15).
- `|z| ≥ 6` is used as a reliability filter (≈ 99th percentile of the
  young male control distribution).
- Group comparisons and brain-behaviour associations use **linear
  mixed models with a random intercept per participant**, accounting
  for repeated concussion events within players (22 players,
  34 events).
- One-sided p-values are reported for associations (hypothesis:
  higher asymmetry ↔ worse performance), with FDR correction across
  the neuropsychological battery.

## License

MIT (see LICENSE). Code is provided as-is for reproducibility of the
published analyses.
