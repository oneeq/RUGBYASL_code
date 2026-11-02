# RUGBYASL code

Analysis code for the manuscript:

> Pann Y, Stellmann J-P, et al. *Increased arterial spin labelling perfusion asymmetry following sport-related concussion in professional rugby players.* Brain Communications (in revision).

## Contents

| Script | Purpose |
| --- | --- |
| `code/01_asymmetry_zscores.R` | Vertex-wise perfusion (and cortical-thickness) asymmetry z-score pipeline: reads FreeSurfer xhemi asymmetry maps, computes normative z-scores against a young female reference cohort, derives the individual asymmetry rate (proportion of vertices with |z| > 2.5), and produces Figure 3. |
| `code/02_statistics_figures.R` | Descriptive statistics, linear mixed-effect models (participant as random intercept), ROC analysis with DeLong test, post-hoc power analysis, and Figure 4. |
| `code/03_supplementary_cases_table.R` | Supplementary table of illustrative individual cases and per-case fsaverage z-score map renders (lateral and medial views) for controls and extreme cases. |

## Requirements

- R (≥ 4.1) with the packages `tidyverse`, `lmerTest`, `emmeans`, `pROC`, `gghalves`, `fsbrain`, `freesurferformats`
- A local FreeSurfer installation (for the fsaverage surface; set the path in script 03, default `/Applications/freesurfer/subjects`)

## Data

The clinical and imaging data are **not included** in this repository: the rugby cohort data cannot be shared publicly due to club data-sharing restrictions and patient confidentiality, and are available from the corresponding author on reasonable request. The healthy control reference dataset is the publicly available OpenNeuro dataset [ds000240](https://openneuro.org/datasets/ds000240) ("Resting State Perfusion in Healthy Aging"). Derivative asymmetry maps must be placed in a `data/` folder; generated figures are written to `figures/`.

## Usage

Run the scripts in order, from the repository root:

```r
source("code/01_asymmetry_zscores.R")
source("code/02_statistics_figures.R")
source("code/03_supplementary_cases_table.R")
```

The scripts are designed to be sourced in sequence in the same R session (script 03 re-uses the z-score maps created by script 01).

## License

MIT (see LICENSE). Note that the graphical abstract was created with Inkscape and the figures are © the authors.

## Correspondence

Jan-Patrick Stellmann (corresponding author)
