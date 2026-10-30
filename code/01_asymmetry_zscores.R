# =====================================================================
# RUGBYASL - Brain Communications revision (BRAINCOM-2026-758.R1)
# Script 1/2: Surface-based ASL perfusion and cortical-thickness
#              asymmetry, vertex-wise z-scores, and individual
#              asymmetry-rate metric (Figure 3)
#
# Requires: FreeSurfer xhemi outputs (.mgh asymmetry maps),
#           FSGD subject lists and OpenNeuro ds000240 participants.tsv
# =====================================================================

rm(list = ls())

library(tidyverse)
library(fsbrain)
library(freesurferformats)
library(gghalves)

## ------------------------------------------------------------------
## 0. Paths
## ------------------------------------------------------------------
# All input files are expected in the "data" folder of this repository
# (see README); generated figures are written to the "figures" folder.
# Adjust these two lines only if you use a different layout. Note that
# relative paths refer to the *current working directory*, not the
# script location: set the working directory to the repository root
# (e.g. setwd("path/to/RUGBYASL_code")) before sourcing the scripts.
data_dir <- "data"
fig_dir  <- "figures"


## ------------------------------------------------------------------
## 1. Cohort definitions
## ------------------------------------------------------------------
# OpenNeuro reference dataset (ds000240, "Resting State Perfusion in
# Healthy Aging"): 63 healthy subjects, wide age range.
refs <- read.delim(file.path(data_dir, "FSGD_ASLREF_PERF.fsgd"), skip = 4, sep = " ", fill = TRUE,
                   header = FALSE)
names(refs) <- c("Input", "participant_id", "CLASS")
refs_info <- read.delim(file.path(data_dir, "ASL_participants.tsv"))
refs <- left_join(refs, refs_info) %>%
  mutate(AgeGroup = ifelse(Age < 50, "Y", "O"),
         AgeSexGroup = str_c(AgeGroup, Gender))

# Rugby cohort (per concussion event / MRI session)
rugs <- read.delim(file.path(data_dir, "FSGD_RUGBY_PERF.fsgd"), skip = 4, sep = " ", fill = TRUE,
                   header = FALSE)

# Quality-control exclusions
excl.aslref <- c("sub-05", "sub-10", "sub-12", "sub-17", "sub-33", "sub-36",
                 "sub-38", "sub-39", "sub-41", "sub-42", "sub-44", "sub-47",
                 "sub-52", "sub-56", "sub-59")
excl.rugby <- c("sub-2100892381_ses-01", "sub-2100897786_ses-01",
                "sub-2100914586_ses-04", "sub-2101045664_ses-01",
                "Sub-2101020459_ses-03")

## ------------------------------------------------------------------
## 2. Vertex-wise asymmetry maps (FreeSurfer xhemi outputs)
## ------------------------------------------------------------------
# ASL perfusion asymmetry (smoothed, 20 mm kernel)
diffrel.volperf.sm20.aslref <- as_tibble(
  read.fs.mgh(file.path(data_dir, "diff.volperf.sm20.aslref.mgh")))
diffrel.volperf.sm20.rugby <- as_tibble(
  read.fs.mgh(file.path(data_dir, "diff.volperf.sm20.rugby.mgh"), with_header = FALSE))

# Cortical thickness asymmetry (same pipeline)
diffthx.rugby   <- as_tibble(read.fs.mgh(file.path(data_dir, "lh.lh-rh.thickness.sm20.rugby.mgh")))
diffthx.aslref  <- as_tibble(read.fs.mgh(file.path(data_dir, "lh.lh-rh.thickness.sm20.aslref.mgh")))

# Apply exclusions
names(diffrel.volperf.sm20.aslref) <- refs$participant_id
diffrel.volperf.sm20.aslref <- diffrel.volperf.sm20.aslref %>%
  select(which(!names(.) %in% excl.aslref))
names(diffthx.aslref) <- refs$participant_id
diffthx.aslref <- diffthx.aslref %>%
  select(which(!names(.) %in% excl.aslref))
refs <- refs %>% filter(!participant_id %in% excl.aslref)

names(diffrel.volperf.sm20.rugby) <- rugs$V2
diffrel.volperf.sm20.rugby <- diffrel.volperf.sm20.rugby %>%
  select(which(!names(.) %in% excl.rugby))
names(diffthx.rugby) <- rugs$V2
diffthx.rugby <- diffthx.rugby %>%
  select(which(!names(.) %in% excl.rugby))
rugs <- rugs %>% filter(!V2 %in% excl.rugby)

## ------------------------------------------------------------------
## 3. Normative means/SDs for z-score computation
## ------------------------------------------------------------------
# Z-score reference cohort: YOUNG FEMALES (FY) of ds000240
# (lowest asymmetry of all subgroups, most conservative reference).
# Comparison cohort: YOUNG MALES (YM).
meandiffallFY <- diffrel.volperf.sm20.aslref[, refs$AgeSexGroup == "YF"] %>%
  reframe(meanFY = apply(., 1, mean),
          sdFY   = apply(., 1, sd))
meandiffallMY <- diffrel.volperf.sm20.aslref[, refs$AgeSexGroup == "YM"] %>%
  reframe(meanMY = apply(., 1, mean),
          sdMY   = apply(., 1, sd))

thxmeanFY <- diffthx.aslref[, refs$AgeSexGroup == "YF"] %>%
  reframe(meanFY = apply(., 1, mean),
          sdFY   = apply(., 1, sd))
thxmeanMY <- diffthx.aslref[, refs$AgeSexGroup == "YM"] %>%
  reframe(meanMY = apply(., 1, mean),
          sdMY   = apply(., 1, sd))

## ------------------------------------------------------------------
## 4. Vertex-wise z-scores (reference: young females)
## ------------------------------------------------------------------
rugby_Z_FY <- diffrel.volperf.sm20.rugby %>%
  mutate_all(~ (.x - meandiffallFY$meanFY) / meandiffallFY$sdFY)
names(rugby_Z_FY) <- rugs$V2

MY_Z_FY <- diffrel.volperf.sm20.aslref[refs$AgeSexGroup == "YM"] %>%
  mutate_all(~ (.x - meandiffallFY$meanFY) / meandiffallFY$sdFY)

thx_rugby_Z_FY <- diffthx.rugby %>%
  mutate_all(~ (.x - thxmeanFY$meanFY) / thxmeanFY$sdFY)
names(thx_rugby_Z_FY) <- rugs$V2

thx_MY_Z_FY <- diffthx.aslref[refs$AgeSexGroup == "YM"] %>%
  mutate_all(~ (.x - thxmeanFY$meanFY) / thxmeanFY$sdFY)

# Long format
long_Z_FY <- bind_rows(
  rugby_Z_FY %>%
    mutate(node = 1:nrow(.)) %>%
    pivot_longer(cols = starts_with("s")) %>%
    mutate(Cohort = "Rugby",
           subject = str_replace(name, "sub", "RUGBY")),
  MY_Z_FY %>%
    mutate(node = 1:nrow(.)) %>%
    pivot_longer(cols = starts_with("s")) %>%
    mutate(Cohort = "MY",
           subject = str_replace(name, "sub", "MY"))
)

thx_long_Z_FY <- bind_rows(
  thx_rugby_Z_FY %>%
    mutate(node = 1:nrow(.)) %>%
    pivot_longer(cols = starts_with("s")) %>%
    mutate(Cohort = "Rugby",
           subject = str_replace(name, "sub", "RUGBY")),
  thx_MY_Z_FY %>%
    mutate(node = 1:nrow(.)) %>%
    pivot_longer(cols = starts_with("s")) %>%
    mutate(Cohort = "MY",
           subject = str_replace(name, "sub", "MY"))
)

# Empirical justification of the reliability filter:
# |z| = 6 corresponds approximately to the 99th percentile of the
# young male control group's absolute z-score distribution.
long_Z_FY %>%
  filter(Cohort == "MY") %>%
  summarise(q99 = quantile(abs(value), probs = 0.99, na.rm = TRUE))

## ------------------------------------------------------------------
## 5. Individual asymmetry-rate metric
## ------------------------------------------------------------------
ZFILTER <- 6  # unreliable z-scores (|z| >= 6) set to missing

rate_Z_scores <- long_Z_FY %>%
  group_by(subject, Cohort) %>%
  mutate(value = ifelse(abs(value) < ZFILTER, value, NA)) %>%
  summarise(rate = sum(abs(value) > 2.5, na.rm = TRUE) / sum(!is.na(value)))

thx_rate_Z_scores <- thx_long_Z_FY %>%
  group_by(subject, Cohort) %>%
  mutate(value = ifelse(abs(value) < ZFILTER, value, NA)) %>%
  summarise(rate = sum(abs(value) > 2.5, na.rm = TRUE) / sum(!is.na(value)))

## ------------------------------------------------------------------
## 6. Export for statistical analysis (Script 2)
## ------------------------------------------------------------------
rugbyexp <- rate_Z_scores %>%
  rename(rate_z_asl = rate) %>%
  left_join(thx_rate_Z_scores %>%
              rename(rate_z_thx = rate))

write_rds(rugbyexp, file = file.path(data_dir, "rugby_z_scores.rds"))

## ------------------------------------------------------------------
## 7. Figure 3
## ------------------------------------------------------------------
# Panel colours
colors10 <- c("Young males" = "#01c552", "Rugby" = "#e1302e")

# Panels A / C: per-vertex z-score heat maps per subject
plot_zscore_heatmap <- function(dat, ylab_text, filename) {
  dat %>%
    rename(`z-score` = value) %>%
    mutate(Cohort = factor(ifelse(Cohort == "MY", "Young males", Cohort),
                           levels = c("Young males", "Rugby"),
                           ordered = TRUE)) %>%
    filter(abs(`z-score`) < 6) %>%
    ggplot(aes(subject, `z-score`)) +
    geom_bin2d(binwidth = c(1, 0.1)) +
    theme_classic() +
    scale_fill_viridis_c(option = "rocket", name = "Number of vertices",
                         limits = c(0, 10000)) +
    geom_hline(yintercept = 2.5, color = "goldenrod", linewidth = 2) +
    geom_hline(yintercept = 0, color = "white", linewidth = 2) +
    geom_hline(yintercept = -2.5, color = "goldenrod", linewidth = 2) +
    annotate("text", x = 1, y = 2.8, label = "+2.5 SD",
             color = "goldenrod", size = 4, hjust = 0) +
    annotate("text", x = 1, y = -2.8, label = "-2.5 SD",
             color = "goldenrod", size = 4, hjust = 0) +
    ylab(ylab_text) + xlab("Subjects") +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "right",
          panel.background = element_rect(fill = "black")) +
    coord_cartesian(ylim = c(-3, 5)) +
    facet_wrap(. ~ Cohort, scales = "free")
  ggsave(file.path(fig_dir, filename), width = 7, height = 3)
}

plot_zscore_heatmap(long_Z_FY,
                    "z-score ASL perfusion\n[young females as reference]",
                    "RUGBY_ASL_zscores_MY_Rugby_FYreference.pdf")
plot_zscore_heatmap(thx_long_Z_FY,
                    "z-score cortical thickness\n[young females as reference]",
                    "RUGBY_THX_zscores_MY_Rugby_FYreference.pdf")

# Panels B / D: individual asymmetry rates (half violin + box + points)
plot_rate_boxplot <- function(dat, xlab_text, filename) {
  dat %>%
    mutate(Cohort = factor(ifelse(Cohort == "MY", "Young males", Cohort),
                           levels = c("Young males", "Rugby"),
                           ordered = TRUE)) %>%
    ggplot(aes(Cohort, rate, fill = Cohort)) +
    geom_half_violin(side = "l", color = "black", nudge = 0) +
    geom_half_boxplot(side = "r", center = TRUE, outlier.shape = NA,
                      color = "black", nudge = 0.05, width = 0.9,
                      errorbar.draw = TRUE) +
    geom_jitter() +
    theme_minimal() +
    scale_fill_manual(values = colors10) +
    ylab("Rate of vertex z-scores above 2.5") +
    guides(fill = "none") +
    xlab(xlab_text) +
    ylim(c(0, 0.7))
  ggsave(file.path(fig_dir, filename), width = 3, height = 3)
}

plot_rate_boxplot(rate_Z_scores, "ASL perfusion asymmetry",
                  "RUGBY_ASL_zscores_Rates_FYreference.pdf")
plot_rate_boxplot(thx_rate_Z_scores, "Cortical thickness asymmetry",
                  "RUGBY_THX_zscores_Rates_FYreference.pdf")
