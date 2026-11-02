# =====================================================================
# RUGBYASL - Brain Communications revision (BRAINCOM-2026-758.R1)
# Script 3/3: Supplementary table of illustrative individual cases
#              (response to Reviewer 2, comment 3)
#
# Generates one row per concussion event (n = 34), combining the
# clinical characteristics (Table 1 / REDCap) with the individual
# ASL perfusion asymmetry rate. Optionally renders per-case
# vertex-wise z-score maps on the left hemisphere.
#
# Run AFTER scripts 01 and 02 in the same R session (needs the
# `rugby` data frame and `rate_z_asl`; section 4 additionally needs
# the per-vertex z-score maps `rugby_Z_FY` from script 01).
# =====================================================================

# ---- Paths ----------------------------------------------------------
data_dir <- "data"
fig_dir  <- "figures"

## ------------------------------------------------------------------
## 1. Pseudonymise players
## ------------------------------------------------------------------
# Map each participant id to a simple stable pseudonym: 1, 2, 3 ...
player_lookup <- rugby %>%
  distinct(id) %>%
  arrange(id) %>%
  mutate(player = row_number())          # simple pseudonym: 1..22

rugby_cases <- rugby %>%
  left_join(., player_lookup, by = "id") %>%
  mutate(concussion_no = as.numeric(redcap_repeat_instance))

## ------------------------------------------------------------------
## 2. Assemble the supplementary table
## ------------------------------------------------------------------
suppl_cases <- rugby_cases %>%
  arrange(player, concussion_no) %>%
  transmute(
    `Player`                                = player,
    `Concussion`                            = concussion_no,
    `Player age (years)`                    = round(Age, 1),
    `No. of previous concussions`           = as.numeric(history_tbi_no),
    `Concussion context (match/training)`   = as.character(tbi_context),
    `Loss of consciousness (Y/N)`           = as.character(tbi_lost_consciousness),
    `Delay concussion-HIA3 (days)`          = round(as.numeric(delay), 1),
    `No. of HIA3 symptoms`                  = as.numeric(hia3_symptoms_no),
    `HIA3 memory recall score`              = as.numeric(hia3_memory_recall_score),
    `ASL perfusion asymmetry rate (|z| > 2.5)` = round(rate_z_asl, 3),
    `Qualitative ASL asymmetry on MRI (Y/N)` =
      case_when(
        is.na(mri_asl_anormal.factor) ~ NA_character_,
        mri_asl_anormal.factor == "Asymetrie en perfusion" ~ "Y",
        mri_asl_anormal.factor == "Non" ~ "N",
        TRUE ~ NA_character_   # other/technical categories -> missing
      )
  )

# Check completeness
cat("Rows (should equal number of concussion events, n = 34):",
    nrow(suppl_cases), "\n")
print(suppl_cases, n = Inf, width = Inf)

## ------------------------------------------------------------------
## 3. Export
## ------------------------------------------------------------------
# CSV for further editing / formatting in Word
write.csv(suppl_cases,
          file.path(data_dir, "RUGBYASL_supplementary_cases.csv"),
          row.names = FALSE, na = "")

# Also write a Word-friendly tab-separated version
write.table(suppl_cases,
            file.path(data_dir, "RUGBYASL_supplementary_cases.docx.tsv"),
            sep = "\t", row.names = FALSE, na = "")

# Quick consistency summary (for the rebuttal):
# agreement between quantitative metric and qualitative reading
cat("\nQualitative ASL asymmetry reading (n):\n")
print(table(rugby_cases$mri_asl_anormal.factor, useNA = "ifany"))

## ------------------------------------------------------------------
## 4. Optional: render per-case vertex-wise z-score maps (left
##    hemisphere) for illustrative cases
## ------------------------------------------------------------------
# Prerequisites: FreeSurfer SUBJECTS_DIR of the rugby cohort (subject
# IDs must match the fsgd names, e.g. "sub-XXXXXXXXX_ses-01") and
# the fsbrain and rgl packages (used for surface rendering).
subjects_dir <- "path/to/your/subjects_dir"   # <-- ADAPT to your setup

library(fsbrain)
library(rgl)

# Select illustrative cases: n highest and n lowest asymmetry rates
n_cases <- 3
case_info <- rugby_cases %>%
  filter(!is.na(rate_z_asl)) %>%
  arrange(rate_z_asl)
cases <- bind_rows(
  case_info %>% head(n_cases),       # lowest rates (near-normal)
  case_info %>% tail(n_cases)        # highest rates (marked asymmetry)
) %>% distinct()

# Render each case on the left-hemisphere surface
for (i in seq_len(nrow(cases))) {
  subj   <- cases$subject[i]
  player <- cases$player[i]
  event  <- cases$concussion_no[i]

  # Vertex-wise z-score map of this examination (from script 01)
  zmap <- rugby_Z_FY[[subj]]
  if (is.null(zmap)) {
    message("No z-score map found for ", subj, " - skipped.")
    next
  }

  # Write the map as a morphometry file into the subject's surf dir
  mgh_file <- file.path(subjects_dir, subj, "surf", "z_asl_sym.mgh")
  dir.create(dirname(mgh_file), recursive = TRUE, showWarnings = FALSE)
  write.fs.mgh(filepath = mgh_file, data = zmap)

  # Render on the left hemisphere. Saturated colour range [-2.5, 2.5]
  # makes suprathreshold vertices (|z| > 2.5) stand out; widen to
  # c(-6, 6) for a graded display. Colormaps: fsbrain::cm.available()
  # (e.g. "RdBu", "RdYlBu", "jet", "inferno").
  vis <- tryCatch(
    fsbrain::vis.subject.morph.native(
      subjects_dir, subj, "z_asl_sym",
      hemi = "lh", surface = "pial",
      cmap = "RdBu", clim = c(-2.5, 2.5)),
    error = function(e) {
      # Fallback for older fsbrain versions
      fsbrain::vis.subject.surface(
        subjects_dir, subj, hemi = "lh", surface = "pial",
        morphometry = "z_asl_sym")
    })

  Sys.sleep(1)  # let the renderer catch up
  png_file <- file.path(fig_dir,
                       sprintf("case_player%02d_concussion%d.png",
                               player, event))
  rgl.snapshot(png_file)
  message("Saved ", png_file)
  rgl.close()
}

# The saved images can be assembled into a supplementary figure, e.g.
# "Supplementary Figure X - Individual ASL perfusion z-score maps of
#  illustrative cases: the three players with the highest and the
#  three with the lowest asymmetry rates (left hemisphere)."
