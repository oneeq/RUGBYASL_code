# =====================================================================
# RUGBYASL - Brain Communications revision (BRAINCOM-2026-758.R1)
# Script 3/3: Supplementary table of illustrative individual cases
#              (response to Reviewer 2, comment 3)
#
# Generates one row per concussion event (n = 34), combining the
# clinical characteristics (Table 1 / REDCap) with the individual
# ASL perfusion asymmetry rate. Optionally renders per-case
# vertex-wise z-score maps on the fsaverage left hemisphere.
#
# Run AFTER scripts 01 and 02 in the same R session. If the per-vertex
# z-score maps from script 01 are missing, section 4 sources script 01
# automatically (the scripts no longer clear the R session).
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
    `subject`                               = subject,
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
#print(suppl_cases, n = Inf, width = Inf)

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
## 4. Optional: render per-case vertex-wise z-score maps on the
##    fsaverage left hemisphere for illustrative cases
## ------------------------------------------------------------------
# The vertex-wise asymmetry maps all live in the common fsaverage
# space (mris_preproc/fsgd workflow), so each case can be rendered
# directly on the standard fsaverage left-hemisphere surface - no
# per-subject FreeSurfer directories needed. Prerequisites: a local
# FreeSurfer installation (for the fsaverage surfaces) and the
# fsbrain and rgl packages.
#fs_home <- Sys.getenv("FREESURFER_HOME")
#if (fs_home == "") stop("FREESURFER_HOME is not set - please start R 
#                         from a FreeSurfer-configured shell or set 
#                         the environment variable.")
subjects_dir <- "/Applications/freesurfer/subjects"   # contains fsaverage

library(fsbrain)
library(rgl)

# fsbrain expects morphometry files inside <subjects_dir>/<subject>/surf,
# but a FreeSurfer installation under /Applications is typically read-only
# (Permission denied when writing lh.z_case.mgh). We therefore render from
# a small writable SHADOW COPY of fsaverage that contains only the surface
# files needed for visualisation; the temporary morphometry file is written
# there instead of into the read-only installation.
render_dir <- file.path(tempdir(), "fs_subjects")
fs_shadow_surf <- file.path(render_dir, "fsaverage", "surf")
dir.create(fs_shadow_surf, recursive = TRUE, showWarnings = FALSE)
for (fname in c("lh.pial", "lh.white", "lh.curv")) {
  dst <- file.path(fs_shadow_surf, fname)
  if (!file.exists(dst)) {
    file.copy(file.path(subjects_dir, "fsaverage", "surf", fname), dst)
  }
}

# Sanity check: the z-score maps must match the fsaverage lh
# surface vertex count; adjust template subject if you used
# e.g. fsaverage6.
fsaverage_lh <- file.path(fs_shadow_surf, "lh.pial")

# Ensure the per-vertex z-score maps from script 01 are available
# (possible because the scripts no longer clear the R session)
if (!exists("rugby_Z_FY") || !exists("MY_Z_FY")) {
  message("z-score maps not found - sourcing script 01...")
  source(file.path("code", "01_asymmetry_zscores.R"))
}

# Select illustrative cases: the 3 rugby examinations with the
# highest asymmetry rates (High_1..High_3), the 3 with the lowest
# (Low_1..Low_3), and 3 typical young male controls from the reference
# cohort (Control_1..Control_3, asymmetry rate closest to the control
# group median).
n_cases <- 3
suppl_cases_compl <- suppl_cases[suppl_cases %>% complete.cases(),]
# Rugby: highest and lowest asymmetry rates
case_info <- rugby_cases %>%
  filter(is.element(subject, suppl_cases_compl$subject)) %>%
  arrange(rate_z_asl)
cases <- bind_rows(
  case_info %>% tail(n_cases) %>% arrange(desc(rate_z_asl)) %>%
    mutate(label = sprintf("High_%d", row_number())),
  case_info %>% head(n_cases) %>%
    mutate(label = sprintf("Low_%d", row_number()))
) %>%
  transmute(label, subject, rate = rate_z_asl,
            source = "rugby_Z_FY")

# Controls: young males of the reference cohort (script 01); pick the
# three subjects with the rate closest to the group median (typical,
# not extreme controls). Rates computed live with the same metric.
ctrl_rates <- sapply(MY_Z_FY, function(z) mean(abs(z) > 2.5, na.rm = TRUE))
ctrl_med   <- median(ctrl_rates)
ctrl_sel   <- names(sort(abs(ctrl_rates - ctrl_med)))[1:n_cases]
cases <- bind_rows(
  cases,
  tibble(
    label  = sprintf("Control_%d", seq_along(ctrl_sel)),
    subject = ctrl_sel,
    rate   = unname(ctrl_rates[ctrl_sel]),
    source = "MY_Z_FY"
  )
)

# Render each case on the fsaverage left hemisphere
for (i in seq_len(nrow(cases))) {
  lab  <- cases$label[i]
  subj <- cases$subject[i]

  # Vertex-wise z-score map of this examination (rugby or control)
  zmap <- get(cases$source[i])[[subj]]
  if (is.null(zmap)) {
    message("No z-score map found for ", subj, " - skipped.")
    next
  }
  if (length(zmap) != nrow(freesurferformats::read.fs.surface(fsaverage_lh)$vertices)) {
    stop("Vertex count of ", subj, " does not match the fsaverage 
          surface - are the maps really in fsaverage space?")
  }

  # Graded colour range [-6, 6]: clamped so that vertices with
  # |z| >= 6 (the reliability threshold of the z-score pipeline) get
  # the full end colours. Narrow to c(-2.5, 2.5) for a saturated
  # display that highlights the asymmetry threshold of the manuscript.
  clim <- c(-3, 6)
  zplot <- pmin(pmax(zmap, clim[1]), clim[2])

  # Write the map as a temporary morphometry file in the writable
  # shadow copy of fsaverage/surf (overwritten at each loop iteration).
  # Two formats, because fsbrain versions differ: recent versions find
  # "lh.z_case.mgh", while older ones look for the extensionless file
  # "lh.z_case" (curv format). Both are harmless if unused.
  write.fs.mgh(filepath = file.path(fs_shadow_surf, "lh.z_case.mgh"),
               data = zplot)
  write.fs.curv(filepath = file.path(fs_shadow_surf, "lh.z_case"),
                data = zplot)

  # Render the fsaverage left hemisphere in the tiled 4-view layout
  # ("t4": lateral, medial, dorsal, ventral angles of the same
  # hemisphere). Colormap: diverging red (z > 0) - white (z = 0) -
  # blue (z < 0). The title reports the case's asymmetry rate (share
  # of vertices with |z| > 2.5, the same metric as in the
  # supplementary table).
  rate <- cases$rate[i]
  fsbrain::vis.subject.morph.native(
    render_dir, "fsaverage", "z_case",
    hemi = "lh", surface = "pial", views = "t4",
    rgloptions = list(windowRect = c(20, 20, 1000, 1000)),
    makecmap_options = list(
      colFn = grDevices::colorRampPalette(
        c("#2166AC", "#F7F7F7", "#B2182B")),
      n = 256))
  rgl::title3d(
    main = sprintf("%s: |z| > 2.5 in %.1f%% of left-hemisphere vertices",
                  lab, 100 * rate))
  print(lab)
  print(subj)
  print(sprintf("%s: |z| > 2.5 in %.1f%% of left-hemisphere vertices",
                lab, 100 * rate))
  Sys.sleep(1)  # let the renderer catch up
  png_file <- file.path(fig_dir, sprintf("%s.png", lab))
  rgl.snapshot(png_file)
  message("Saved ", png_file)
  # rgl renamed rgl.close() to close3d() in recent versions
  if ("close3d" %in% getNamespaceExports("rgl")) {
    rgl::close3d()
  } else {
    rgl::rgl.close()
  }
}

## ------------------------------------------------------------------
## 5. Standalone colourbar (PDF) for the case renders
## ------------------------------------------------------------------
# The surfaces are rendered with the diverging red (z > 0) - white
# (z = 0) - blue (z < 0) colormap over the clamped range clim. This
# standalone scale can be placed next to the assembled supplementary
# figure in Inkscape. Same palette and range as the renders, so they
# always match.
#clim <- c(-6, 6)
n_col <- 256
bar_cols <- grDevices::colorRampPalette(c("#2166AC","#F7F7F7", "#B2182B"))(n_col)
pdf(file.path(fig_dir, "colorbar_z.pdf"), width = 6, height = 1.5)
old_mar <- par(mar = c(3.5, 1, 1, 1))
image(x = seq(clim[1], clim[2], length.out = n_col), y = 1,
      z = matrix(1:n_col, nrow = n_col, ncol = 1),
      col = bar_cols,
      xaxt = "n", yaxt = "n", bty = "n",
      xlab = "Perfusion asymmetry z-score (reference: healthy young females)",
      ylab = "")
axis(side = 1, at = seq(clim[1], clim[2], by = 2))
box()
par(old_mar)
invisible(dev.off())
message("Saved ", file.path(fig_dir, "colorbar_z.pdf"))

# The saved images can be assembled into a supplementary figure, e.g.
# "Supplementary Figure X - Individual ASL perfusion z-score maps on
#  the fsaverage left hemisphere of illustrative cases: the three
#  players with the highest and the three with the lowest asymmetry
#  rates."
