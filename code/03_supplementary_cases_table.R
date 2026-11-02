successfully downloaded text file (SHA: fa50d49cb6fa9900f4430bfd972287912a42a879)
## ------------------------------------------------------------------
## 4. Optional: render per-case vertex-wise z-score maps (left
##    hemisphere) for illustrative cases
## ------------------------------------------------------------------
# Prerequisites:
#   - script 01 must have been run in this session (it provides the
#     per-vertex z-score maps 'rugby_Z_FY'), and script 02 for 'rugby'.
#   - FreeSurfer SUBJECTS_DIR of the rugby cohort (subject IDs must
#     match the fsgd names, e.g. "sub-XXXXXXXXX_ses-01").
#   - the fsbrain and rgl packages (used for surface rendering).
subjects_dir <- "path/to/your/subjects_dir"   # <-- ADAPT to your setup

library(fsbrain)
library(rgl)

# Select illustrative cases: n highest and n lowest asymmetry rates
n_cases <- 3
case_info <- rugby_cases %>%
  left_join(., player_lookup, by = "id") %>%
  mutate(concussion_no = as.numeric(redcap_repeat_instance)) %>%
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
