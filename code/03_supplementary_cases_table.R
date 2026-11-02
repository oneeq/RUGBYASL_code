# =====================================================================
# RUGBYASL - Brain Communications revision (BRAINCOM-2026-758.R1)
# Script 3/3: Supplementary table of illustrative individual cases
#              (response to Reviewer 2, comment 3)
#
# Generates one row per concussion event (n = 34), combining the
# clinical characteristics (Table 1 / REDCap) with the individual
# ASL perfusion asymmetry rate.
#
# Run AFTER script 02 in the same R session (needs the `rugby`
# data frame and `rate_z_asl`).
# =====================================================================

## ------------------------------------------------------------------
## 1. Pseudonymise players
## ------------------------------------------------------------------
# Map each participant id to a stable pseudonym P01, P02, ...
player_lookup <- rugby %>%
  distinct(id) %>%
  arrange(id) %>%
  mutate(player = str_c("P", str_pad(row_number(), width = 2, pad = "0")))

rugby_cases <- rugby %>%
  left_join(., player_lookup, by = "id") %>%
  mutate(event_id = str_c(player, "_E", redcap_repeat_instance))

## ------------------------------------------------------------------
## 2. Assemble the supplementary table
## ------------------------------------------------------------------
suppl_cases <- rugby_cases %>%
  arrange(player, redcap_repeat_instance) %>%
  transmute(
    `Event ID`                              = event_id,
    `Player age (years)`                    = round(Age, 1),
    `No. of previous concussions`           = as.numeric(history_tbi_no),
    `Concussion context (match/training)`   = as.character(tbi_context),
    `Loss of consciousness (Y/N)`           = as.character(tbi_lost_consciousness),
    `Delay concussion-HIA3 (days)`          = round(as.numeric(delay), 1),
    `No. of HIA3 symptoms`                  = as.numeric(hia3_symptoms_no),
    `HIA3 memory recall score`               = as.numeric(hia3_memory_recall_score),
    `ASL perfusion asymmetry rate (|z| > 2.5)` = round(rate_z_asl, 3),
    `Qualitative ASL asymmetry on MRI (Y/N)` =
      case_when(
        is.na(mri_asl_anormal.factor) ~ NA_character_,
        mri_asl_anormal.factor %in% c("Yes", "yes", "1",
                                      "Anormal", "Abnormal") ~ "Y",
        TRUE ~ "N"
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
          file.path("data", "RUGBYASL_supplementary_cases.csv"),
          row.names = FALSE, na = "")

# Also write a Word-friendly tab-separated version
write.table(suppl_cases,
            file.path("data", "RUGBYASL_supplementary_cases.docx.tsv"),
            sep = "\t", row.names = FALSE, na = "")

# Quick consistency summary (for the rebuttal):
# agreement between quantitative metric and qualitative reading
cat("\nQualitative ASL asymmetry reading (n):\n")
print(table(rugby_cases$mri_asl_anormal.factor, useNA = "ifany"))
