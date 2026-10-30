# =====================================================================
# RUGBYASL - Brain Communications revision (BRAINCOM-2026-758.R1)
# Script 2/2: Cohort description, group comparisons (linear mixed
#              models), ROC / DeLong analysis, brain-behaviour
#              associations, power analyses, and Figure 4
#
# Requires: the REDCap data export ("Rugby_R_....r"),
#           rugby_analysis_outline_v2.xlsx, and the output of
#           Script 1 (rugby_z_scores.rds)
# =====================================================================

rm(list = ls())

library(tidyverse)
library(lmerTest)
library(effsize)
library(pROC)
library(simr)
library(ggeffects)

## ------------------------------------------------------------------
## 1. Helper functions
## ------------------------------------------------------------------
not_all_na <- function(x) any(!is.na(x))
not_all_empty <- function(x) any(x != "")

# "mean (SD); median [min-max]" summary string
mean_jps <- function(x, na.rm = TRUE, r = 2) {
  str_c(round(mean(x, na.rm = na.rm), r), " (",
        round(sd(x, na.rm = na.rm), r), "); ",
        round(median(x, na.rm = na.rm), r), " [",
        round(min(x, na.rm = na.rm), r), "-",
        round(max(x, na.rm = na.rm), r), "]")
}

# "level: n (percent)" summary string for factors
n_jps <- function(x, na.rm = TRUE, r = 1) {
  t <- table(x)
  res <- ""
  for (i in seq_along(t)) {
    res <- str_c(res, names(t)[i], ": ", t[i], " (",
                 round(t[i] * 100 / sum(t), r), "%)/")
  }
  str_replace(res, "/$", "")
}

# Simulation-based power for the ASL-brain-behaviour association,
# using the observed random-intercept and residual variances.
sim_power_assoc <- function(effect, tau2 = 0, sigma2, n_sim = 1000,
                            alpha = 0.05) {
  ids <- unique(rugby$id)
  n_id <- length(ids)
  successes <- 0
  for (i in 1:n_sim) {
    re <- rnorm(n_id, 0, sqrt(tau2))
    names(re) <- ids
    sim_data <- rugby
    sim_data$y_sim <- effect * sim_data$z_rate_asl +
      re[as.character(sim_data$id)] +
      rnorm(nrow(sim_data), 0, sqrt(sigma2))
    fit <- suppressMessages(suppressWarnings(
      lmer(y_sim ~ z_rate_asl + (1 | id), data = sim_data)))
    t_val <- coef(summary(fit))["z_rate_asl", "t value"]
    df_val <- coef(summary(fit))["z_rate_asl", "df"]
    p_one_sided <- pt(t_val, df = df_val, lower.tail = TRUE)
    if (!is.na(p_one_sided) && p_one_sided < alpha) {
      successes <- successes + 1
    }
  }
  ci <- binom.test(successes, n_sim)$conf.int
  tibble(effect = effect, power = successes / n_sim,
         ci_low = ci[1], ci_high = ci[2])
}

## ------------------------------------------------------------------
## 2. Clinical data import and preparation
## ------------------------------------------------------------------
source(file = "Rugby_R_2025-03-04_1430.r")   # REDCap export

master <- readxl::read_xlsx("rugby_analysis_outline_v2.xlsx", 1)
zscores <- read_rds("rugby_z_scores.rds")

master <- master %>%
  filter(perf == "folder" & !is.na(perf))
subs <- str_replace(unique(master$Subject), "S", "s")

# Inclusion data (one row per player)
incl <- data %>%
  filter(!is.na(ipp)) %>%
  mutate(subject = str_c("sub-", ipp)) %>%
  select(where(not_all_na)) %>%
  select(where(not_all_empty)) %>%
  select(-redcap_event_name, -record_id, -redcap_event_name.factor)

# Per-event data (concussion, MRI, HIA3, neuropsychology)
comm <- data %>%
  filter(redcap_event_name == "commotion_arm_1") %>%
  select(where(not_all_na)) %>%
  select(where(not_all_empty)) %>%
  left_join(., incl) %>%
  filter(is.element(subject, subs))

mri  <- comm %>% select(id, subject, redcap_repeat_instance, starts_with("mri"))
hia3 <- comm %>% select(id, subject, redcap_repeat_instance, starts_with("hia3"))
np   <- comm %>% select(id, subject, redcap_repeat_instance, starts_with("np"))

comm <- comm %>%
  select(id, redcap_repeat_instance, tbi_date, tbi_lost_consciousness,
         lost_consciousness_time, tbi_impact_direction, tbi_context,
         sport, position, level, competition, height, weight)

rugby <- left_join(mri, incl) %>%
  left_join(., comm) %>%
  left_join(., hia3) %>%
  left_join(., np)

rugby <- rugby %>%
  mutate(subject = str_c(subject, "_ses-0", redcap_repeat_instance))

# Merge imaging z-score metrics (Script 1) with clinical data
rugby <- left_join(rugby,
                   zscores %>% mutate(subject = str_replace(subject, "RUGBY-", "sub-")))

rugby <- rugby %>%
  mutate(Age = lubridate::year(mri_date) - yobirth,
         tbi_lost_consciousness = factor(tbi_lost_consciousness,
                                         levels = c(1, 2),
                                         labels = c("Yes", "No")),
         tbi_context = factor(tbi_context, levels = c(1, 2, 3),
                              labels = c("Match", "Training", "Other")),
         mri_date = as.POSIXct(mri_date),
         tbi_date = ifelse(tbi_date == "", NA, tbi_date),
         tbi_date = as.POSIXct(tbi_date),
         delay = (mri_date - tbi_date) / 24)

# Keep the imaging z-scores of the rugby players (exclude the
# reference/comparison cohorts for the analyses below)
zselector <- str_replace(rugby$subject, "sub-", "RUGBY-")
zscores <- zscores[(is.element(zscores$subject, zselector) +
                      str_detect(zscores$subject, "MY") +
                      str_detect(zscores$subject, "aphm")) == 1, ]
zscores <- zscores %>% mutate(id = str_remove(subject, "_ses-.*"))

## ------------------------------------------------------------------
## 3. Descriptive statistics (Tables 1 and 2)
## ------------------------------------------------------------------
# Demographics at first visit
rugby %>%
  filter(redcap_repeat_instance == 1) %>%
  summarise(Sex = n_jps(sex.factor),
            Age = mean_jps(Age),
            `Country of Birth` = n_jps(toupper(birth_country)),
            Handedness = n_jps(hand.factor),
            `Previous TBI` = mean_jps(history_tbi_no),
            `Age at first TBI` =
              mean_jps(as.numeric(str_extract(history_age_first_tbi, "[0-9]{1,2}")))) %>%
  t() %>% print()

# Concussion characteristics (Table 1)
rugby %>%
  summarise(`Age at TBI` = mean_jps(Age),
            Height = mean_jps(height),
            Weight = mean_jps(weight),
            `Loss of consciousness` = n_jps(tbi_lost_consciousness),
            `TBI context` = n_jps(tbi_context),
            Position = n_jps(position),
            `HIA 1` = n_jps(hia3_recap_hia1.factor),
            `HIA 2` = n_jps(hia3_recap_hia2.factor),
            Quarter = n_jps(hia3_quarter),
            Situation = n_jps(hia3_situation.factor),
            Collision = n_jps(hia3_collision.factor),
            Contact = n_jps(hia3_contact.factor)) %>%
  t() %>% print()

# MRI findings and imaging metrics
rugby %>%
  summarise(delay = mean_jps(delay),
            FLAIR = n_jps(mri_flair_anormal.factor),
            ASL = n_jps(mri_asl_anormal.factor),
            Diffusion = n_jps(mri_diff_anormal.factor),
            Microbleeds = n_jps(mri_microbleeds.factor),
            Other = n_jps(mri_other_anormal.factor),
            rate_z_asl = mean_jps(rate_z_asl),
            rate_z_thx = mean_jps(rate_z_thx)) %>%
  t() %>% print()

# Imaging summary by cohort (controls vs players)
zscores %>%
  group_by(Cohort) %>%
  summarise(n = n(),
            rate_z_asl = mean_jps(rate_z_asl),
            rate_z_thx = mean_jps(rate_z_thx)) %>%
  t() %>% print()

# HIA3 and symptoms (Table 2)
rugby %>%
  summarise(Orientation = mean_jps(hia3_orientation_score),
            `Memory immediate` = mean_jps(hia3_memory_immediate),
            `Memory recall` = mean_jps(hia3_memory_recall_score),
            `Memory recall delayed` = mean_jps(hia3_memory_recall_delayed),
            Concentration = mean_jps(hia3_concentration_score),
            `Number of Symptoms` = mean_jps(hia3_symptoms_no),
            `No symptoms` = n_jps(hia3_symptoms_no == 0),
            `Severity of symptoms` = mean_jps(hia3_symptoms_severity),
            `Neurological examination` = n_jps(hia3_neuro),
            Romberg = mean_jps(hia3_bipodal),
            `One leg` = mean_jps(hia3_monopodal),
            Tandem = mean_jps(hia3_tandem),
            `Results HIA3` = n_jps(hia3_result)) %>%
  t() %>% print()

# Extended neuropsychological battery (Table 2)
rugby %>%
  summarise(np_mdc = mean_jps(np_mdc),
            np_numbers_fw = mean_jps(np_numbers_fw),
            np_mpc_bw = mean_jps(np_mpc_bw),
            np_mdc_inc = mean_jps(np_mdc_inc),
            np_tmt_a = mean_jps(np_tmt_a),
            np_tmt_b = mean_jps(np_tmt_b),
            np_tmt_ba = mean_jps(np_tmt_ba),
            np_stroop_1 = mean_jps(np_stroop_1),
            np_stroop_2 = mean_jps(np_stroop_2),
            np_stroop_3 = mean_jps(np_stroop_3),
            np_stroop_3_1 = mean_jps(np_stroop_3_1),
            np_pasat_raw = mean_jps(np_pasat_raw),
            np_code_raw = mean_jps(np_code_raw),
            np_symbol_raw = mean_jps(np_symbol_raw)) %>%
  t() %>% print()

# Sample sizes available per neuropsychological outcome
rugby %>%
  select(subject, matches("_z$")) %>%
  mutate(across(matches("_z$"), as.numeric)) %>%
  pivot_longer(cols = starts_with("np")) %>%
  filter(!is.na(value)) %>%
  group_by(name) %>%
  summarise(n = n())

## ------------------------------------------------------------------
## 4. Group comparisons (linear mixed models, Figure 3 statistics)
## ------------------------------------------------------------------
# Unadjusted effect sizes
effsize::cohen.d(rate_z_asl ~ Cohort, data = zscores %>% filter(Cohort != "APHM"))
effsize::cohen.d(rate_z_thx ~ Cohort, data = zscores %>% filter(Cohort != "APHM"))

# Mixed models with random intercept per participant
# (accounts for repeated concussion events within the same player)
fit_asl_grp <- lmer(rate_z_asl ~ Cohort + (1 | id),
                    data = zscores %>% filter(Cohort != "APHM"))
summary(fit_asl_grp)
confint(fit_asl_grp, method = "Wald")

fit_asl_grpthx <- lmer(rate_z_thx ~ Cohort + (1 | id),
                       data = zscores %>% filter(Cohort != "APHM"))
summary(fit_asl_grpthx)
confint(fit_asl_grpthx, method = "Wald")

# Simulation-based power for the observed fixed effects
power_grp    <- powerSim(fit_asl_grp,
                         test = fixed("CohortRugby", "t"), nsim = 1000)
power_grp
power_grpthx <- powerSim(fit_asl_grpthx,
                         test = fixed("CohortRugby", "z"), nsim = 1000)
power_grpthx

## ------------------------------------------------------------------
## 5. ROC analysis (ASL perfusion vs cortical thickness asymmetry)
## ------------------------------------------------------------------
# AUC computed on per-participant averages (one value per individual)
auc_data_collapsed <- zscores %>%
  filter(Cohort %in% c("Rugby", "MY")) %>%
  group_by(id, Cohort) %>%
  summarise(rate_z_asl = mean(rate_z_asl, na.rm = TRUE),
            rate_z_thx = mean(rate_z_thx, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(group = ifelse(Cohort == "Rugby", 1, 0))

roc_asl_c <- roc(auc_data_collapsed$group, auc_data_collapsed$rate_z_asl)
roc_thx_c <- roc(auc_data_collapsed$group, auc_data_collapsed$rate_z_thx)

auc(roc_asl_c); ci.auc(roc_asl_c)
auc(roc_thx_c); ci.auc(roc_thx_c)
roc.test(roc_asl_c, roc_thx_c, method = "delong")

## ------------------------------------------------------------------
## 6. Shapiro-Wilk normality screening and variable selection
## ------------------------------------------------------------------
np_selector_full <- c(
  "hia3_memory_immediate", "hia3_memory_recall_score",
  "hia3_memory_recall_delayed", "hia3_concentration_score",
  "hia3_symptoms_no", "hia3_symptoms_severity",
  "np_mdc", "np_numbers_fw", "np_mpc_bw", "np_mdc_inc",
  "np_tmt_a", "np_tmt_b", "np_tmt_ba",
  "np_stroop_1", "np_stroop_2", "np_stroop_3", "np_stroop_3_1",
  "np_pasat_raw"
)

normality_check <- rugby %>%
  select(all_of(np_selector_full)) %>%
  summarise(across(everything(), ~ {
    x <- na.omit(as.numeric(.x))
    if (length(unique(x)) < 3 || length(x) < 3) return(NA_real_)
    shapiro.test(x)$p.value
  })) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "shapiro_p")

normality_check %>% arrange(shapiro_p)

# Final set of variables retained for parametric analysis
# (variables with substantial deviation from normality excluded)
np_selector <- c("hia3_memory_immediate", "hia3_memory_recall_score",
                 "hia3_memory_recall_delayed", "np_numbers_fw",
                 "np_tmt_b", "np_tmt_ba", "np_stroop_3_1", "np_pasat_raw")

## ------------------------------------------------------------------
## 7. Associations between asymmetry and cognition
##    (linear mixed models, one-sided p, FDR correction)
## ------------------------------------------------------------------
cors_select <- tibble(
  out = np_selector,
  # a priori direction: "g" = lower score means worse performance,
  #                      "less" = higher score means worse performance
  alt = case_when(
    str_detect(out, "_tmt_") ~ "g",
    str_detect(out, "_stroop_") ~ "g",
    TRUE ~ "less"
  )
)

cors_new <- tibble()
for (i in 1:nrow(cors_select)) {
  out_name <- cors_select$out[i]
  alt <- cors_select$alt[i]

  # LMM: standardised clinical outcome ~ standardised ASL asymmetry rate
  #      + random intercept per participant
  fit <- lmer(formula(str_c("scale(", out_name, ") ~ scale(rate_z_asl) + (1 | id)")),
              data = rugby)
  beta <- summary(fit)$coefficients[2, 1]
  p_one_sided <- pt(coef(summary(fit))["scale(rate_z_asl)", "t value"],
                    df = coef(summary(fit))["scale(rate_z_asl)", "df"],
                    lower.tail = (alt == "less"))

  # Same model for cortical thickness asymmetry
  fit_thx <- lmer(formula(str_c("scale(", out_name, ") ~ scale(rate_z_thx) + (1 | id)")),
                  data = rugby)
  beta_thx <- summary(fit_thx)$coefficients[2, 1]
  p_one_sided_thx <- pt(coef(summary(fit_thx))["scale(rate_z_thx)", "t value"],
                        df = coef(summary(fit_thx))["scale(rate_z_thx)", "df"],
                        lower.tail = (alt == "less"))

  cors_new <- bind_rows(cors_new,
    tibble(out = out_name, alt = alt,
           beta = beta, p_lmer = p_one_sided,
           beta_thx = beta_thx, p_lmer_thx = p_one_sided_thx)
  )
}

# FDR correction of the ASL associations, separately for
# HIA3 subtests and extended neuropsychological tests
cors_new <- cors_new %>%
  mutate(hia3 = str_detect(out, "^hia3")) %>%
  group_by(hia3) %>%
  mutate(pfdr_asl = p.adjust(p_lmer, method = "fdr")) %>%
  ungroup()

cors_new

## ------------------------------------------------------------------
## 8. Memory recall association and power (Figure 4)
## ------------------------------------------------------------------
# Main reported association: HIA3 memory recall score
fit <- lmer(hia3_memory_recall_score ~ rate_z_asl + (1 | id), data = rugby)
summary(fit)
p_one_sided <- pt(coef(summary(fit))["rate_z_asl", "t value"],
                  df = coef(summary(fit))["rate_z_asl", "df"],
                  lower.tail = TRUE)   # hypothesised negative direction

# Simulation-based power at the observed standardised effect
fit_assoc <- lmer(scale(hia3_memory_recall_score) ~ scale(rate_z_asl) + (1 | id),
                  data = rugby)
tau2_est   <- as.data.frame(VarCorr(fit_assoc))$vcov[1]  # random intercept variance
sigma2_est <- sigma(fit_assoc)^2                         # residual variance

sim_power_assoc(effect = -0.461, tau2 = tau2_est, sigma2 = sigma2_est,
                n_sim = 1000)

# Figure 4: scatter plot with fitted line and 95% CI from the LMM
pred <- ggpredict(fit, terms = "rate_z_asl [all]")

rugby %>%
  select(id, subject, mri_date, mri_dispo, rate_z_asl, hia3_memory_recall_score) %>%
  ggplot(aes(rate_z_asl, hia3_memory_recall_score)) +
  geom_jitter() +
  geom_ribbon(data = pred, aes(x = x, ymin = conf.low, ymax = conf.high),
              inherit.aes = FALSE, fill = "darkred", alpha = 0.15) +
  geom_line(data = pred, aes(x = x, y = predicted),
            inherit.aes = FALSE, color = "darkred", linewidth = 1.3) +
  theme_classic() +
  xlab("Rate of abnormal vertices with absolute z-score > 2.5") +
  ylab("HIA3 memory recall score")
ggsave("Rugby_memory_recall_ASL.pdf", width = 4, height = 4)

# Available data for the memory recall score
table(is.na(rugby$hia3_memory_recall_score))
