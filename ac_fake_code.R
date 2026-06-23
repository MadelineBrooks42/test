#######################################
## SER 2026 Git workshop fake code
## Anlan Cao
## 06/22/2026
#######################################

rm(list = ls())

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(stringr)

BSA <- read.csv(BSA_PATH)
SMM <- read.csv(SMM_PATH)

final <- read.csv(DATA_PATH)

baseline <- final %>%
  group_by(part_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    part_id,
    female = as.numeric(as.character(female)),
    sex = ifelse(female == 1, "Female", "Male"),
    sex = factor(sex, levels = c("Male", "Female")),
    Age_rand,
    Age_55,
    race2,
    bmi_BL,
    bsa_BL,
    SMM,
    SMI,
    SMI_z,
    stage2,
    intervention,
    site
  )

sim_vars <- c(
  ## Completion / treatment process
  "cycle_miss_sim_mean",
  "cycle_miss_sim_med",
  "p_complete_sim",
  "p_anymiss_sim",
  "p_early_stop_sim",
  "p_any_delay_sim",
  "total_delay_days_sim_mean",
  
  ## Treatment-process clinical endpoints
  "ox_drop_ever_prob_sim",
  "ox_drop_cycle_mean_sim",
  "ox_drop_cycle_med_sim",
  "fiveFU_drop_ever_prob_sim",
  "fiveFU_drop_cycle_mean_sim",
  "fiveFU_drop_cycle_med_sim",
  "fp_backbone_complete_prob_sim",
  
  ## NCCN-relative RDI
  "rdi_regimen_sim",
  "rdi_5FU_sim",
  "rdi_ox_sim",
  
  ## Cycle-1-relative RDI
  "rdi_regimen_cycle1_sim",
  "rdi_5FU_cycle1_sim",
  "rdi_ox_cycle1_sim",
  
  ## PRO missingness
  "tox_missing_mean_sim",
  "any_PRO_missing_prob_sim",
  
  ## PRO summary 1: missing-as-no toxicity
  "G34_8_cat_mean_sim",
  "cipn_max_prob_sim",
  "cipn_by6_prob_sim",
  
  ## PRO summary 2: observed among nonmissing
  "G34_8_cat_obs_mean_sim",
  "cipn_obs_max_prob_sim",
  "cipn_obs_by6_prob_sim",
  
  ## Lab toxicities
  "neutropenia_max_prob_sim",
  "anemia_max_prob_sim"
)

## Observed variables retained in each simulation output.
obs_vars <- c(
  "cycle_miss",
  "complete_obs",
  "anymiss_obs",
  "early_stop_obs",
  "any_delay_obs",
  "total_delay_days",
  
  "ox_drop_ever",
  "ox_drop_cycle",
  "fiveFU_drop_ever",
  "fiveFU_drop_cycle",
  "fp_backbone_complete",
  
  "rdi_regimen",
  "rdi_5FU",
  "rdi_ox",
  
  "tox_missing_mean",
  "any_PRO_missing",
  "G34_8_cat_mean",
  "G34_8_cat_obs_mean",
  "cipn_max",
  "cipn_by6",
  "cipn_obs_max",
  "cipn_obs_by6",
  
  "neutropenia_max",
  "anemia_max"
)

rename_policy_cols <- function(df, suffix) {
  df %>%
    rename_with(
      ~ {
        x <- .
        x <- sub("_prob_sim$", "_prob", x)
        x <- sub("_sim_mean$", "_mean", x)
        x <- sub("_sim_med$", "_med", x)
        x <- sub("_sim$", "", x)
        paste0(x, "_", suffix)
      },
      all_of(sim_vars)
    )
}

BSA_use <- BSA %>%
  rename_policy_cols("bsa")

SMM_use <- SMM %>%
  rename_policy_cols("smm") %>%
  dplyr::select(
    part_id,
    ends_with("_smm")
  )

results <- BSA_use %>%
  left_join(SMM_use, by = "part_id") %>%
  left_join(baseline, by = "part_id") %>%
  relocate(part_id, sex, female, Age_rand, bmi_BL, bsa_BL, SMM, SMI)

results <- results %>%
  mutate(
    complete_obs = ifelse(is.na(complete_obs), as.integer(cycle_miss == 0), complete_obs),
    anymiss_obs = ifelse(is.na(anymiss_obs), as.integer(cycle_miss > 0), anymiss_obs)
  )

write.csv(
  results,
  file.path(OUT_DIR, "analysis_dataset_BSA_SMM_0622.csv"),
  row.names = FALSE
)
