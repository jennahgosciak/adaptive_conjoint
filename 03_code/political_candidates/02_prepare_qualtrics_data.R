# R script to clean and process survey data from Qualtrics saved locally
library(tidyverse)
library(qualtRics)
library(magrittr)
library(assertr)

set.seed(2023)

source("./03_code/_data_cleaning.R")

file <- file("./_logs/02_prepare_qualtrics_data.txt", open = "wt")
sink(file, type = "output")
sink(file, type = "message")

###############################################
# Load Qualtrics data
###############################################
survey_lab <- "job_applicants"
add_all_phases <- FALSE
if (add_all_phases == TRUE) {
  fname <- "00_data/qualtrics_data_{survey_lab}_all_phases.RDS"
} else {
  fname <- "00_data/qualtrics_data_{survey_lab}.RDS"
}
df_clean <- readRDS(str_glue(fname))

cat("\nNumber of rows in data\n")
nrow(df_clean)

# check attention, percent who pass the attention check
cat("\nAttention check results\n")
if (survey_lab == "political_candidates") {
  df_clean %>%
    mutate(older_candidate = if_else(rnum_age <= 0.5, "Candidate 2", "Candidate 1")) %>%
    mutate(pass_attention_check = if_else(Manipulation_Q1 == older_candidate, 1, 0)) %>%
    summarize(per_pass_attention_check = mean(pass_attention_check))
} else {
  df_clean %>%
    mutate(candidate_mother = if_else(rnum_mother <= 0.5, "Candidate 2", "Candidate 1"),
           manipulation_check_total = rowSums(select(., starts_with("Manipulation_Q1_")) %>% 
                                                is.na())) %>%
    mutate(pass_attention_check = case_when((rnum_mother <= 0.5) & 
                                        (Manipulation_Q1_2 == "Candidate 2") &
                                        (manipulation_check_total == 2) ~ 1,
                                      (rnum_mother > 0.5) & 
                                        (Manipulation_Q1_2 == "Candidate 1") &
                                        (manipulation_check_total == 2) ~ 1,
                                      TRUE ~ 0)) %>%
    summarize(per_pass_attention_check = mean(pass_attention_check))
}

###############################################
# Process Qualtrics data
###############################################

# create outcome variable
df_clean <- create_outcome_var(df_clean, survey_lab)

# create profile context variable
df_clean <- create_context_var(df_clean, survey_lab)

df_clean %>% 
  group_by(context) %>% 
  summarize(n = n())

# validation of outcome variable
# each question number is the random ordering of the context attributes
if (survey_lab=="political_candidates") {
  outcome_var <- chose_younger
} else if (survey_lab=="job_applicants") {
  outcome_var <- "chose_mother"
}

cat("\nValidation of outcome data\n")
df_clean %>%
  select(outcome = outcome_var, all_of(str_c("Q", 1:8))) %>%
  verify(!is.na(outcome))

df_clean %>% 
  rename(outcome = outcome_var) %>% 
  mutate(neg_outcome = 1 - outcome) %>% 
  group_by(context, context_label) %>% 
  summarize(mean_chose_younger = mean(outcome),
            mean_chose_older = mean(neg_outcome))

 # create cleaned demographic variables
df_demo <- df_clean %>%
  mutate(
    hispanic = if_else(QD4 == "Yes", TRUE, FALSE),
    female = if_else(QD5 == "Female", TRUE, FALSE)
  ) %>%
  mutate(age = QD2_1_TEXT) %>%
  verify(is.numeric(age)) %>%
  mutate(across(starts_with("QD3_"), .fns = lst(race_num = ~ if_else(!is.na(.), 1, 0)))) %>%
  mutate(
    race_count = rowSums(select(., ends_with("race_num"))),
    race = case_when(
      race_count > 1 ~ "Two or More Races",
      !is.na(QD3_1) ~ "American Indian or Alaska Native",
      !is.na(QD3_2) ~ "Asian",
      !is.na(QD3_3) ~ "Black or African American",
      !is.na(QD3_4) ~ "Native Hawaiian or Other Pacific Islander",
      !is.na(QD3_5) ~ "White",
      !is.na(QD3_6) ~ "Other",
      !is.na(QD3_7) ~ "Prefer not to disclose"
    ),
    race = factor(race, levels = c(
      "Black or African American",
      "White", "American Indian or Alaska Native",
      "Asian", "Native Hawaiian or Other Pacific Islander",
      "Other", "Two or More Races", "Prefer not to disclose"
    )),
    drop_demo_flag = if_else(!is.na(QD3_7) |
      QD4 == "Prefer not to disclose" |
      QD5 == "Prefer not to disclose" |
      QD2 == "Prefer not to disclose", TRUE, FALSE)
  ) %>%
  verify(!is.na(race))

# filter to only the variables we need
if (add_all_phases == TRUE) {
  df_demo <- df_demo %>%
    select(
      batch_id, batch_type, id, outcome_var, race, female, age, hispanic, drop_demo_flag,
      context, context_label
    )
  output_fname <- str_glue("01_intermediate/qualtrics_data_{survey_lab}_clean_all_phases")
  
  df_demo %>%
    group_by(batch_id, batch_type) %>%
    summarize(n = n()) %>% 
    print(n=50)
} else {
  df_demo <- df_demo %>%
    select(
      id, outcome_var, race, female, age, hispanic, drop_demo_flag,
      context, context_label
    )
  output_fname <- str_glue("01_intermediate/qualtrics_data_{survey_lab}_clean")
}

# saving locally
df_demo %>%
  saveRDS(str_glue("{output_fname}.RDS"))

df_demo %>%
  write_csv(str_glue("{output_fname}.csv"), na = "")

sink()
