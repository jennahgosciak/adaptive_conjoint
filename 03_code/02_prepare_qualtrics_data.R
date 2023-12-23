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
survey_lab <- "political_candidates"
df_clean <- readRDS(str_glue("00_data/qualtrics_data_{survey_lab}.RDS"))

cat("\nNumber of rows in data\n")
nrow(df_clean)

# check attention, percent who pass the attention check
cat("\nAttention check results\n")
df_clean %>%
  mutate(older_candidate = if_else(rnum_age <= 0.5, "Candidate 2", "Candidate 1")) %>%
  mutate(pass_attention_check = if_else(Manipulation_Q1 == older_candidate, 1, 0)) %>%
  summarize(per_pass_attention_check = mean(pass_attention_check))

###############################################
# Process Qualtrics data
###############################################

# create outcome variable
df_clean <- create_outcome_var(df_clean)

# create profile context variable
df_clean <- create_context_var(df_clean)

# validation of outcome variable
# each question number is the random ordering of the context attributes
cat("\nValidation of outcome data\n")
df_clean %>%
  select(chose_younger, str_c("Q", 1:8)) %>%
  verify(!is.na(chose_younger))

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
    drop_demo_flag = if_else(race == "Prefer not to disclose" |
      QD4 == "Prefer not to disclose" |
      QD5 == "Prefer not to disclose" |
      QD2 == "Prefer not to disclose", TRUE, FALSE)
  ) %>%
  verify(!is.na(race))

# filter to only the variables we need
df_demo <- df_demo %>%
  select(
    id, chose_younger, race, female, age, hispanic, drop_demo_flag,
    context, context_label
  )

# saving locally
df_demo %>%
  saveRDS(str_glue("01_intermediate/qualtrics_data_{survey_lab}_clean.RDS"))

df_demo %>%
  write_csv(str_glue("01_intermediate/qualtrics_data_{survey_lab}_clean.csv"), na = "")

sink()
