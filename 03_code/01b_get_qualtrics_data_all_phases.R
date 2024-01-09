# R script to load survey data from Qualtrics via the API
# and save a cleaned, de-identified data file locally/on GitHub

library(tidyverse)
library(qualtRics)
library(magrittr)
library(assertr)

set.seed(2023)
config <- config::get()

file <- file("./_logs/01b_get_qualtrics_data_all_phases.txt", open = "wt")
sink(file, type = "output")
sink(file, type = "message")

source("./03_code/_data_cleaning.R")

###############################################
# Load Qualtrics
###############################################
url <- str_glue("https://{config$datacenter_id}.qualtrics.com")
survey_name <- "Political Candidates"
survey_lab <- "political_candidates"
survey_id <- config$pol_candidates_survey_id

df_survey <- load_qualtrics(survey_name)

cat("\nNumber of observations in data\n")
nrow(df_survey)

# dropping test cases
df_survey <- df_survey %>%
  mutate(StartDate_clean = ymd_hms(StartDate)) %>%
  verify(is.na(StartDate_clean) == is.na(StartDate)) %>%
  # drop test data
  filter(StartDate_clean >= ymd_hms("2023-12-26-17-56-00")) %>%
  # drop data collected incorrectly
  filter(StartDate_clean <= ymd_hms("2023-12-27-16-54-00") | StartDate_clean >= ymd_hms("2024-01-03-00-00-00")) %>%
  mutate(
    batch_id = case_when(
      StartDate_clean <= ymd_hms("2023-12-26-18-48-00") ~ 0,
      StartDate_clean <= ymd_hms("2023-12-27-13-59-00") ~ 1,
      StartDate_clean <= ymd_hms("2023-12-27-14-47-00") ~ 2,
      StartDate_clean <= ymd_hms("2023-12-27-15-33-00") ~ 3,
      StartDate_clean <= ymd_hms("2023-12-27-16-54-00") ~ 4,
      StartDate_clean <= ymd_hms("2024-01-03-12-43-00") ~ 1,
      StartDate_clean <= ymd_hms("2024-01-03-16-32-00") ~ 2,
      StartDate_clean <= ymd_hms("2024-01-03-17-02-00") ~ 3,
      StartDate_clean <= ymd_hms("2024-01-03-17-21-00") ~ 4,
      StartDate_clean <= ymd_hms("2024-01-03-18-05-00") ~ 5,
      StartDate_clean <= ymd_hms("2024-01-04-10-05-00") ~ 6,
      StartDate_clean <= ymd_hms("2024-01-04-10-55-00") ~ 7,
      StartDate_clean <= ymd_hms("2024-01-04-11-58-00") ~ 8,
      StartDate_clean <= ymd_hms("2024-01-04-12-33-00") ~ 9,
      StartDate_clean <= ymd_hms("2024-01-04-15-30-00") ~ 10,
      StartDate_clean <= ymd_hms("2024-01-04-15-55-00") ~ 11,
      StartDate_clean <= ymd_hms("2024-01-04-16-55-00") ~ 12,
      StartDate_clean <= ymd_hms("2024-01-04-23-55-00") ~ 13,
      StartDate_clean <= ymd_hms("2024-01-05-10-52-00") ~ 14,
      StartDate_clean <= ymd_hms("2024-01-05-11-50-00") ~ 15,
      StartDate_clean <= ymd_hms("2024-01-05-13-33-00") ~ 16,
      StartDate_clean <= ymd_hms("2024-01-05-14-10-00") ~ 17,
      StartDate_clean <= ymd_hms("2024-01-05-14-25-00") ~ 18,
      StartDate_clean <= ymd_hms("2024-01-05-14-40-00") ~ 19,
      StartDate_clean <= ymd_hms("2024-01-05-14-53-00") ~ 20,
      StartDate_clean <= ymd_hms("2024-01-05-15-05-00") ~ 21,
      StartDate_clean > ymd_hms("2024-01-08-20-00-00") ~ 100,
      TRUE ~ NA_integer_
    ),
    batch_type = case_when(
      batch_id == 0 ~ "Warmup",
      StartDate_clean <= ymd_hms("2023-12-27-16-54-00") ~ "Iterative Batch Phase: Max",
      StartDate_clean > ymd_hms("2023-12-27-16-54-00") & StartDate_clean <= ymd_hms("2024-01-08-20-00-00")~ "Iterative Batch Phase: Min",
      StartDate_clean > ymd_hms("2023-12-27-16-54-00") ~ "Validation",
      TRUE ~ NA_character_
    )
  ) %>% 
  verify(!is.na(batch_id)) %>% 
  verify(!is.na(batch_type)) %>% 
  # drop the 21st batch
  filter(batch_id != 21)

cat("\nNumber of observations after dropping test cases\n")
nrow(df_survey)

df_survey %>%
  group_by(batch_id, batch_type) %>%
  summarize(n = n()) %>% 
  print(n=50)

###############################################
# Survey Validation
###############################################

# check ID uniquely identifies rows in data
if (length(unique(df$`Prolific ID Q`)) != nrow(df)) {
  warning("ID is not unique")
}

if (!all(unique(df$Status) == "IP Address")) {
  warning("Test/spam data included in the analysis file")
  print(str_glue("Number of observations in data: {nrow(df)}"))
  print(str_glue("Status values in data: {str_c(unique(df$Status), collapse=', ')}"))
  df <- df %>% 
    filter(Status == "IP Address")
  print(str_glue("Number of observations left in data: {nrow(df)}"))
}

# check we have consent from all participants
# if not, make sure we have not collected data
# drop participants who have not consented
df_survey <- df_survey %>%
  check_consent()

# check that all completed
df_survey <- df_survey %>%
  check_completion()

# check all are in the US
df_survey <- df_survey %>%
  check_location_screen()

# check commitment
df_survey <- df_survey %>%
  check_commitment()

###############################################
# Generate new ID
###############################################
# create cleaned version for saving locally
df_clean <- df_survey %>%
  select(batch_id, batch_type, PreScreen_Q1:rnum_age, age1:career2) %>%
  # create a new unique random ID for linking
  mutate(id = runif(nrow(df_survey), 0, 1)) %>%
  arrange(id) %>%
  mutate(id = row_number()) %>%
  select(id, everything())

df_clean %>%
  group_by(batch_id, batch_type) %>%
  summarize(n = n()) %>% 
  print(n=50)

# saving locally
df_clean %>%
  saveRDS(str_glue("00_data/qualtrics_data_{survey_lab}_all_phases.RDS"))

df_clean %>%
  write_csv(str_glue("00_data/qualtrics_data_{survey_lab}_all_phases.csv"), na = "")

sink()
