# R script to load survey data from Qualtrics via the API
# and save a cleaned, de-identified data file locally/on GitHub

library(tidyverse)
library(qualtRics)
library(magrittr)
library(assertr)

set.seed(2023)
config <- config::get()

file <- file("./_logs/01_get_qualtrics_data.txt", open = "wt")
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
  filter(StartDate_clean >= ymd_hms("2024-01-08-20-00-00"))

cat("\nNumber of observations after dropping test cases\n")
nrow(df_survey)
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
  select(PreScreen_Q1:rnum_age, age1:career2) %>%
  # create a new unique random ID for linking
  mutate(id = runif(nrow(df_survey), 0, 1)) %>%
  arrange(id) %>%
  mutate(id = row_number()) %>%
  select(id, everything())

# saving locally
df_clean %>%
  saveRDS(str_glue("00_data/qualtrics_data_{survey_lab}.RDS"))

df_clean %>%
  write_csv(str_glue("00_data/qualtrics_data_{survey_lab}.csv"), na = "")

sink()
