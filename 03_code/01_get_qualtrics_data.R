# R script to load survey data from Qualtrics via the API
# and save a cleaned, de-identified data file locally/on GitHub

library(tidyverse)
library(qualtRics)
library(magrittr)
library(assertr)

set.seed(2023)
config <- config::get()

source("./03_code/_data_cleaning.R")

# load survey data
url <- str_glue("https://{config$datacenter_id}.qualtrics.com")
survey_name <- "Political Candidates"
survey_lab <- "political_candidates"
survey_id <- config$pol_candidates_survey_id


df_survey <- load_qualtrics(survey_name)

nrow(df_survey)

df_survey %>%
  head()

# drop test cases
df_survey <- df_survey %>%
  mutate(StartDate_clean = ymd_hms(StartDate)) %>%
  verify(is.na(StartDate_clean) == is.na(StartDate)) %>%
  filter(StartDate_clean >= ymd_hms("2024-12-14-00-00-00"))

nrow(df_survey)

# create cleaned version for saving locally
df_clean <- df_survey %>% 
  select(PreScreen_Q1:rnum_age, age1:career2) %>% 
  # create a new unique random ID for linking
  mutate(id = runif(1,0,1)) %>% 
  arrange(id) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

df_clean %>% 
  saveRDS(str_glue('00_data/qualtrics_data_{survey_lab}.RDS'))

df_clean %>% 
  write_csv(str_glue('00_data/qualtrics_data_{survey_lab}.csv'), na = "")