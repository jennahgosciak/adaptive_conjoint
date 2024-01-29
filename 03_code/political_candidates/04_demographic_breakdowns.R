# R script to run poststratification analysis
library(tidyverse)
library(qualtRics)
library(magrittr)
library(assertr)
library(modelr)

set.seed(2023)

source("./03_code/_data_cleaning.R")
file <- file("./_logs/04_demographic_breakdowns.txt", open = "wt")
sink(file, type = "output")
sink(file, type = "message")

###############################################
# Load Data
###############################################
survey_lab <- "political_candidates"
df_analysis <- readRDS(str_glue("01_intermediate/qualtrics_data_{survey_lab}_clean.RDS"))

# female and hispanicity
df_analysis %>% 
  summarize(across(c("female", "hispanic"), .fns = lst(n = ~sum(.),
                                                       per = ~mean(.))))

# age
df_analysis %>% 
  group_by(age) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(per = n/sum(n))

df_analysis %>% 
  ggplot(aes(age)) +
  geom_histogram()

# race
df_analysis %>%
  group_by(race) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(per = n/sum(n)) %>% 
  arrange(desc(per))

df_analysis %>%
  group_by(race) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(per = n/sum(n)) %>% 
  arrange(desc(per)) %>% 
  ggplot(aes(reorder(race, desc(per)), per)) +
  geom_col()
sink()
