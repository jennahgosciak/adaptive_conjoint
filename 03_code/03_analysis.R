# R script to run poststratification analysis
library(tidyverse)
library(qualtRics)
library(magrittr)
library(assertr)
library(logger)

set.seed(2023)
config <- config::get()

source("./03_code/_data_cleaning.R")

###############################################
# Simple Mean and Poststratification Estimate
###############################################
survey_lab <- "political_candidates"
df_analysis <- readRDS(str_glue('01_intermediate/qualtrics_data_{survey_lab}_clean.RDS'))

# load poststratification weights from ipums acs survey
wgts <- readRDS("00_data/ipums_strata_sizes.RDS")
wgts %>% 
  head()

# (1) simple mean
df_simple_mean <- df_analysis %>% 
  summarize(Mean = mean(chose_younger),
            `Standard Error` = sqrt((Mean * (1 - Mean))/length(chose_younger))) %>% 
  pivot_longer(everything(), names_to = "type", values_to = "Simple Mean")

df_simple_mean %>% 
  View()

# (2) poststratified estimate

# first, drop people who choose not to disclose for any of the demographic features
df_filter <- df_analysis %>% 
  # exclude respondents who listed 'Prefer not to disclose' for any demographic variables
  filter(drop_demo_flag == FALSE)

# exclude race categories that aren't in the data we have
wgts <- wgts %>% 
  filter(race %in% unique(df_filter$race))

# running logistic regression for predicting probability of choosing younger candidate
lm <- glm(formula = chose_younger ~ female + hispanic + age + race, family = "binomial",
          data = df_filter)

summary(lm)

# predict based on population weight categories
df_prob <- cbind(wgts, prob_chose_younger = predict(lm, newdata=wgts, type='response'))

# compute weighted mean
df_post <- df_prob %>% 
  summarize(`Poststratified Estimate (probabilty chose younger)` = weighted.mean(prob_chose_younger, weight))

###############################################
# Confidence Intervals
###############################################