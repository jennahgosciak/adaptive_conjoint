# R script to run poststratification analysis
library(tidyverse)
library(qualtRics)
library(magrittr)
library(assertr)
library(modelr)

set.seed(2023)
config <- config::get()

source("./03_code/_data_cleaning.R")
file <- file('./_logs/03_analysis.txt', open = "wt")
sink(file ,type = "output")
sink(file, type = "message")

###############################################
# Load Data
###############################################
survey_lab <- "political_candidates"
df_analysis <- readRDS(str_glue("01_intermediate/qualtrics_data_{survey_lab}_clean.RDS"))

# identify the distinct contexts in the data
distinct_contexts <- df_analysis %>%
  distinct(context, context_label) %>%
  arrange(context)
c_val <- pull(distinct_contexts, context)
c_desc <- pull(distinct_contexts, context_label)

cat(str_c("Distinct contexts for the validation phase: ", 
            str_c(c_val, collapse = ", ")))

# load poststratification weights from ipums acs survey
wgts <- readRDS("00_data/ipums_strata_sizes.RDS")

cat('\nPopulation weights\n')
wgts %>%
  head()

###############################################
# (1) Simple Mean
###############################################
df_simple_mean <- df_analysis %>%
  group_by(context, context_label) %>%
  summarize(
    mean = mean(chose_younger),
    se = sqrt((mean * (1 - mean)) / length(chose_younger))
  ) %>%
  mutate(
    Mean = as.character(round(mean, 2)),
    `Confidence Interval (95%)` = str_glue("({round(mean - (qnorm(.975) * se), 2)}, {round(mean + (qnorm(.975) * se), 2)})")
  ) %>%
  select(context, context_label, Mean, `Confidence Interval (95%)`) %>%
  pivot_longer(-c(context, context_label),
    names_to = "type", values_to = "Simple Mean"
  )

cat('\nSimple mean estimates\n')
df_simple_mean

###############################################
# (2) Poststratified Estimate
###############################################

# first, drop people who choose not to disclose for any of the demographic features
df_filter <- df_analysis %>%
  # exclude respondents who listed 'Prefer not to disclose' for any demographic variables
  filter(drop_demo_flag == FALSE)

# exclude race categories that aren't in the data we have
wgts <- wgts %>%
  filter(race %in% unique(df_filter$race))

# running logistic regression for predicting probability of choosing younger candidate
glm_drop_cons_factors <- function(df, vars) {
  # need to drop factors that don't vary (e.g., only one racial category appears)
  if (length(unique(df$race)) <= 1) vars <- vars[vars != "race"]
  form <- str_c(vars, collapse = " + ")

  glm(
    formula = str_c("chose_younger ~ ", form),
    family = "binomial",
    data = df
  )
}

df_models <- df_filter %>%
  arrange(context) %>%
  group_by(context, context_label) %>%
  nest() %>%
  mutate(glm = map(
    data,
    ~ glm_drop_cons_factors(
      .x,
      c("female", "hispanic", "age", "race")
    )
  ))

# predict based on population weight categories
w_mean <- map2(df_models$data, df_models$glm, ~ compute_weighted_prob(.x, wgts, .y)) %>%
  unlist()

# compute weighted mean
df_post <- tibble(
  context = c_val,
  context_label = c_desc,
  `Poststratified Estimate (probabilty chose younger)` = w_mean
)
cat('\nSingle poststratification estimate\n')
df_post

###############################################
# CI with Bootstrapping
###############################################
compute_weighted_prob <- function(df, wgts, lm) {
  # exclude race categories that aren't in the data we have
  wgts <- wgts %>%
    filter(race %in% unique(as_tibble(df)[["race"]]))

  # predict probabilities
  prob <- predict(lm, newdata = wgts, type = "response")
  # return weighted mean
  return(weighted.mean(prob, w = wgts$weight))
}

produce_bootstrap_estimates <- function(df, context, iter = 1000) {
  df %>%
    filter(context == context) %>%
    bootstrap(iter) %>%
    mutate(glm = map(
      strap,
      ~ glm_drop_cons_factors(
        .x,
        c("female", "hispanic", "age", "race")
      )
    ))

  # produce bootstrap estimate
  bootstrap_est <- map2(
    df_bootstrap$strap, df_bootstrap$glm,
    ~ compute_weighted_prob(.x, wgts, .y)
  ) %>%
    unlist()

  # produce mean and sd
  tibble(
    mean = mean(bootstrap_est),
    se = sd(bootstrap_est),
    context = context,
  ) %>%
    mutate(
      Mean = as.character(round(mean, 2)),
      `Confidence Interval (95%)` = str_glue("({round(mean - (qnorm(.975) * se), 2)}, {round(mean + (qnorm(.975) * se), 2)})")
    ) %>%
    select(context, Mean, `Confidence Interval (95%)`) %>%
    pivot_longer(-context,
      names_to = "type",
      values_to = "PostStratification Estimate (Bootstrapped)"
    )
}

cat('\nBootstrap estimates\n')
df_post_bootstrap <- map_dfr(c_val, ~ produce_bootstrap_estimates(df_filter, .))
df_post_bootstrap

# present both simple mean and poststratification results
df_final <- left_join(df_simple_mean, df_post_bootstrap, by = c("context", "type"))

cat('\nResults: simple mean and poststratification estimates\n')
df_final

df_final %>% 
  write_csv('02_output/validation_results.csv')

sink()