# R script to run poststratification analysis
library(assertr)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(here)
library(modelr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

set.seed(2023)

###############################################
# Load Data
###############################################
# survey_lab <- "political_candidates"
df_analysis <- read_csv(here("data", "pol_cand_clean_all_phases.csv"))

# # identify the distinct contexts in the data
# distinct_contexts <- df_analysis |>
#   distinct(context, context_label) |>
#   arrange(context)
# c_val <- pull(distinct_contexts, context)
# c_desc <- pull(distinct_contexts, context_label)

# cat(str_c(
#   "Distinct contexts for the validation phase: ",
#   str_c(c_val, collapse = ", ")
# ))

# # load poststratification weights from ipums acs survey
# wgts <- readRDS(here("data/ipums_strata_sizes.RDS")) |>
#   mutate(
#     race = as.character(race),
#     race = case_when(
#       race == "Native Hawaiian or Other Pacific Islander" ~
#         "Native Hawaiian or Pacific Islander",
#       TRUE ~ race
#     ),
#     race = factor(race, levels = levels(responses$race_cleaned))
#   )

# cat("\nPopulation weights\n")
# wgts |>
#   head()

# cat("\nNumber of rows in data")
# nrow(df_analysis)

###############################################
# (1) Simple Mean
###############################################
df_simple_mean <- df_analysis |>
  filter(batch_type == "Validation") |>
  group_by(context, context_label) |>
  summarize(
    estimate = mean(chose_younger),
    se = sqrt((estimate * (1 - estimate)) / length(chose_younger)),
    ci_min = estimate - (qnorm(.975) * se),
    ci_max = estimate + (qnorm(.975) * se)
  ) |>
  mutate(method = "simple_mean") |>
  select(context, context_label, method, estimate, se, ci_min, ci_max)

cat("\nSimple mean estimates\n")
df_simple_mean

# Adaptive phase estimates
adaptive_estimates <- df_analysis |>
  filter(!batch_type == "Validation") |>
  group_by(context, context_label) |>
  summarize(alpha = sum(chose_younger) + 10,
            beta = sum(1 - chose_younger) + 10,
            n = n()) |>
  mutate(mu = alpha / (alpha + beta),
         lb = qbeta(.025, shape1 = alpha, shape2 = beta),
         ub = qbeta(.975, shape1 = alpha, shape2 = beta)) |>
  separate(context_label, into = c("race", "sex", "experience"))

estimated_discrim_all_pol_cand <- ggplot(
    adaptive_estimates,
    aes(x = fct_reorder(factor(context), desc(mu)), y = mu, ymin = lb, ymax = ub)
  ) +
  geom_point() +
  geom_errorbar(width = 0.1) +
  theme_minimal() +
  labs(x = "Context ID", y = "Posterior expected probability")

ggsave(
  plot = estimated_discrim_all_pol_cand,
  filename = here("figures", "estimated_discrimination_all_pol_cand.png"),
  width = 5,
  height = 3,
  dpi = 500
)

# # validation phase, estimate
# estimates_validation_data <- df_analysis |>
#   group_by(context, context_label) |>
#   summarize(alpha = sum(chose_younger) + 10,
#             beta = sum(1 - chose_younger) + 10,
#             mean = mean(chose_younger),
#             n = n()) |>
#   mutate(estimate = alpha / (alpha + beta),
#          ci.min = qbeta(.025, shape1 = alpha, shape2 = beta),
#          ci.max = qbeta(.975, shape1 = alpha, shape2 = beta)) |>
#   separate(context_label, into = c("race", "sex", "experience"))

# print(estimates_validation_data)

# estimates_validation_data |>
#   ggplot(aes(x = experience, y = estimate,
#              label = format(round(estimate,2), nsmall = 2),
#              ymin = ci.min, ymax = ci.max)) +
#   geom_errorbar(width = .5) +
#   geom_label() +
#   facet_grid(race ~ sex) +
#   xlab("Political Experience") +
#   ylab("P(Chooses Younger)")

# ###############################################
# # (2) Poststratified Point Estimate
# ###############################################

# # first, drop people who choose not to disclose for any of the demographic features
# df_filter <- df_analysis |>
#   # exclude respondents who listed 'Prefer not to disclose' for any demographic variables
#   filter(drop_demo_flag == FALSE)

# # exclude race categories that aren't in the data we have
# wgts <- wgts |>
#   filter(race %in% unique(df_filter$race))

# # running logistic regression for predicting probability of choosing younger candidate
# glm_drop_cons_factors <- function(df, vars) {
#   # need to drop factors that don't vary (e.g., only one racial category appears)
#   if (length(unique(df$race)) <= 1) vars <- vars[vars != "race"]
#   form <- str_c(vars, collapse = " + ")

#   glm(
#     formula = str_c("chose_younger ~ ", form),
#     family = "binomial",
#     data = df
#   )
# }

# # compute weighted probability of choosing the younger candidate
# # exclude race categories that aren't in the data when doing the prediction
# compute_weighted_prob <- function(df, wgts, lm) {
#   # exclude race categories that aren't in the data we have
#   wgts <- wgts |>
#     filter(race %in% unique(as_tibble(df)[["race"]]))

#   # predict probabilities
#   prob <- predict(lm, newdata = wgts, type = "response")
#   # return weighted mean
#   return(weighted.mean(prob, w = wgts$weight))
# }

# df_models <- df_filter |>
#   arrange(context) |>
#   group_by(context, context_label) |>
#   nest() |>
#   mutate(glm = map(
#     data,
#     ~ glm_drop_cons_factors(
#       .x,
#       c("female", "hispanic", "age", "race")
#     )
#   ))

# # predict based on population weight categories
# # and then compute the weighted mean
# w_mean <- map2(df_models$data, df_models$glm, ~ compute_weighted_prob(.x, wgts, .y)) |>
#   unlist()

# df_post <- tibble(
#   context = df_models$context,
#   context_label = df_models$context_label,
#   estimate = w_mean
# )

# ###############################################
# # (3) CI with Bootstrapping
# ###############################################

# produce_bootstrap_estimates <- function(df, c_val, c_desc, iter = 1000) {
#   df_bootstrap <- df |>
#     filter(context == c_val) |>
#     bootstrap(iter) |>
#     mutate(glm = map(
#       strap,
#       ~ glm_drop_cons_factors(
#         .x,
#         c("female", "hispanic", "age", "race")
#       )
#     ))

#   # produce bootstrap estimate
#   bootstrap_est <- map2(
#     df_bootstrap$strap, df_bootstrap$glm,
#     ~ compute_weighted_prob(.x, wgts, .y)
#   ) |>
#     unlist()

#   # produce mean and sd
#   tibble(
#     se = sd(bootstrap_est),
#     context = c_val,
#     context_label = c_desc
#   ) |>
#     select(context, context_label, se)
# }

# cat("\nBootstrap estimates\n")
# df_post_bootstrap <- map2_dfr(c_val, c_desc, ~ produce_bootstrap_estimates(df_filter, .x, .y)) |>
#   full_join(df_post, ., by = c("context", "context_label")) |>
#   # adding bootstrap standard error to calculation of ci with point estimate of the mean
#   mutate(
#     method = "poststratified",
#     ci_min = estimate - (qnorm(.975) * se),
#     ci_max = estimate + (qnorm(.975) * se)
#   ) |>
#   select(context, context_label, method, estimate, se, ci_min, ci_max)

# df_post_bootstrap

# # present both simple mean and poststratification results
# df_final <- bind_rows(df_simple_mean, df_post_bootstrap) |>
#   filter(context %in% c(5, 6))

# cat("\nResults: simple mean and poststratification estimates\n")
# df_final

# Job Applicants example ------------------------------------------------------

###############################################
# Load Data
###############################################

df_analysis <- read_csv(here("data", "job_app_clean_all_phases.csv"))

# Adaptive phase estimates
adaptive_estimates <- df_analysis |>
  group_by(context, context_label) |>
  summarize(alpha = sum(1 - chose_mother) + 10,
            beta = sum(chose_mother) + 10,
            n = n()) |>
  mutate(
    mu = alpha / (alpha + beta),
    lb = qbeta(.025, shape1 = alpha, shape2 = beta),
    ub = qbeta(.975, shape1 = alpha, shape2 = beta)
  ) |>
  separate(context_label, into = c("race", "education")) |>
  mutate(
    x_lab = case_when(
      context == 1 ~ paste0(
        c("**Black** job candidate", "**Mid-ranked** educational institution"),
        collapse = "<br>"
      ),
      context == 2 ~ paste0(
        c("**Black** job candidate", "**Highly ranked** educational institution"),
        collapse = "<br>"
      ),
      context == 3 ~ paste0(
        c("**White** job candidate", "**Mid-ranked** educational institution"),
        collapse = "<br>"
      ),
      context == 4 ~ paste0(
        c("**White** job candidate", "**Highly ranked** educational institution"),
        collapse = "<br>"
      )
    )
  )

estimated_discrim_all_job_app <- ggplot(
    adaptive_estimates,
    aes(x = x_lab, y = mu, ymin = lb, ymax = ub)
  ) +
  geom_point() +
  geom_errorbar(width = 0.1) +
  geom_label(
    aes(
      label = paste0("N = ", n),
      y = 0.44
    ),
    # vjust = 1
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(
      hjust = 0,
      # margin = margin(l = -40)
    ),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(x = "", y = "Probability of preferring the non-mother job candidate") +
  scale_y_continuous(limits = c(0.41, 0.57))

ggsave(
  plot = estimated_discrim_all_job_app,
  filename = here("figures", "estimated_discrimination_all_job_app.png"),
  width = 7,
  height = 4,
  dpi = 500
)

