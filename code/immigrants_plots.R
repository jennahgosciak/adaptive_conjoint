library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(gtsummary)
library(here)
library(modelr)
library(readr)
library(tidyr)

# Import and clean survey data --------------------------------------

clean_ethnicity <- function(x) {
  x |>
    mutate(
      ethnicity_cleaned = factor(
        ethnicity,
        levels = c("hisp_latin_spanish_no", "hisp_latin_spanish_yes"),
        labels = c(
          "Not Hispanic or Latino",
          "Hispanic or Latino"
        )
      )
    )
}

clean_race <- function(x) {
  levs <- c(
    "race_white",
    "race_black",
    "race_asian",
    "race_aian",
    "race_nhpi",
    "race_other",
    "race_multiracial"
  )
  labs <- c(
    "White",
    "Black or African American",
    "Asian",
    "American Indian or Alaska Native",
    "Native Hawaiian or Pacific Islander",
    "Other",
    "Two or More Races"
  )
  x |>
    mutate(
      race_cleaned = case_when(
        is.na(race) ~ NA,
        race %in% levs ~ race,
        TRUE ~ "race_multiracial"
      ),
      race_cleaned = factor(race_cleaned, levels = levs, labels = labs)
    )
}

clean_sex <- function(x) {
  x |>
    mutate(
      sex_cleaned = factor(
        sex,
        levels = c("female", "male"),
        labels = c("Female", "Male")
      )
    )
}

clean_responses <- function(x) {
  x |>
    clean_ethnicity() |>
    clean_race() |>
    clean_sex()
}

# Import data
responses <- read_csv(here("data/immigrants_main_response.csv"))
responses_validation_max <- read_csv(here("data/immigrants_max_response.csv"))
responses_validation_min <- read_csv(here("data/immigrants_min_response.csv"))

# Clean data
responses <- responses |>
  clean_responses() |>
  mutate(phase_cleaned = "Main")

responses_validation_max <- responses_validation_max |>
  clean_responses() |>
  mutate(phase = "Max", phase_cleaned = phase)

responses_validation_min <- responses_validation_min |>
  clean_responses() |>
  mutate(phase = "Min", phase_cleaned = phase)

# Prep data for creating post-stratified estimates ------------------

# load poststratification weights from ipums acs survey
wgts <- readRDS(here("data/ipums_strata_sizes.RDS")) |>
  mutate(
    race = as.character(race),
    race = case_when(
      race == "Native Hawaiian or Other Pacific Islander" ~
        "Native Hawaiian or Pacific Islander",
      TRUE ~ race
    ),
    race = factor(race, levels = levels(responses$race_cleaned))
  )

# This function uses IPUMS demographic features to generate post-stratified 
# point estimates and calculate bootstrapped confidence intervals
poststratified_estimates <- function(df, arm = "max") {
  # First, drop people who choose not to disclose for any of the demographic features
  # Then align variable names with IPUMS
  df_filter <- df |>
    filter(if_all(.cols = c(race, ethnicity, sex, age), .fns = \(x) !is.na(x))) |>
    mutate(
      race = factor(as.character(race_cleaned), levels = levels(wgts$race)),
      female = sex == "female",
      hispanic = ethnicity == "hisp_latin_spanish_yes"
    )

  # exclude race categories that aren't in the data we have
  wgts <- wgts |>
    filter(race %in% unique(df_filter$race))

  # running logistic regression for predicting probability of choosing younger candidate
  glm_drop_cons_factors <- function(df, vars) {
    # need to drop factors that don't vary (e.g., only one racial category appears)
    if (length(unique(df$race)) <= 1) vars <- vars[vars != "race"]
    form <- paste0(vars, collapse = " + ")

    glm(
      formula = paste0("discriminated ~ ", form),
      family = "binomial",
      data = df
    )
  }

  # compute weighted probability of choosing the younger candidate
  # exclude race categories that aren't in the data when doing the prediction
  compute_weighted_prob <- function(df, wgts, lm) {
    # exclude race categories that aren't in the data we have
    wgts <- wgts |>
      filter(race %in% unique(as_tibble(df)[["race"]]))

    # predict probabilities
    prob <- predict(lm, newdata = wgts, type = "response")
    # return weighted mean
    return(weighted.mean(prob, w = wgts$weight))
  }

  df_models <- df_filter |>
    arrange(arm_id) |>
    group_by(arm_id) |>
    nest() |>
    mutate(glm = lapply(
      data,
      \(x) {
        glm_drop_cons_factors(
          x,
          c("race", "female", "hispanic", "age")
        )
      }
    ))

  # predict based on population weight categories
  # and then compute the weighted mean
  w_mean <- lapply(
    1:length(df_models$data),
    \(index) {
      data <- df_models[["data"]][[index]]
      model <- df_models[["glm"]][[index]]
      compute_weighted_prob(data, wgts, model)
    }
  ) |>
    unlist()

  df_post <- tibble(
    context = df_models$arm_id,
    estimate = w_mean
  )

  # Construct parameter CI using bootstrapping with 1000 replicates
  produce_bootstrap_estimates <- function(df, c_val, iter = 1000) {
    df_bootstrap <- df |>
      filter(arm_id == c_val) |>
      bootstrap(iter) |>
      mutate(glm = lapply(
        strap,
        \(x) {
          glm_drop_cons_factors(
            x,
            c("female", "hispanic", "age", "race")
          )
        }
      ))

    # produce bootstrap estimate
    bootstrap_est <- lapply(
      1:length(df_bootstrap$strap),
      \(index) {
        strap <- df_bootstrap$strap[[index]]
        model <- df_bootstrap$glm[[index]]
        compute_weighted_prob(strap, wgts, model)
      }
    ) |>
      unlist()

    # produce mean and sd
    tibble(
      se = sd(bootstrap_est),
      context = c_val
    ) |>
      select(context, se)
  }

  c_val <- unique(df_filter$arm_id)
  df_post_bootstrap <- lapply(c_val, \(x) produce_bootstrap_estimates(df_filter, c_val)) |>
    bind_rows() |>
    full_join(df_post, ., by = c("context")) |>
    # adding bootstrap standard error to calculation of ci with point estimate of the mean
    mutate(
      phase = "Poststratified",
      arm_id = case_when(.env$arm == "max" ~ 7, TRUE ~ 10),
      mu = estimate,
      lb = estimate - (qt(.975, df = nrow(df_filter) - 1) * se),
      ub = estimate + (qt(.975, df = nrow(df_filter) - 1) * se)
    ) |>
    select(arm_id, mu, lb, ub, phase)

  return(df_post_bootstrap)
}

# Descriptive statistics table --------------------------------------

descriptive_stats <- responses |>
  bind_rows(responses_validation_max) |>
  bind_rows(responses_validation_min) |>
  filter(!garbage) |>
  select(phase_cleaned, age, ends_with("_cleaned")) |>
  tbl_summary(
    by = phase_cleaned,
    label = list(
      age ~ "Age",
      ethnicity_cleaned ~ "Ethnicity",
      race_cleaned ~ "Race",
      sex_cleaned ~ "Sex"
    )
  ) |>
  bold_labels() |>
  as_gt()

gt::gtsave(descriptive_stats, here("figures", "descriptive_stats.tex"))

# Plots -------------------------------------------------------------

# Generate post-stratified estimates from the validation phase
ps_max <- poststratified_estimates(responses_validation_max, "max")
ps_min <- poststratified_estimates(responses_validation_min, "min")
ps <- bind_rows(ps_max, ps_min)

# Import parameters estimated during the experimental phases
params_main <- read_csv(here("data/immigrants_main_parameters.csv")) 
params_main_final_batch <- params_main |>
  filter(batch_id == max(batch_id)) |>
  arrange(desc(arm_id)) |>
  group_by(arm_id) |>
  mutate(
    mu = alpha/(alpha + beta),
    lb = qbeta(0.025, alpha, beta),
    ub = qbeta(0.975, alpha, beta),
    phase = "Warmup + Adaptive"
  ) |>
  ungroup() |>
  select(arm_id, mu, lb, ub, phase)

params_main_min_max <- params_main_final_batch |>
  filter(arm_id %in% c(10, 7))

params_validation <- responses_validation_max |> 
  mutate(arm_id = 7) |>
  bind_rows(
    mutate(responses_validation_min, arm_id = 10)
  ) |>
  filter(!garbage) |> 
  group_by(arm_id) |>
  summarise(
    mu = mean(discriminated),
    sd = sd(discriminated), root_n = sqrt(n()),
    df = n() - 1
  ) |> 
  ungroup() |>
  group_by(arm_id) |>
  mutate(
    lb = mu - qt(0.975, df = df)*sd/root_n,
    ub = mu - qt(0.025, df = df)*sd/root_n,
    phase = "Validation"
  ) |>
  ungroup() |>
  select(arm_id, mu, lb, ub, phase)

# Merge estimates from all phases into a single dataframe
params_comparison <- bind_rows(params_main_min_max, params_validation) |>
  bind_rows(ps) |>
  arrange(arm_id) |>
  mutate(
    phase = factor(
      phase,
      levels = c("Warmup + Adaptive", "Validation", "Poststratified"),
      ordered = TRUE
    ),
    x_lab = case_when(
      arm_id == 7 ~ paste0(
        c(
          "**African** country of origin",
          "**Few** prior visits",
          "Seeking **better employment**",
          "**Lower-skilled** profession"
        ),
        collapse = "<br>"
      ),
      arm_id == 10 ~ paste0(
        c(
          "**European** country of origin",
          "**Many** prior visits",
          "Escaping **political/religious persecution**",
          "**Higher-skilled** profession"
        ),
        collapse = "<br>"
      )
    )
  )

### Figure (2): Estimates of $theta_c$ for all contexts
estimated_discrim_all_plot <- ggplot(
    params_main_final_batch,
    aes(x = fct_reorder(factor(arm_id), desc(mu)), y = mu, ymin = lb, ymax = ub)
  ) +
  geom_point() +
  geom_errorbar(width = 0.1) +
  theme_minimal() +
  labs(x = "Context ID", y = "Posterior expected probability")

ggsave(
  plot = estimated_discrim_all_plot,
  filename = here("figures", "estimated_discrimination_all.png"),
  width = 5,
  height = 3,
  dpi = 500
)

### Figure (3): Estimates of $theta_c$ for the most/least discrim. contexts
estimated_discrim_max_min_plot <- ggplot(
    params_comparison,
    aes(x = x_lab, y = mu, ymin = lb, ymax = ub, color = phase)
  ) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(width = 0.1, position = position_dodge(width = 0.3)) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(hjust = 0, margin = margin(l = -40)),
    legend.title = element_blank()
  ) +
  labs(x = "", y = "Posterior expected probability")

ggsave(
  plot = estimated_discrim_max_min_plot,
  filename = here("figures", "estimated_discrimination_max_min.png"),
  width = 6,
  height = 4,
  dpi = 500
)

### Figure (4): Certainty of estimates over time
params_main_filtered <- params_main |> 
  filter(batch_id %in% pull(filter(responses, !garbage), batch_id)) |>
  left_join(
    select(distinct(responses, batch_id, .keep_all = TRUE), batch_id, phase),
    by = "batch_id"
  ) |>
  filter(phase != "warmup") |>
  arrange(batch_id, arm_id) |>
  mutate(
    mu = alpha/(alpha + beta),
    lb = qbeta(0.025, alpha, beta),
    ub = qbeta(0.975, alpha, beta),
    width = ub - lb
  ) |>
  group_by(batch_id) |>
  mutate(group_id = cur_group_id()) |>
  ungroup() |>
  group_by(phase) |>
  mutate(max_batch_id = max(batch_id)) |>
  ungroup()

ci_width_all <- params_main_filtered |>
  filter(!arm_id %in% c(10, 7)) |>
  ggplot(aes(x = group_id, y = width, group = arm_id)) +
  geom_line(color = "gray80") +
  geom_line(
    data = params_main_filtered |> 
      filter(arm_id == 10) |>
      mutate(Context = "Min"),
    aes(color = Context)
  ) +
  geom_line(
    data = params_main_filtered |> 
      filter(arm_id == 7) |>
      mutate(Context = "Max"),
    aes(color = Context)
  ) +
  geom_vline(
    xintercept = params_main_filtered |>
      filter(phase == "max") |>
      pull(max_batch_id) |>
      first(),
    color = "black",
    linetype = "dashed"
  ) +
  theme_minimal() +
  labs(
    x = "Progress through adaptive phase",
    y = "Credible interval width"
  )

ggsave(
  plot = ci_width_all,
  filename = here("figures", "ci_width_all.png"),
  width = 6,
  height = 4,
  dpi = 500
)

ci_max_min_df <- params_main_filtered |>
  filter(arm_id == 7 & phase == "max" | arm_id == 10 & phase == "min") |>
  group_by(arm_id) |>
  mutate(progress = 1:n()) |>
  ungroup()

ci_width_max_min <- ci_max_min_df |>
  mutate(
    context = case_when(
      arm_id == 10 ~ "Context with least discrimination",
      TRUE ~ "Context with most discrimination"
    )
  ) |>
  ggplot(aes(x = progress, y = mu, ymin = lb, ymax = ub)) +
  geom_ribbon(alpha = 0.2) +
  geom_line(size = 0.5) +
  facet_wrap(~ context, nrow = 1) +
  theme_minimal() +
  labs(x = "", y = "Posterior expected probability credible interval")

ggsave(
  plot = ci_width_max_min,
  filename = here("figures", "ci_width_max_min.png"),
  width = 6,
  height = 4,
  dpi = 500
)
