The data in this directory comprise all data collected from the experimental phases of our study.
All files beginning with `immigrants_...` are collected from our primary example, the immigration
study. There are two additional files: `job_app_clean_all_phases.csv` which contains the raw responses from our
supplemental job applicant experiment, and `ipums_strata_sizes.RDS` which contains poststratification
weights for poststratified causal effect estimates in the validation phase of the immigration study.

The sections below describe how to interpret and work with the raw data from our immigration study.

# Study Phase

There were three phases of the study: the main phase (encapsulating warmup, adaptive max, and adaptive min), the max validation phase, and the min validation phase.

- `immigrants_main_...csv`: These files correspond to the main phase
- `immigrants_max_...csv`: These files correspond to the max validation phase
- `immigrants_min_...csv`: These files correspond to the min validation phase

The data for each of these three phases are also backed up in separate volumes in AWS.

# Phase-level Data

In each phase of the study, the following seven tables were collected:
- **Bandit**: A small table that contains the `label` and `id` for each bandit arm.
- **Batch**: A table that contains the `id` and some corresponding (but useless) information for each batch of the study. This table is really only useful as a linking table
since several other tables use `batch_id` as a foreign key.
- **Metadata**: This contains all the immigrant profile metadata that was shown to respondents. Each bandit arm had two profiles, so this table has (2 * `nrow(Bandit)`) rows.
- **NoConsent**: This contains sparse entries for all respondents who did not consent to participate. For these folks we collected no data, so this is really only useful to
know the N of how many refused to consent.
- **Parameters**: Each (batch x arm) from has a set of posterior Beta parameters. Each row in this table has two foreign keys: `arm_id` linking the parameters
to one of the bandit arms, and `batch_id` linking the parameters to a specific batch.
- **Pi**: Exactly like the **Parameters** table except instead of posterior parameters, this contains the MC calculated probability of discriminating (or not discriminating
depending on the phase).
- **Response**: This contains all the responses and corresponding information.

# Usage

As a simple example, to get a table of responses for all the phases of the study and filter out the bad responses, do the following:
```r
library(dplyr)
library(here)
library(readr)

responses_main <- read_csv(here("data/immigrants_main_response.csv"))
responses_max <- read_csv(here("data/immigrants_max_response.csv")) |> mutate(phase = "validation_max")
responses_min <- read_csv(here("data/immigrants_min_response.csv")) |> mutate(phase = "validation_min")

responses <- responses_main |>
  bind_rows(responses_max) |>
  bind_rows(responses_min) |>
  filter(!garbage)
```

To plot posterior means and 95% credible intervals for the estimated probability of discrimination in each arm:
```r
library(dplyr)
library(forcats)
library(here)
library(ggplot2)
library(readr)

params <- read_csv(here("data/immigrants_main_parameters.csv"))
params_last <- params |>
  filter(batch_id == max(batch_id)) |>
  group_by(arm_id) |>
  summarize(
    mu = alpha/(alpha + beta),
    ub = qbeta(0.975, alpha, beta),
    lb = qbeta(0.025, alpha, beta)
  )

ggplot(params_last, aes(x = fct_reorder(factor(arm_id), desc(mu)), y = mu, ymin = lb, ymax = ub)) +
  geom_errorbar(width = 0.2) +
  geom_point() +
  labs(x = "Arm ID", y = "Posterior probability of discrimination")
```

To plot the main results figure:
```r
library(dplyr)
library(forcats)
library(here)
library(ggplot2)
library(readr)

params <- read_csv(here("data/immigrants_main_parameters.csv"))
params_main <- params |>
  filter(batch_id == max(batch_id), arm_id %in% c(7, 10)) |>
  group_by(arm_id) |>
  summarize(
    mu = alpha/(alpha + beta),
    ub = qbeta(0.975, alpha, beta),
    lb = qbeta(0.025, alpha, beta)
  ) |>
  mutate(phase = "Main")

responses_max <- read_csv(here("data/immigrants_max_response.csv")) |>
  filter(!garbage) |>
  mutate(arm_id = 7)
responses_min <- read_csv(here("data/immigrants_min_response.csv")) |>
  filter(!garbage) |>
  mutate(arm_id = 10)
params_validation <- bind_rows(responses_max, responses_min) |>
  group_by(arm_id) |>
  summarise(
    mu = mean(discriminated),
    ub = t.test(discriminated, conf.level = 0.95)$conf.int[2],
    lb = t.test(discriminated, conf.level = 0.95)$conf.int[1]
  ) |>
  mutate(phase = "Validation")

params_main_valid <- bind_rows(params_main, params_validation)

ggplot(
  params_main_valid,
  aes(
    x = fct_reorder(factor(arm_id), desc(mu)),
    y = mu,
    ymin = lb,
    ymax = ub,
    color = phase
  )
) +
  geom_errorbar(width = 0.1, position = position_dodge(width = 0.3)) +
  geom_point(position = position_dodge(width = 0.3)) +
  labs(x = "Arm ID", y = "Estimated probability of discrimination", color = "") +
  theme_minimal() +
  theme(
    text = element_text(size = 12)
  )
```
