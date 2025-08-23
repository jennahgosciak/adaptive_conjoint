
library(tidyverse)
library(here)
set.seed(90095)

sink(here("figures/job_applicants_posterior.txt"))
print(Sys.time())


job <- read_csv(here("data/job_app_clean_all_phases.csv"))

job |>
  group_by(context_label) |>
  summarize(
    # reversing codes so that 1 = chose non-mother
    y1 = sum(1 - chose_mother),
    y0 = sum(chose_mother)
  ) |>
  group_by(context_label) |>
  mutate(
    sims = list(rbeta(100000, y1 + 1, y0 + 1))
  ) |>
  select(context_label, sims) |>
  pivot_wider(names_from = context_label, values_from = sims) |>
  mutate(index = list(1:100000)) |>
  unnest(everything()) |>
  pivot_longer(cols = -index, names_to = "context_label", values_to = "value") |>
  group_by(index) |>
  mutate(
    is_lowest = value == min(value),
    is_highest = value == max(value)
  ) |>
  select(context_label, starts_with("is_")) |>
  group_by(context_label) |>
  summarize_all(mean) |>
  arrange(is_lowest)

sink()