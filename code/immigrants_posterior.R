
# Posterior probabilities at end of warmup + validation

library(here)
library(tidyverse)

set.seed(90095)

sink(here("figures/immigrants_posterior.txt"))

print(Sys.time())

immigrants_response <- read_csv(
  here("data/immigrants_main_response.csv"),
  col_types = "ilciiccccililildicc"
)

print("Posterior probabilities of being correct lowest and highest contexts")

immigrants_response |>
  group_by(arm_id) |>
  summarize(
    y1 = sum(discriminated),
    y0 = sum(1 - discriminated)
  ) |>
  group_by(arm_id) |>
  mutate(
    sims = list(rbeta(100000, y1 + 1, y0 + 1))
  ) |>
  select(arm_id, sims) |>
  pivot_wider(names_from = arm_id, values_from = sims) |>
  mutate(index = list(1:100000)) |>
  unnest(everything()) |>
  pivot_longer(cols = -index, names_to = "arm", values_to = "value") |>
  group_by(index) |>
  mutate(
    is_lowest = value == min(value),
    is_highest = value == max(value)
  ) |>
  select(arm, starts_with("is_")) |>
  group_by(arm) |>
  summarize_all(mean) |>
  arrange(is_lowest)


# Confirm that n in each arm matches n in other figure in text
immigrants_response |>
  group_by(arm_id) |>
  count() |>
  ungroup() |>
  arrange(n)

print("Posterior probability that lowest-estimate context is lower than next-lowest")

immigrants_response |>
  filter(arm_id %in% c(2,10)) |>
  group_by(arm_id) |>
  summarize(
    y1 = sum(discriminated),
    y0 = sum(1 - discriminated)
  ) |>
  group_by(arm_id) |>
  mutate(
    sims = list(rbeta(100000, y1 + 1, y0 + 1))
  ) |>
  select(arm_id, sims) |>
  pivot_wider(names_from = arm_id, values_from = sims) |>
  unnest(everything()) |>
  ungroup() |>
  summarize(is_lower = mean(`10` < `2`))

sink()



