
sink("../figures/normality_simulation.txt")

library(tidyverse)
library(foreach)
library(doParallel)
library(doRNG)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

set.seed(90095)

simulations <- foreach(rep = 1:1000, .combine = "rbind", .packages = "tidyverse") %dorng% {
  data_aggregated <- tibble(
    x = 0:1, y0 = 0, y1 = 0,
    theta = c(.1,.9)
  )
  
  # Draw adaptive sample of n = 1,000
  for (i in 1:1000) {
    new <- data_aggregated |>
      mutate(theta_star = rbeta(n(), y1 + 1, y0 + 1)) |>
      arrange(theta_star) |>
      slice_tail(n = 1) |>
      mutate(
        y = rbinom(1, 1, theta),
        y0 = ifelse(y == 0, y0 + 1, y0),
        y1 = ifelse(y == 1, y1 + 1, y1)
      ) |>
      select(all_of(colnames(data_aggregated)))
    data_aggregated <- data_aggregated |>
      filter(!(x %in% new$x)) |>
      bind_rows(new)
  }
  
  return(
    data_aggregated |>
      mutate(
        theta_hat = y1 / (y1 + y0),
        n = y1 + y0,
        se = sqrt(theta_hat * (1 - theta_hat) / n),
        ci.min = theta_hat - qnorm(.975) * se,
        ci.max = theta_hat + qnorm(.975) * se,
        covers = ci.min < theta & ci.max > theta
      )
  )
}

# Coverage
simulations |>
  group_by(x) |>
  summarize(
    coverage = mean(covers),
    n = n()
  ) |>
  mutate(
    se = sqrt(coverage * (1 - coverage) / n),
    ci.min = coverage - qnorm(.975) * se,
    ci.max = coverage + qnorm(.975) * se
  ) |>
  select(x,coverage, ci.min, ci.max)

# Bias
simulations |>
  group_by(x) |>
  summarize(
    bias = mean(theta_hat - theta),
    se = sd(theta_hat) / sqrt(n())
  ) |>
  mutate(
    ci.min = bias - qnorm(.975) * se,
    ci.max = bias + qnorm(.975) * se
  ) |>
  select(x,bias, ci.min, ci.max)

# Total samples
simulations |>
  group_by(x) |>
  summarize(
    min_n = min(y0 + y1),
    mean_n = mean(y0 + y1),
    max_n = max(y0 + y1)
  )
  
# Visualize departures from normality
simulations |>
  group_by(x) |>
  mutate(z_hat = (theta_hat - mean(theta_hat)) / sd(theta_hat)) |>
  select(x, z_hat) |>
  arrange(x, z_hat) |>
  mutate(
    p = (1:n()) / (n() + 1),
    z_theory = qnorm(p)
  ) |>
  ggplot(aes(x = z_theory, y = z_hat)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  facet_wrap(~x, labeller = as_labeller(\(x) paste("X =",x))) +
  xlab("Theoretical Normal Quantiles") +
  ylab("Empirical Normal Quantiles")

ggsave("../figures/normality_simulation.pdf", height = 3, width = 5)
