
#sink("../figures/normality_simulation_weighted.txt")

library(tidyverse)
library(foreach)
library(doParallel)
library(doRNG)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

set.seed(90095)

t0 <- Sys.time()
simulations <- foreach(rep = 1:1000, .packages = "tidyverse", .combine = "rbind") %dorng% {

  # Define parameters
  theta <- tibble(
    x = 0:1, theta = c(.4,.6)
  )
  
  # Draw warm-up sample of n = 10 per group
  data <- tibble(
   x = rep(0:1, each = 100)
  ) |>
   left_join(theta, by = join_by(x)) |>
   mutate(y = rbinom(n(), 1, theta)) |>
   mutate(pi = .5)
  
  # Draw adaptive sample of n = 50
  for (i in 1:50) {
    
    # Determine pi
    if (i == 1) {
      pi <- tibble(x = 0:1, pi = .5)
    } else {
      data_aggregate <- data |>
        group_by(x) |>
        summarize(y1 = sum(y == 1), y0 = sum(y == 0))
      if (!(all(0:1 %in% data_aggregate$x))) {
        data_aggregate <- data_aggregate |>
          bind_rows(
            tibble(x = 0:1, y1 = 0, y0 = 0) |>
              filter(!(x %in% data_aggregate$x))
          )
      }
      pi <- data_aggregate |>
        group_by(x) |>
        mutate(theta_star = list(rbeta(1000, y1 + 1, y0 + 1))) |>
        unnest(theta_star) |>
        mutate(index = 1:n()) |>
        group_by(index) |>
        arrange(index, theta_star) |>
        slice_tail(n = 1) |>
        ungroup() |>
        count(x) |>
        mutate(pi = n / sum(n)) |>
        select(-n)
    }
    
    new <- pi |>
      slice_sample(n = 1, weight_by = pi) |>
      left_join(theta, by = join_by(x)) |>
      mutate(
        y = rbinom(1, 1, theta)
      )
    
    # if (i == 1) {
    #   data <- new
    # } else {
      data <- data |>
        bind_rows(new)
    # }
  }
  
  data |>
    mutate(rep = rep) |>
    mutate(index = 1:n())
}

spent <- difftime(Sys.time(), t0)

aggregate_estimates <- function(simulations) {
  simulations |>
    # Apply estimators to data
    group_by(rep, x) |>
    summarize(
      n = n(),
      theta = unique(theta),
      unweighted = list(lm(y ~ 1)),
      weighted = list(lm(y ~ 1, w = 1 / pi)),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = c("unweighted","weighted"), 
      names_to = "estimator", 
      values_to = "fit"
    ) |>
    group_by(rep, x, estimator) |>
    mutate(
      estimate = map(fit, coef),
      # Note: This confidence interval uses t-distribution I think,
      # because it is not the same as what I get from normal approximation
      ci = map(fit, confint),
      covers = map2(.x = theta, .y = ci, \(x,y) y[1] < x & y[2] > x)
    ) |>
    select(x, theta, rep, n, estimator, estimate, covers, ci) |>
    unnest(cols = c("estimate", "covers","ci"))
}

# Visualize bias across total sample size
bias_by_n <- foreach(i = seq(100,max(simulations$index),10), .combine = "rbind") %do% {
  aggregate_estimates(simulations |> filter(index <= i)) |>
    arrange(x, estimator) |>
    group_by(x, estimator) |>
    summarize(
      bias = mean(estimate - theta),
      se = sd(estimate) / sqrt(n()),
      .groups = "drop"
    ) |>
    mutate(
      ci.min = bias - qnorm(.975) * se,
      ci.max = bias + qnorm(.975) * se,
      n = i
    )
}
bias_by_n |>
  filter(n >= 100) |>
  ggplot(
    aes(
      x = n, y = bias, ymin = bias - qnorm(.975) * se,
      ymax = bias + qnorm(.975) * se
    )
  ) +
  geom_point() +
  geom_errorbar(width = .1) +
  facet_wrap(~estimator + x, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()

simulations |>
  group_by(x, rep) |>
  count() |>
  ggplot(aes(x = n)) +
  geom_histogram() +
  facet_wrap(~x, ncol = 1, labeller = as_labeller(\(x) paste("Samples from X =",x)))

# sink()