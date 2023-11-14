library(tidyverse)
library(foreach)

n_x <- 4
truth <- data.frame(
  x = 1:n_x,
  pi1 = seq(.2, .8, by = .2),
  pi2 = seq(.8, .2, by = -.2)
)
prior <- data.frame(
  x = 1:n_x,
  alpha1 = 1,
  beta1 = 1,
  alpha2 = 1,
  beta2 = 1
)

draw_data <- function(x_val) {
  data.frame(x = x_val) %>%
    left_join(truth, by = "x") %>%
    mutate(
      y1 = rbinom(1, 1, pi1),
      y2 = rbinom(1, 1, pi2)
    ) %>%
    select(x, y1, y2)
}

draw_posterior <- function(data) {
  posterior <- prior %>%
    left_join(
      data %>%
        group_by(x) %>%
        summarize(
          y1_yes = sum(y1),
          y1_no = sum(1 - y1),
          y2_yes = sum(y2),
          y2_no = sum(1 - y2),
          .groups = "drop"
        ),
      by = "x"
    ) %>%
    mutate(
      alpha1_post = alpha1 + y1_yes,
      beta1_post = beta1 + y1_no,
      alpha2_post = alpha2 + y2_yes,
      beta2_post = beta2 + y2_no,
      pi1_post = rbeta(n(), alpha1_post, beta1_post),
      pi2_post = rbeta(n(), alpha2_post, beta2_post)
    )
}

choose_highest <- function(posterior_draw) {
  posterior_draw %>%
    mutate(difference = pi1_post - pi2_post) %>%
    arrange(-difference) %>%
    slice_head(n = 1)
}

# Draw initial data
data <- foreach(i = 1:n_x, .combine = "rbind") %do% draw_data(x_val = i)

index <- 1
while (index <= 10) {
  # Draw a posterior
  posterior <- draw_posterior(data)
  # Pick the highest
  highest <- choose_highest(posterior)
  # Draw new data
  data <- data %>%
    bind_rows(draw_data(highest$x))
  index <- index + 1
}

# Visualize posteriors over time
n_sims <- 1e3
posterior_parameters <- data %>%
  mutate(time = 1:n()) %>%
  group_by(x) %>%
  mutate(
    posterior_alpha1 = 1 + cumsum(y1),
    posterior_alpha2 = 1 + cumsum(y2),
    posterior_beta1 = 1 + cumsum(1 - y1),
    posterior_beta2 = 1 + cumsum(1 - y2)
  ) %>%
  ungroup() %>%
  mutate(estimate = posterior_alpha1 / (posterior_alpha1 + posterior_beta1) -
    posterior_alpha2 / (posterior_alpha2 + posterior_beta2))

posterior_ci <- foreach(i = 1:nrow(posterior_parameters), .combine = "rbind") %do% {
  sim1 <- rbeta(
    n_sims,
    posterior_parameters$posterior_alpha1[i],
    posterior_parameters$posterior_beta1[i]
  )
  sim2 <- rbeta(
    n_sims,
    posterior_parameters$posterior_alpha2[i],
    posterior_parameters$posterior_beta2[i]
  )
  sim <- sim1 - sim2
  return(data.frame(
    ci.min = quantile(sim, .025),
    ci.max = quantile(sim, .975)
  ))
}
rownames(posterior_ci) <- NULL
posterior_parameters %>%
  bind_cols(posterior_ci) %>%
  right_join(
    data.frame(
      x = rep(1:n_x, each = nrow(data)),
      time = rep(1:nrow(data), n_x)
    ),
    by = c("x", "time")
  ) %>%
  select(x, time, estimate, ci.min, ci.max) %>%
  group_by(x) %>%
  arrange(x, time) %>%
  fill(estimate, ci.min, ci.max) %>%
  ggplot(aes(
    x = time, y = estimate,
    color = factor(x), fill = factor(x),
    ymin = ci.min, ymax = ci.max
  )) +
  geom_line() +
  geom_ribbon(alpha = .2, color = NA) +
  facet_wrap(~x, ncol = 1) +
  xlim(c(0, 30))
