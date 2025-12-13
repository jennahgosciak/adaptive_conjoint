library(tidyverse)
library(furrr)
library(tictoc)
plan(multisession, workers = 60)
knitr::opts_chunk$set(echo = TRUE)

theme_set(theme_bw())
library(foreach)
library(doParallel)
library(doRNG)
library(assertr)
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)

### Set up functions
draw_sample <- function(n_size, true_p) {
  # return(unlist(map(true_p, ~rbinom(n=1, size=1, prob=.))))
  return(rbinom(n=n_size*length(true_p), size=1, prob=true_p))
}

draw_adaptive_sample <- function(pi, true_p) {
  profile_draw <- rmultinom(1, 1, pi)
  stopifnot(sum(profile_draw)==1)
  context <- which.max(profile_draw)

  output <- rep(NA, length(true_p))
  draw <- rbinom(n=1, size=1, prob=true_p[context])
  output[context] <- draw
  return(output)
}

format_sample_df <- function(sample_draws, true_p) {
  sample_draws %>%
    as_tibble() %>%
    mutate(context = ((row_number()-1) %% length(true_p)) + 1)
}

produce_est2 <- function(n, true_p, num_sim=100) {
  est <- lst()
  for (sim in c(1:num_sim)) {
    outcomes <- draw_sample(n, true_p)
    outcomes_df <- outcomes %>%
      format_sample_df(true_p)

    outcomes_df_grpd <- outcomes_df %>%
      group_by(context) %>%
      summarize(mean_est = mean(value),
                y1 = sum(value==1),
                y0 = sum(value==0)) %>%
      mutate(n_sim = sim) %>%
      ungroup()

    est[[sim]] <- outcomes_df_grpd
  }
  est_df <- bind_rows(est) %>%
    mutate(n_size = n)
  return(est_df)
}

produce_est <- function(n, true_p, num_sim=1000) {
  print(length(true_p))
    # print("#############")
    # print(str_glue("N={n}"))

  est <- lst()
  for (sim in c(1:num_sim)) {
    outcomes <- draw_sample(n, true_p)
    outcomes_df <- outcomes %>%
      format_sample_df(true_p)

    outcomes_df_grpd <- outcomes_df %>%
      group_by(context) %>%
      summarize(mean_est = mean(value),
                y1 = sum(value==1),
                y0 = sum(value==0)) %>%
      mutate(n_sim = sim) %>%
      ungroup()  %>%
      mutate(theta_star = map2(.x = y1, .y = y0,
                               .f = \(x,y) rbeta(1000,
                                                 x + 1, y + 1))) %>%
            unnest(theta_star)  %>%
      group_by(context) %>%
      mutate(index = 1:1000) %>%
      arrange(index, context) %>%
      group_by(index, n_sim) %>%
      summarize(max_arm = which.max(theta_star)) %>%
      group_by(max_arm, n_sim) %>%
      summarize(prob = n()/1000)

    est[[sim]] <- outcomes_df_grpd
  }
  est_df <- bind_rows(est) %>%
    mutate(n_size = n)
  return(est_df)
}

# true_p <- seq(0.3, 0.7, length.out = 9)
# true_p

# est_df <- suppressMessages(future_map_dfr(seq(10,510,500), ~produce_est(., true_p)))
# est_df

# est_df %>% 
#   mutate(n_total = n_size*length(true_p)) %>% 
#   group_by(n_total, max_arm) %>% 
#   summarize(mean_prob = mean(prob),
#             prob_ci_min = quantile(prob, 0.025),
#             prob_ci_max = quantile(prob, 0.975)) %>% 
#   ggplot(aes(n_total, mean_prob)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = prob_ci_min, ymax = prob_ci_max)) +
#   geom_hline(yintercept=0.95, color='red') +
#   facet_wrap(~max_arm) +
#   scale_y_continuous(limits=c(0,1.05))

# ggsave("./equal_sampling.pdf")

produce_avgn <- function(n_increment, true_p, num_sim=1000) {
  est <- lst()
  for (sim in c(1:num_sim)) {
    max_prob <- 0
    n <- 10
    while (max_prob < 0.95) {
      outcomes <- draw_sample(n, true_p)
      outcomes_df <- outcomes %>%
        format_sample_df(true_p)

      outcomes_df_grpd <- outcomes_df %>%
        group_by(context) %>%
        summarize(mean_est = mean(value),
                  y1 = sum(value==1),
                  y0 = sum(value==0)) %>%
        mutate(n_sim = sim,
               n = n) %>%
        ungroup()  %>%
        mutate(theta_star = map2(.x = y1, .y = y0,
                                 .f = \(x,y) rbeta(1000,
                                                   x + 1, y + 1))) %>%
              unnest(theta_star)  %>%
        group_by(context, n) %>%
        mutate(index = 1:1000) %>%
        arrange(index, context) %>%
        group_by(index, n_sim, n) %>%
        summarize(max_arm = which.max(theta_star)) %>%
        group_by(max_arm, n_sim, n) %>%
        summarize(prob = n()/1000) %>%
        ungroup() %>%
        filter(max_arm == length(true_p)) %>%
        mutate(num_arms = length(true_p))

        print(outcomes_df_grpd$prob[1])
        max_prob <- if_else(!is.na(outcomes_df_grpd$prob[1]),
        outcomes_df_grpd$prob[1], max_prob)
        n <- n + n_increment
    }
    print(max_prob)
    est[[sim]] <- outcomes_df_grpd

  }
  est_df <- bind_rows(est) %>%
    mutate(n_size = n)
  return(est_df)
}

set.seed(1234)
est_avgn <- future_map_dfr(c(9, 12, 15, 16, 18, 21, 24, 27, 30), 
                    ~produce_avgn(10, 
                    c(seq(0.3, 0.6, length.out = .-1), 0.7), 
                         num_sim = 1000),.options=furrr_options(seed=TRUE))

est_avgn %>%
  saveRDS("equal_prob_constant_effect.RDS")
est_avgn <- readRDS("equal_prob_constant_effect.RDS")

est_avgn %>% 
  mutate(total_n = n*num_arms) %>% 
  group_by(num_arms) %>% 
  summarize(mean_n = mean(total_n),
            ci_low = mean_n - qnorm(0.975) * sd(total_n) / sqrt(n()),
            ci_high = mean_n + qnorm(0.975) * sd(total_n) / sqrt(n())) %>% 
  ggplot(aes(num_arms, mean_n)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high)) +
  scale_y_continuous(limits=c(0, 9000)) +
  scale_x_continuous(breaks=c(9, 12, 15, 16, 18, 21)) +
  labs(x = 'Number of arms',
       y = 'Mean # of samples to obtain 95% posterior probability')

ggsave('./sample_size_avgn_equal_prob_2.pdf')