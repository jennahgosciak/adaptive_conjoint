library(tidyverse)
library(furrr)
library(tictoc)
plan(multisession, workers = 60)
knitr::opts_chunk$set(echo = TRUE)

theme_set(theme_bw())
# library(foreach)
# library(doParallel)
# library(doRNG)
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

update_outcomes <- function(df, num_contexts) {
  # vector of num outcomes for either 1 or 0
  df %>% 
    mutate(chose_educ0 = if_else(chose_educ==0 & !is.na(chose_educ), 1, 0)) %>% 
    group_by(context) %>% 
    summarize(chose1 = sum(chose_educ, na.rm = TRUE),
              chose0 = sum(chose_educ0, na.rm = TRUE))
}

update_ts <- function(df, num_sim, num_contexts, type) {
  # with the data provided
  # calculated the observed outcomes = 1, and outcomes = 0
  
  df_outcomes <- df %>% 
    update_outcomes(., num_contexts)
  
  num_outcome1 <- df_outcomes %>% 
    pull(chose1)
  
  num_outcome0 <- df_outcomes %>% 
    pull(chose0)

  # calculate the probability that each arm is the best
  draws <- replicate(num_sim, rbeta(num_contexts, num_outcome1 + 1, num_outcome0 + 1))
  if (type == "max") {
    #print("Predicting the most discriminatory context: taking the argmax")
    # calculate argmax across draws
    arg <- apply(draws, 2, which.max)
  } else if (type == "min") {
    #print("Predicting the least discriminatory context: taking the argmin")
    # calculate argmin across draws
    arg <- apply(draws, 2, which.min)
  } else {
    print("Direction 'type' is unclear")
  }

  # generate new pi
  # print(arg)
  # print(table(cut(arg, 0:num_contexts)))
  pi <- unname(table(cut(arg, 0:num_contexts)) / num_sim)
  #print(str_c("PDF: ", str_c(pi, collapse = ",")))
  stopifnot(length(pi) == num_contexts)
  return(pi)
}

adaptive_phase <- function(warmup_df, n_total, true_p, n_sim=1) {
  pi <- rep(1, length(true_p))/length(true_p)
  df <- warmup_df
  
  max_reached <- FALSE
  n <- 1
  while (max_reached == FALSE) {
    adaptive_draw <- draw_adaptive_sample(pi, true_p) %>% 
      format_sample_df(true_p) %>% 
      rename(chose_educ = value)
    
    df <- bind_rows(df, adaptive_draw)
    
    if ((n %% 200) == 0) {
      pi <- update_ts(df, 1000, length(true_p), "max")
      if (max(pi) >= 0.95) {
        print(pi)
        break
      }
    } else {
      pi <- update_ts(df, 1, length(true_p), "max")
    }
    # stopifnot(abs(sum(pi)-1) < 0.05)
    n <- n + 1
  }
  outcomes <- df %>% 
    update_outcomes(length(true_p)) %>% 
    mutate(n_i = i)
  
  return(lst(outcomes, pi))
}

full_experiment <- function(n_warmup, n_adaptive, true_p) {
  warmup_df <- draw_sample(round(n_warmup/length(true_p)), true_p) %>% 
    format_sample_df(true_p) %>% 
    rename(chose_educ = value)
  return(adaptive_phase(warmup_df, n_adaptive, true_p))
}

adaptive_warmupfixed <- function(true_p, n=50000) {
  adaptive_res <- full_experiment(100, n, true_p)
  final_pi <- adaptive_res$pi
  final_outcomes <- adaptive_res$outcomes
  mean_estimates_df <- final_outcomes %>% 
    rowwise() %>% 
    mutate(chose_n = chose1 + chose0,
           mean_est = chose1 / chose_n) %>% 
    arrange(context) %>% 
    cbind(., tibble(pi = final_pi))  %>% 
    ungroup() %>% 
    mutate(n_adaptive = n,
           n_total = n_i + 100)
  
  return(mean_estimates_df)
}

set.seed(1234)
n_sim <- 100
library(progressr)

run_with_progress <- function(n_sim, n_arms) {
  with_progress({
    # Initialize a progressor
    p <- progressor(steps = length(n_sim))
    
    res <- future_map_dfr(1:n_sim, ~{
      p()
      Sys.sleep(0.05) 
      adaptive_warmupfixed(c(seq(0.3, 0.6, length.out = n_arms-1), 0.7))
    },.options=furrr_options(seed=TRUE)) %>%
    mutate(num_arms = n_arms)
  })
  return(res)
}

# mean_estimates_warmupfixed <- bind_rows(run_with_progress(n_sim, 9),
#   run_with_progress(n_sim, 12)) %>%
#   bind_rows(run_with_progress(n_sim, 15)) %>%
#   bind_rows(run_with_progress(n_sim, 18)) %>%
#   bind_rows(run_with_progress(n_sim, 21))

# mean_estimates_warmupfixed %>%
#   saveRDS("adaptive_prob_fixed_effect.RDS")

mean_estimates_warmupfixed <- readRDS("adaptive_prob_fixed_effect.RDS")
mean_estimates_warmupfixed %>%
  distinct(num_arms)

mean_estimates_warmupfixed %>%  
  group_by(num_arms) %>% 
  summarize(mean_n = mean(n_total),
            ci_low = mean_n - qnorm(0.975) * sd(n_total) / sqrt(n()),
            ci_high = mean_n + qnorm(0.975) * sd(n_total) / sqrt(n())) %>% 
  ggplot(aes(num_arms, mean_n)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high)) +
  scale_y_continuous(limits=c(0, 9000)) +
  scale_x_continuous(breaks=c(9, 12, 15, 16, 18, 21)) +
  labs(x = 'Number of arms',
       y = 'Mean # of samples to obtain 95% posterior probability')

ggsave('./sample_size_adaptive_2.pdf')

n_adaptive_summary <- mean_estimates_warmupfixed %>% 
  mutate(n_total = 100 + n_i) %>% 
  group_by(num_arms) %>% 
  summarize(mean_n = mean(n_total),
            ci_low = mean_n - qnorm(0.975) * sd(n_total) / sqrt(n()),
            ci_high = mean_n + qnorm(0.975) * sd(n_total) / sqrt(n())) %>% 
  mutate(method = 'adaptive')

n_adaptive_summary %>% 
  summarize(max_n = max(ci_high))

est_avgn <- readRDS("equal_prob_constant_effect.RDS")

n_equal_summary <- est_avgn %>% 
  mutate(total_n = n*num_arms) %>% 
  group_by(num_arms) %>% 
  summarize(mean_n = mean(total_n),
            ci_low = mean_n - qnorm(0.975) * sd(total_n) / sqrt(n()),
            ci_high = mean_n + qnorm(0.975) * sd(total_n) / sqrt(n()))

n_equal_summary %>% 
  mutate(method = 'equal') %>% 
  bind_rows(n_adaptive_summary) %>% 
  ggplot(aes(num_arms, mean_n, color=method)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high)) +
  scale_y_continuous(limits=c(0, 9000)) +
  scale_x_continuous(breaks=c(9, 12, 15, 16, 18, 21)) +
  labs(x = 'Number of arms',
       y = 'Mean # of samples to obtain 95% posterior probability')

ggsave('./sample_size_adaptive_2.pdf')