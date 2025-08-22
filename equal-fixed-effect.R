library(tidyverse)

warmup_phase <- function(true_p, n_sim=1000) {
  pi_equal <- rep(1, length(true_p))/length(true_p)
  
  n <- 0
  max_reached <- FALSE
  while (max_reached == FALSE) {
    n <- n + 1
    profile_draw <- rmultinom(1, 1, pi_equal)
    context <- which.max(profile_draw)
    print(context)
    print(true_p[context])
    new_draw <- tibble(context = context,
                       outcome = rbinom(1,1, prob=true_p[context])) %>% 
      mutate(y1 = if_else(outcome==1,1,0),
             y0 = if_else(outcome==0,1,0))
    
    if (n==1) {
      df <- new_draw
    } else {
      df <- bind_rows(df, new_draw)
    }
    
    if ((n %% 100) == 0) {
      pi <- df %>%
        group_by(context) %>% 
        summarize(y1 = sum(y1),
                  y0 = sum(y0)) %>% 
        mutate(theta_star = map2(.x = y1, .y = y0, 
                                 .f = \(x,y) rbeta(n_sim, 
                                                   x + 1, y + 1))) %>% 
        unnest(theta_star) %>% 
        mutate(sim_index = rep(x = 1:n_sim, times = length(unique(context)))) %>% 
        group_by(sim_index) %>% 
        summarize(max_arm = which.max(theta_star)) %>% 
        group_by(max_arm) %>% 
        summarize(n = n()) %>% 
        ungroup() %>% 
        mutate(pi = n / n_sim) %>% 
        left_join(tibble(max_arm = 1:length(true_p)), ., by = join_by(max_arm)) %>%
        mutate(pi = if_else(is.na(pi),0,pi)) %>% 
        pull(pi)
      
      stopifnot(length(pi)==length(true_p))
      print(pi)
      if (max(pi) >= 0.95) {
        max_reached <- TRUE
      }
    }
  }
  outcomes <- df %>% 
    left_join(tibble(context = 1:length(true_p)), ., by = join_by(context)) %>%
    group_by(context) %>%
    summarize(y0 = sum(y0, na.rm = TRUE),
              y1 = sum(y1, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(context) %>% 
    cbind(pi) %>% 
    mutate(n_total = n,
           num_arms = length(true_p)) %>% 
    mutate(chose_correct = if_else(num_arms == which.max(pi), 1, 0))
  return(outcomes)
}

n_sim <- 10
set.seed(1234)
run_with_progress <- function(n_sim, n_arms) {
  true_p <- c(seq(0.3, 0.6, length.out = n_arms-1), 0.7)
  
  with_progress({
    # Initialize a progressor
    p <- progressor(steps = length(n_sim))

    res <- future_map_dfr(1:n_sim, ~{
      p()
      Sys.sleep(0.05)
      warmup_phase(true_p)
    },.options=furrr_options(seed=TRUE))
  })
  return(res)
}

outcomes <- run_with_progress(n_sim, 9) %>%
  bind_rows(run_with_progress(n_sim, 12)) %>%
  bind_rows(run_with_progress(n_sim, 15)) %>%
  bind_rows(run_with_progress(n_sim, 18)) %>%
  bind_rows(run_with_progress(n_sim, 21)) %>%
  bind_rows(run_with_progress(n_sim, 24)) %>%
  bind_rows(run_with_progress(n_sim, 27)) %>%
  bind_rows(run_with_progress(n_sim, 30))
outcomes