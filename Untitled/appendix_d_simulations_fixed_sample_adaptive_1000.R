library(dplyr)
library(stringr)
library(here)
library(furrr)
library(progressr)
library(tidyr)

## Change this parameter to change the number of cores for parallelization
n_cores <- min(parallel::detectCores()-1, 120)
plan(multisession, workers = n_cores)

for (number_of_respondents in c(500, 1000)) {
  # change this parameter to change the number of simulations
  n_sim <- 1000
  
  draw_adaptive_sample <- function(pi, true_p) {
    profile_draw <- rmultinom(1, 1, pi)
    stopifnot(sum(profile_draw)==1)
    context <- which.max(profile_draw)
    
    output <- rep(NA, length(true_p))
    draw <- rbinom(n=1, size=1, prob=true_p[context])
    output[context] <- draw
    return(output)
  }
  
  warmup_phase <- function(n_total, true_p, n_sim=1000, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    pi_equal <- rep(1, length(true_p))/length(true_p)
    
    n <- 0
    while (n < n_total) {
      n <- n + 1
      profile_draw <- rmultinom(1, 1, pi_equal)
      context <- which.max(profile_draw)
      new_draw <- tibble(context = context,
                         outcome = rbinom(1,1, prob=true_p[context])) |> 
        mutate(y1 = if_else(outcome==1,1,0),
               y0 = if_else(outcome==0,1,0))
      
      if (n==1) {
        df <- new_draw
      } else {
        df <- bind_rows(df, new_draw)
      }
    }
    outcomes <- df |> 
      right_join(tibble(context = 1:length(true_p)), by = join_by(context)) |>
      group_by(context) |>
      summarize(y0 = sum(y0, na.rm = TRUE),
                y1 = sum(y1, na.rm = TRUE)) |> 
      ungroup()
    
    pi <- outcomes |> 
      mutate(theta_star = map2(.x = y1, .y = y0, 
                               .f = \(x,y) rbeta(n_sim, 
                                                 x + 1, y + 1)))  |> 
      unnest(theta_star) |> 
      mutate(sim_index = rep(x = 1:n_sim, times = length(true_p)))  |> 
      group_by(sim_index) |> 
      summarize(max_arm = which.max(theta_star)) |> 
      group_by(max_arm) |> 
      summarize(n = n()) |> 
      ungroup() |> 
      mutate(pi = n / n_sim) |> 
      right_join(tibble(max_arm = 1:length(true_p)), by = join_by(max_arm)) |>
      mutate(pi = if_else(is.na(pi),0,pi)) |> 
      arrange(max_arm) |> 
      pull(pi)
    
    outcomes <- outcomes |> 
      arrange(context) |> 
      cbind(pi) |> 
      mutate(n_total = n,
             num_arms = length(true_p)) |> 
      mutate(chose_correct = if_else(num_arms == which.max(pi), 1, 0))
    
    return(outcomes)
  }
  
  adaptive_phase <- function(true_p, num_warmup=100, num_total=number_of_respondents-100, n_sim=1000, seed) {
    set.seed(seed)
    df <- warmup_phase(num_warmup, true_p)
    
    pi <- rep(1, length(true_p))/length(true_p)
    
    max_reached <- FALSE
    n <- 0
    while (n < num_total) {
      n <- n + 1
      profile_draw <- rmultinom(1, 1, pi)
      context <- which.max(profile_draw)
      adaptive_draw <- tibble(context = context,
                              outcome = rbinom(1,1, prob=true_p[context])) |> 
        mutate(y1 = if_else(outcome==1,1,0),
               y0 = if_else(outcome==0,1,0))
      
      df <- bind_rows(df, adaptive_draw)
      
      if (n==num_total) {
        pi <- df |> 
          group_by(context) |> 
          summarize(y1 = sum(y1),
                    y0 = sum(y0)) |> 
          mutate(theta_star = map2(.x = y1, .y = y0, 
                                   .f = \(x,y) rbeta(n_sim, 
                                                     x + 1, y + 1))) |> 
          unnest(theta_star) |> 
          mutate(sim_index = rep(x = 1:n_sim, times = length(true_p))) |> 
          group_by(sim_index) |> 
          summarize(max_arm = which.max(theta_star)) |> 
          group_by(max_arm) |> 
          summarize(n = n()) |> 
          ungroup() |> 
          mutate(pi = n / n_sim) |> 
          right_join(tibble(max_arm = 1:length(true_p)), by = join_by(max_arm)) |>
          mutate(pi = if_else(is.na(pi),0,pi)) |> 
          arrange(max_arm) |>
          pull(pi)
      } else {
        pi <- df |> 
          group_by(context) |> 
          summarize(y1 = sum(y1),
                    y0 = sum(y0)) |> 
          right_join(tibble(context = 1:length(true_p)), by = join_by(context)) |>
          mutate(y1 = if_else(is.na(y1),0,y1),
                 y0 = if_else(is.na(y0),0,y0)) |> 
          mutate(theta_star = rbeta(n(), y1 + 1, y0 + 1)) |> 
          arrange(context) |> 
          mutate(max_arm = as.numeric(row_number() == which.max(theta_star))) |>
          pull(max_arm)
      }
      stopifnot(abs(sum(pi)-1) < 0.05)
      stopifnot(length(pi)==length(true_p))
    }
    outcomes <- df |> 
      group_by(context) |>
      summarize(y0 = sum(y0),
                y1 = sum(y1)) |> 
      ungroup() |> 
      cbind(pi) |> 
      mutate(n_total = n + num_warmup,
             num_arms = length(true_p)) |> 
      mutate(chose_correct = if_else(num_arms == which.max(pi), 1, 0))
    
    return(outcomes)
  }
  
  
  run_with_progress <- function(n_sim, n_arms) {
    set.seed(815555)
    seed_seq <- sample(1L:1e6L, n_sim)
    true_p <- c(seq(0.3, 0.65, length.out = n_arms-1), 0.7)
    
    with_progress({
      # Initialize a progressor
      p <- progressor(steps = length(n_sim))
      
      res <- future_map_dfr(seed_seq, function(x){
        Sys.sleep(0.05)
        ap <- adaptive_phase(true_p, seed = x)
        # p()
        return(ap)
      },.options=furrr_options(seed=TRUE))
    })
    res_equal <- future_map_dfr(
      seed_seq,
      function(x) warmup_phase(number_of_respondents, true_p, seed = x),
      .options=furrr_options(seed=TRUE)
    ) |> 
      mutate(type='Equal')
    
    res <- res |>
      mutate(type='Adaptive') |>
      bind_rows(res_equal)
    return(res)
  }
  
  outcomes <- run_with_progress(n_sim, 9) |>
    bind_rows(run_with_progress(n_sim, 12)) |>
    bind_rows(run_with_progress(n_sim, 15)) |>
    bind_rows(run_with_progress(n_sim, 18)) |>
    bind_rows(run_with_progress(n_sim, 21)) |>
    bind_rows(run_with_progress(n_sim, 24)) |>
    bind_rows(run_with_progress(n_sim, 27)) |>
    bind_rows(run_with_progress(n_sim, 30)) |>
    mutate(n_resp = number_of_respondents)
  outcomes
  
  
  outcomes |>
    saveRDS(here(str_glue("data/simulation-data/fixed_sample_adaptive_sim_1000_{number_of_respondents}.RDS")))
  
}
