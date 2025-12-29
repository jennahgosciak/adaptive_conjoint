library(here)
library(dplyr)
library(furrr)
library(progressr)
library(tidyr)

## Change this parameter to change the number of cores for parallelization
n_cores <- parallel::detectCores()-1
plan(multisession, workers = n_cores)

## change this parameter to change the number of simulations
n_sim <- 10

warmup_phase <- function(n_total, true_p) {
  pi <- rep(1, length(true_p))/length(true_p)
  
  n <- 1
  while (n <= n_total) {
    profile_draw <- rmultinom(1, 1, pi)
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
    n <- n + 1
  }
  outcomes <- df |> 
    group_by(context) |>
    summarize(y0 = sum(y0),
              y1 = sum(y1)) |> 
    ungroup()
  
  return(outcomes)
}

adaptive_phase <- function(true_p, n_sim=10) {
  print(length(true_p))
  df <- warmup_phase(100, true_p)
  
  pi <- rep(1, length(true_p))/length(true_p)
  
  max_reached <- FALSE
  n <- 1
  while (max_reached == FALSE) {
    profile_draw <- rmultinom(1, 1, pi)
    context <- which.max(profile_draw)
    adaptive_draw <- tibble(context = context,
                            outcome = rbinom(1,1, prob=true_p[context])) |> 
      mutate(y1 = if_else(outcome==1,1,0),
             y0 = if_else(outcome==0,1,0))
    
    df <- bind_rows(df, adaptive_draw)
    
    if ((n %% 100) == 0) {
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
        pull(pi)

      if (max(pi) >= 0.8) {
        max_reached <- TRUE
      }
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
    n <- n + 1
  }
  outcomes <- df |> 
    group_by(context) |>
    summarize(y0 = sum(y0),
              y1 = sum(y1)) |> 
    ungroup() |> 
    cbind(pi) |> 
    mutate(n_total = n,
           num_arms = length(true_p))
  
  return(outcomes)
}

set.seed(1234)
run_with_progress <- function(n_sim, n_arms) {
  true_p <- c(seq(0.3, 0.65, length.out = n_arms-1), 0.7)
  
  with_progress({
    # Initialize a progressor
    p <- progressor(steps = length(n_sim))

    res <- future_map_dfr(1:n_sim, ~{
      # p()
      Sys.sleep(0.05)
      adaptive_phase(true_p)
    },.options=furrr_options(seed=TRUE))
  })
  return(res)
}

outcomes <- run_with_progress(n_sim, 9) |>
  bind_rows(run_with_progress(n_sim, 12)) |>
  bind_rows(run_with_progress(n_sim, 15)) |>
  bind_rows(run_with_progress(n_sim, 18)) |>
  bind_rows(run_with_progress(n_sim, 21)) |>
  bind_rows(run_with_progress(n_sim, 24)) |>
  bind_rows(run_with_progress(n_sim, 27)) |>
  bind_rows(run_with_progress(n_sim, 30))
outcomes

outcomes |>
  saveRDS(here("data/simulation-data/fixed_effect_adaptive_sim_1000.RDS"))
