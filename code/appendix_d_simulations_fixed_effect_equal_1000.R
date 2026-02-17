library(dplyr)
library(here)
library(furrr)
library(progressr)
library(tidyr)

## Change this parameter to change the number of cores for parallelization
n_cores <- min(parallel::detectCores()-1, 120)
plan(multisession, workers = n_cores)

## change this parameter to change the number of simulations
n_sim <- 1000

warmup_phase <- function(true_p, n_sim=1000, seed) {
  set.seed(seed)
  # initialize pi with equal probabilities
  pi_equal <- rep(1, length(true_p))/length(true_p)

  # initialize to FALSE (will flip to true when pi for one context > 0.95)
  max_reached <- FALSE

  # initialize to 0
  n <- 0

  while (max_reached == FALSE) {
    n <- n + 1
    # note, even though we calculate a different pi
    # we will *always* assign with equal probabilities (pi_equal)
    profile_draw <- rmultinom(1, 1, pi_equal)
    # identify the context we select
    context <- which.max(profile_draw)
    # simulate a response based on the true success probability (true_p)
    new_draw <- tibble(context = context,
                       outcome = rbinom(1,1, prob=true_p[context])) |> 
      # define y1 and y0 based on whether the outcome was 1 or 0
      mutate(y1 = if_else(outcome==1,1,0),
             y0 = if_else(outcome==0,1,0))
    
    # if the first draw, initialize
    if (n==1) {
      df <- new_draw
    } else {
      # combine with previous draws otherwise
      df <- bind_rows(df, new_draw)
    }
    
    # produce the full pi vector in batches of 100
    if ((n %% 100) == 0) {
      pi <- df |>
        # this right join ensures we have observations for contexts even if no observations
        right_join(tibble(context = 1:length(true_p)), by = join_by(context)) |>
        arrange(context) |>
        group_by(context) |> 
        summarize(y1 = sum(y1, na.rm = TRUE),
                  y0 = sum(y0, na.rm = TRUE)) |> 
        # draw from rbeta n_sim (i.e., 1000) times
        mutate(theta_star = map2(.x = y1, .y = y0, 
                                 .f = \(x,y) rbeta(n_sim, 
                                                   x + 1, y + 1))) |> 
        unnest(theta_star) |> 
         # for each simulation, we select the maximum value of theta (across contexts)
        mutate(sim_index = rep(x = 1:n_sim, times = length(true_p))) |> 
        group_by(sim_index) |> 
        # select the right arm by the max theta value for each simulation
        summarize(max_arm = which.max(theta_star)) |> 
        # this fraction is the new pi value
        group_by(max_arm) |> 
        summarize(n = n()) |> 
        ungroup() |> 
        mutate(pi = n / n_sim) |> 
        # ensure any contexts not selected are included with 0 pi values
        right_join(tibble(max_arm = 1:length(true_p)), by = join_by(max_arm)) |>
        mutate(pi = if_else(is.na(pi),0,pi)) |> 
        # ensure pi is properly ordered
        arrange(max_arm) |>
        pull(pi)
      
      stopifnot(abs(sum(pi)-1) < 0.01)
      stopifnot(length(pi)==length(true_p))

      # stop the loop if any pi value is >= 0.95 for any context
      if (max(pi) >= 0.95) {
        max_reached <- TRUE
      }
    }
  }

  # once loop is finished, compute some overall statistics
  outcomes <- df |> 
    group_by(context) |>
    summarize(y0 = sum(y0),
              y1 = sum(y1)) |> 
    ungroup() |> 
    # this right join ensures all contexts are included
    # even if we haven't collected observations for one
    right_join(tibble(context = 1:length(true_p)), by = join_by(context)) |>
    arrange(context) |> 
    # add in the final pi values
    cbind(pi) |> 
    # n_total should be multiple of 100
    mutate(n_total = n,
           num_arms = length(true_p)) |> 
    mutate(chose_correct = if_else(num_arms == which.max(pi), 1, 0))
  return(outcomes)
}

run_with_progress <- function(n_sim, n_arms) {
  set.seed(344371)
  seed_seq <- sample(1L:1e6L, n_sim)
  # ensure the difference between the penultimate and final arm
  # is always 0.05, regardless of total number of arms
  true_p <- c(seq(0.3, 0.65, length.out = n_arms-1), 0.7)
  
  with_progress({
    # initialize a progressor
    p <- progressor(steps = n_sim)
    
    res <- future_map_dfr(seed_seq, function(x){
      p()
      warmup_phase(true_p, seed = x)
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
  saveRDS(here("data/simulation-data/fixed_effect_equal_sim_1000.RDS"))
