library(dplyr)
library(stringr)
library(here)
library(furrr)
library(progressr)
library(tidyr)

# change this parameter to change the number of cores for parallelization
n_cores <- min(parallel::detectCores()-1, 120)
plan(multisession, workers = n_cores)

warmup_phase <- function(n_total, true_p, n_sim=1000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  pi_equal <- rep(1, length(true_p))/length(true_p)
  
  n <- 0
  # for the warmup face, while n < the total allocated continue drawing
  while (n < n_total) {
    # advance 1
    n <- n + 1
    # draw a profile with equal probabilities across profiles (pi_equal)
    profile_draw <- rmultinom(1, 1, pi_equal)
    # identify the context we select
    context <- which.max(profile_draw)
    # simulate a response based on the true success probability (true_p)
    new_draw <- tibble(context = context,
                       outcome = rbinom(1,1, prob=true_p[context])) |> 
      # define y1 and y0 based on whether the outcome was 1 or 0
      mutate(y1 = if_else(outcome==1,1,0),
             y0 = if_else(outcome==0,1,0))
    
    if (n==1) {
      # iniitalize if the first draw
      df <- new_draw
    } else {
      # bind rows together if not the first draw
      df <- bind_rows(df, new_draw)
    }
  }
  outcomes <- df |> 
    # this right join ensures that even contexts with 0 responses are included
    right_join(tibble(context = 1:length(true_p)), by = join_by(context)) |>
    group_by(context) |>
    # sum both y0 and y1 (for all previous draws)
    summarize(y0 = sum(y0, na.rm = TRUE),
              y1 = sum(y1, na.rm = TRUE)) |> 
    ungroup()
  
  # calculate pi
  pi <- outcomes |> 
    mutate(theta_star = map2(.x = y1, .y = y0, 
    # simulate rbeta for n_sim (i.e., =1000) times
                             .f = \(x,y) rbeta(n_sim, 
                                               x + 1, y + 1)))  |> 
    unnest(theta_star) |> 
    # create the simulation index
    mutate(sim_index = rep(x = 1:n_sim, times = length(true_p)))  |> 
    # for each simulation, we select the maximum value of theta (across contexts)
    group_by(sim_index) |> 
    summarize(max_arm = which.max(theta_star)) |> 
    # summarize how many times each context was selected as the max (out of n_sim)
    group_by(max_arm) |> 
    summarize(n = n()) |> 
    ungroup() |> 
    # this fraction is the new pi value
    mutate(pi = n / n_sim) |> 
    # ensure any contexts not selected are included with 0 pi values
    right_join(tibble(max_arm = 1:length(true_p)), by = join_by(max_arm)) |>
    mutate(pi = if_else(is.na(pi),0,pi)) |> 
    # sort by the max arm (1:length(true_p))
    arrange(max_arm) |> 
    # extract pi vector
    pull(pi)
  
  # create outcomes df
  # sort by context; add in the final pi vector
  outcomes <- outcomes |> 
    arrange(context) |> 
    cbind(pi) |> 
    # total_n is column based on sampel size
    mutate(n_total = n,
           num_arms = length(true_p)) |> 
    # check if we chose correctly
    # num_arms is always the correct arm because the highest value of
    # true_p is the last value
    mutate(chose_correct = if_else(num_arms == which.max(pi), 1, 0))
  
  return(outcomes)
}

adaptive_phase <- function(true_p, num_warmup=100, num_total=number_of_respondents-100, n_sim=1000, seed) {
  set.seed(seed)
  # first run warmup phase with 100 participants
  df <- warmup_phase(num_warmup, true_p)
  
  # initialize pi vector with equal probabilities
  # pi <- rep(1, length(true_p))/length(true_p)
  pi <- df |>
    arrange(context) |>
    pull(pi)
  
  n <- 0
  # num total = num_respondents - num_warmup
  while (n < num_total) {
    n <- n + 1
    # draw from different profiles with changing pi vector
    profile_draw <- rmultinom(1, 1, pi)
    # identify the context we select
    context <- which.max(profile_draw)
    # simulate a response based on the true success probability (true_p)
    # for the context we selected
    adaptive_draw <- tibble(context = context,
                            outcome = rbinom(1,1, prob=true_p[context])) |> 
      # initialize y1 and y0
      mutate(y1 = if_else(outcome==1,1,0),
             y0 = if_else(outcome==0,1,0))
    
    # combine with previous responses including warmup
    df <- bind_rows(df, adaptive_draw)
    
    # if we reach the total (num_total = num_respondents - num_warmup)
    if (n==num_total) {
      pi <- df |> 
        group_by(context) |> 
        # ensure we include contexts even if no observations
        right_join(tibble(context = 1:length(true_p)), by = join_by(context)) |>
        summarize(y1 = sum(y1, na.rm = TRUE),
                  y0 = sum(y0, na.rm = TRUE)) |> 
        # simulate beta distribution for n_sim times
        mutate(theta_star = map2(.x = y1, .y = y0, 
                                 .f = \(x,y) rbeta(n_sim, 
                                                   x + 1, y + 1))) |> 
        unnest(theta_star) |> 
        # for each simulation, we select the maximum value of theta (across contexts)
        mutate(sim_index = rep(x = 1:n_sim, times = length(true_p))) |> 
        # summarize how many times each context was selected as the max (out of n_sim)
        group_by(sim_index) |> 
        summarize(max_arm = which.max(theta_star)) |> 
        # this fraction is the new pi value
        group_by(max_arm) |> 
        summarize(n = n()) |> 
        ungroup() |> 
        mutate(pi = n / n_sim) |> 
        # ensure any contexts not selected are included with 0 pi values
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
        # assign contexts, but only drawing once per context
        mutate(theta_star = rbeta(n(), y1 + 1, y0 + 1)) |> 
        arrange(context) |> 
        # this should be a binary vector
        # 1 is in the position for the max arm
        mutate(max_arm = as.numeric(context == which.max(theta_star))) |>
        pull(max_arm)
    }
    # ensure pi vector sums to 1 (valid probability mass function)
    stopifnot(abs(sum(pi)-1) < 0.01)
    stopifnot(length(pi)==length(true_p))
  }
  outcomes <- df |> 
    arrange(context) |>
    group_by(context) |>
    summarize(y0 = sum(y0),
              y1 = sum(y1)) |> 
    ungroup() |> 
    # add in pi vector
    cbind(pi) |> 
    # n_total must be equal to 500 or 1000
    mutate(n_total = n + num_warmup,
           num_arms = length(true_p)) |> 
    # binary indicator if the max value of pi would lead us to correctly select
    # the arm with the maximum value of true_p
    mutate(chose_correct = if_else(num_arms == which.max(pi), 1, 0))
  
  return(outcomes)
}

for (number_of_respondents in c(500, 1000)) {
  # change this parameter to change the number of simulations
  n_sim <- 1000
  
  run_with_progress <- function(n_sim, n_arms) {
    set.seed(815555)
    seed_seq <- sample(1L:1e6L, n_sim)
    # initialize the true probability values so the difference
    # between the penultimate and last values is always fixed
    true_p <- c(seq(0.3, 0.65, length.out = n_arms-1), 0.7)
    
    with_progress({
      # initialize a progressor
      p <- progressor(steps = n_sim)
      
      res <- future_map_dfr(seed_seq, function(x){
        ap <- adaptive_phase(true_p, seed = x)
        p()
        return(ap)
      },.options=furrr_options(seed=TRUE))
    })
    
    # compute a version with just the warmup phase (i.e., equal probability sampling)
    res_equal <- future_map_dfr(
      seed_seq,
      function(x) warmup_phase(number_of_respondents, true_p, seed = x),
      .options=furrr_options(seed=TRUE)
    ) |> 
      mutate(type='Equal')
    
    # combine equal + adaptive
    res <- res |>
      mutate(type='Adaptive') |>
      bind_rows(res_equal)
    return(res)
  }
  
  # bind everything together (num_arms: 9 - 30)
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
