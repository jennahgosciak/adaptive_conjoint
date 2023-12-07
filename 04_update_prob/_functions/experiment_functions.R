update_outcomes <- function(df, num_profiles, num_outcome, outcome_value) {
  profile_cols <- as.character(c(1:num_profiles))
  # vector of num outcomes for either 1 or 0
  obs_outcome <- table(df[df$candidate_response == outcome_value, "profile"])[profile_cols] %>%
    unname()
  obs_outcome[is.na(obs_outcome)] <- 0

  # check each vector is the length of the number of profiles
  stopifnot(length(obs_outcome) == num_profiles)

  # add the observed outcomes to the count of outcomes
  # for each profile
  return(num_outcome + obs_outcome)
}

update_outcomes_loop <- function(df, num_profiles, num_outcome1, num_outcome0) {
  # iterate through each profile number
  # update the number of 1,0 responses based on the data
  for (i in 1:num_profiles) {
    obs_responses <- df %>%
      filter(profile == i) %>%
      pull(candidate_response)
    num_outcome1[i] <- num_outcome1[i] + sum(obs_responses)
    num_outcome0[i] <- num_outcome0[i] + sum(obs_responses == 0)
  }

  # check each vector is the length of the number of profiles
  stopifnot(length(obs_outcome1) == num_profiles)
  stopifnot(length(obs_outcome0) == num_profiles)
  return(lst(num_outcome1, num_outcome0))
}

update_ts <- function(df, num_sim, num_profiles, num_outcome1, num_outcome0, cdf) {
  # with the data provided
  # calculated the observed outcomes = 1, and outcomes = 0
  
  # uncomment this for the vectorized approach
  # num_outcome1 <- update_outcomes(df, num_profiles, num_outcome1, 1)
  # num_outcome0 <- update_outcomes(df, num_profiles, num_outcome0, 0)
  
  # this approach uses a for loop
  upd_outcomes <- update_outcomes_loop(df, num_profiles, num_outcome1, num_outcome0)
  num_outcome1 <- upd_outcomes$num_outcome1
  num_outcome0 <- upd_outcomes$num_outcome0

  # calculate the probability that each arm is the best
  draws <- replicate(num_sim, rbeta(num_profiles, num_outcome1, num_outcome0))
  # calculate argmax across draws
  argmax <- apply(draws, 2, which.max)

  # generate new pi
  pi <- unname(table(cut(argmax, 0:num_profiles)) / num_sim)
  if (cdf == T) {
    pi <- cumsum(pi)
  }
  stopifnot(length(pi) == num_profiles)
  return(lst(pi, num_outcome1, num_outcome0))
}

run_ts <- function(batch_size, num_profiles, pi_init, num_outcome1 = NULL, num_outcome0 = NULL, fake_data = T, cdf = T) {
  # if null, init number of outcomes in previous rounds to 0
  # generate warning message with output
  warning_message <- NULL
  if (is.null(num_outcome1)) {
    warning_message <- "Initializing number of outcomes=1 in previous rounds to 0"
    num_outcome1 <- integer(num_profiles)
  }
  if (is.null(num_outcome0)) {
    warning_message <- str_c(
      warning_message,
      "\nInitializing number of outcomes=0 in previous rounds to 0\n"
    )
    num_outcome0 <- integer(num_profiles)
  }

  if (fake_data == T) {
    # generate fake data
    df <- create_fake_data(pi_init, profile_prob, batch_size, num_profiles, cdf = cdf)
  } else {
    ## function to load qualtrics data
    df <- load_qualtrics("Political Candidates") %>%
      clean_qualtrics_data() %>%
      select_batch() %>%
      create_profile_var() %>%
      select(candidate_response, profile)
  }
  output <- update_ts(df, num_sim, num_profiles, num_outcome1, num_outcome0, cdf = cdf)

  # warn if automatically initialized outcome values in previous rounds to 0
  if (!is.null(warning_message)) {
    warning(warning_message)
  }
  return(output)
}
