update_ts <- function(df, num_sim, num_profiles, num_outcome1, num_outcome0) {
  # with the data provided
  # calculated the observed outcomes = 1, and outcomes = 0
  
  profile_cols <- as.character(c(1:num_profiles))
  # vector of num outcomes = 1
  obs_outcome1 <- table(df[df$candidate_response == 1, "profile"])[profile_cols] %>% 
    unname()
  obs_outcome1[is.na(obs_outcome1)] <- 0
  
  # vector of num outcomes = 0
  obs_outcome0 <- table(df[df$candidate_response == 0, "profile"])[profile_cols] %>% 
    unname()
  obs_outcome0[is.na(obs_outcome0)] <- 0
  
  # check each vector is the length of the number of profiles
  stopifnot(length(obs_outcome1)==num_profiles)
  stopifnot(length(obs_outcome0)==num_profiles)
  
  # add the observed outcomes to the count of outcomes
  # for each profile
  num_outcome1 <- num_outcome1 + obs_outcome1
  num_outcome0 <- num_outcome0 + obs_outcome0
  
  # calculate the probability that each arm is the best
  draws <- replicate(num_sim, rbeta(num_profiles, num_outcome1, num_outcome0))
  # calculate argmax across draws
  argmax <- apply(draws, 2, which.max)
  # generate new pi
  pi_cdf_upd <- cumsum(unname(table(cut(argmax, 0:num_profiles)) / num_sim))
  stopifnot(length(pi_cdf_upd) == num_profiles)
  return(lst(pi_cdf_upd, num_outcome1, num_outcome0))
}

run_ts <- function(batch_size, pi_init, fake_data = T) {
  # init parameters
  num_outcome1 <- integer(num_profiles) # vector for each arm
  num_outcome0 <- integer(num_profiles)
  pi <- lst(pi_init) # init uniform treatment assignment
  
  if (fake_data == T) {
    # generate fake data
    df <- create_fake_data(pi_init, profile_prob, batch_size, num_profiles)
  } else {
    ## function to load qualtrics data
    df <- load_qualtrics("Political Candidates") %>% 
      clean_qualtrics_data() %>% 
      select_batch() %>% 
      create_profile_var() %>% 
      select(candidate_response, profile)
  }
  output <- update_ts(df, num_sim, num_profiles, num_outcome1, num_outcome0)
  num_outcome1 <- output$num_outcome1
  num_outcome0 <- output$num_outcome0
  return(lst(output$pi))
}