update_outcomes <- function(df, num_contexts, num_outcome, outcome_value) {
  context_cols <- as.character(c(1:num_contexts))
  # vector of num outcomes for either 1 or 0
  obs_outcome <- table(df[df$chose_younger == outcome_value, "context"])[context_cols] %>%
    unname()
  obs_outcome[is.na(obs_outcome)] <- 0

  # check each vector is the length of the number of contexts
  stopifnot(length(obs_outcome) == num_contexts)

  # add the observed outcomes to the count of outcomes
  # for each context
  return(num_outcome + obs_outcome)
}

update_outcomes_loop <- function(df, varname, num_contexts, num_outcome1, num_outcome0) {
  var <- enquo(varname)
  # iterate through each context number
  # update the number of 1,0 responses based on the data
  for (i in 1:num_contexts) {
    obs_responses <- df %>%
      filter(context == i) %>%
      pull(!!var)
    num_outcome1[i] <- num_outcome1[i] + sum(obs_responses)
    num_outcome0[i] <- num_outcome0[i] + sum(obs_responses == 0)
  }

  # check each vector is the length of the number of contexts
  stopifnot(length(num_outcome1) == num_contexts)
  stopifnot(length(num_outcome0) == num_contexts)
  return(lst(num_outcome1, num_outcome0))
}

update_ts <- function(df, varname, num_sim, num_contexts, num_outcome1, num_outcome0, cdf, type) {
  # with the data provided
  # calculated the observed outcomes = 1, and outcomes = 0

  # uncomment this for the vectorized approach
  # num_outcome1 <- update_outcomes(df, num_contexts, num_outcome1, 1)
  # num_outcome0 <- update_outcomes(df, num_contexts, num_outcome0, 0)

  # this approach uses a for loop
  upd_outcomes <- update_outcomes_loop(df, varname, num_contexts, num_outcome1, num_outcome0)
  num_outcome1 <- upd_outcomes$num_outcome1
  num_outcome0 <- upd_outcomes$num_outcome0

  # calculate the probability that each arm is the best
  print(num_sim)
  draws <- replicate(num_sim, rbeta(num_contexts, num_outcome1 + 10, num_outcome0 + 10))
  if (type == "Iterative Batch Phase: Max") {
    print("Predicting the most discriminatory context: taking the argmax")
    # calculate argmax across draws
    arg <- apply(draws, 2, which.max)
  } else if (type == "Iterative Batch Phase: Min") {
    print("Predicting the least discriminatory context: taking the argmin")
    # calculate argmin across draws
    arg <- apply(draws, 2, which.min)
  } else {
    print("Direction 'type' is unclear")
  }

  # generate new pi
  pi <- unname(table(cut(arg, 0:num_contexts)) / num_sim)
  print(str_c("PDF: ", str_c(pi, collapse = ",")))
  if (cdf == TRUE) {
    pi <- cumsum(pi)
    print(str_c("CDF: ", str_c(pi, collapse = ",")))
  }
  stopifnot(length(pi) == num_contexts)
  return(lst(pi, num_outcome1, num_outcome0))
}
