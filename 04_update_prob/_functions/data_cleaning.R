load_qualtrics <- function(survey_name) {
  # load api key permissions
  readRenviron("~/.Renviron")

  # load survey data
  surveys <- all_surveys()
  # select survey ID
  pc_id <- surveys[surveys["name"] == survey_name, ][["id"]]

  return(fetch_survey(
    surveyID = pc_id,
    verbose = TRUE,
    force_request = TRUE
  ))
}

filter_test_data <- function(df) {
  df %>%
    filter(Status == "Survey Test")
}

clean_political_data <- function(df) {
  df %>%
    # = 1 if selecting the younger candidate
    # = 0 if selecting the older candidate
    mutate(across(str_c("Q", 1:8), ~ case_when(
      . == "Candidate 1" & rnum_age <= 0.5 ~ 1,
      . == "Candidate 2" & rnum_age > 0.5 ~ 1,
      . == "Candidate 1" & rnum_age > 0.5 ~ 0,
      . == "Candidate 2" & rnum_age <= 0.5 ~ 0,
      TRUE ~ NA_real_
    ))) %>%
    mutate(candidate_response = select(., str_c("Q", 1:8)) %>%
      rowSums(na.rm = T))
}

clean_job_data <- function(df) {
  df %>%
    # = 1 if selecting non-mother
    # = 0 if selecting mother candidate
    mutate(across(str_c("Q", 1:8), ~ case_when(
      . == "Candidate 1" & rnum_mother <= 0.5 ~ 1,
      . == "Candidate 2" & rnum_mother > 0.5 ~ 1,
      . == "Candidate 1" & rnum_mother > 0.5 ~ 0,
      . == "Candidate 2" & rnum_mother <= 0.5 ~ 0,
      TRUE ~ NA_real_
    ))) %>%
    mutate(candidate_response = select(., str_c("Q", 1:8)) %>%
             rowSums(na.rm = T))
}

select_batch <- function(df) {
  df %>%
    arrange(desc(StartDate)) %>%
    head(100)
}

# should be missing if participants do not consent
check_consent <- function(df) {
  df %>%
    filter(Consent == "I do not consent to participate") %>%
    mutate(Q2 = as.character(Q2)) %>%
    distinct(`Q2`) %>%
    is.na() %>%
    stopifnot()
}

# should be missing if they are not in the US
check_location_screen <- function(df) {
  filtered_df <- df %>%
    filter(PreScreen_Q1 != "Yes")
  
  if (!all(is.na(filtered_df$Q1))) {
    stop("Not all Q1 values are missing.")
  }
}

check_completion <- function(df) {
  df %>%
    select("Finished") %>%
    equals(TRUE) %>%
    all() %>%
    stopifnot()
}

create_profile_var_political <- function(df, pi) {
  df %>%
    # create 'profile' variable
    mutate(profile = case_when(
      rnum <= pi1 ~ 1,
      rnum > pi1 & rnum <= pi2 ~ 2,
      rnum > pi2 & rnum <= pi3 ~ 3,
      rnum > pi3 & rnum <= pi4 ~ 4,
      rnum > pi4 & rnum <= pi5 ~ 5,
      rnum > pi5 & rnum <= pi6 ~ 6,
      rnum > pi6 & rnum <= pi7 ~ 7,
      rnum > pi7 ~ 8
    ))
}

create_profile_var_jobs <- function(df, pi) {
  df %>%
    # create 'profile' variable
    mutate(profile = case_when(
      rnum <= pi1 ~ 1,
      rnum > pi1 & rnum <= pi2 ~ 2,
      rnum > pi2 & rnum <= pi3 ~ 3
    ))
}

create_fake_data <- function(pi, profile_prob, batch_size, num_profiles = 8, cdf = T) {
  # initialize empty dataframe
  df_fake <- tibble("candidate_response" = rep(NA, batch_size))
  df_fake["profile"] <- NA
  # randomly generate profile based on pi cdf
  for (i in 1:batch_size) {
    if (cdf == T) {
      rnum <- runif(1)
      profile <- case_when(
        rnum < pi[1] ~ 1,
        rnum >= pi[1] & rnum < pi[2] ~ 2,
        rnum >= pi[2] & rnum < pi[3] ~ 3,
        rnum >= pi[3] & rnum < pi[4] ~ 4,
        rnum >= pi[4] & rnum < pi[5] ~ 5,
        rnum >= pi[5] & rnum < pi[6] ~ 6,
        rnum >= pi[6] & rnum < pi[7] ~ 7,
        rnum >= pi[7] ~ 8
      )
    } else {
      rnum <- runif(1)
      profile <- case_when(
        rnum < pi[1] ~ 1,
        rnum >= sum(pi[1]) & rnum < sum(pi[1:2]) ~ 2,
        rnum >= sum(pi[1:2]) & rnum < sum(pi[1:3]) ~ 3,
        rnum >= sum(pi[1:3]) & rnum < sum(pi[1:4]) ~ 4,
        rnum >= sum(pi[1:4]) & rnum < sum(pi[1:5]) ~ 5,
        rnum >= sum(pi[1:5]) & rnum < sum(pi[1:6]) ~ 6,
        rnum >= sum(pi[1:6]) & rnum < sum(pi[1:7]) ~ 7,
        rnum >= sum(pi[1:7]) ~ 8
      )
      # profile <- match(1, rmultinom(1, size = 1, prob = pi))
    }

    # assign based on probability of choosing a younger profile
    df_fake[i, "candidate_response"] <- rbinom(1, 1, profile_prob[profile])
    df_fake[i, "profile"] <- profile
  }
  return(df_fake)
}
