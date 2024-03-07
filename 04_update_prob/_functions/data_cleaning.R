load_qualtrics <- function(survey_name) {
  # load api key permissions
  readRenviron("~/.Renviron")

  # load survey data
  surveys <- all_surveys()
  # select survey ID
  pc_id <- surveys[surveys["name"] == survey_name, ][["id"]]

  print(str_glue("Loading survey data for {survey_name}"))
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

create_outcome_var_political <- function(df) {
  df %>%
    # = 1 if selecting the younger candidate
    # = 0 if selecting the older candidate
    mutate(across(str_c("Q", 1:8), ~ case_when(
      . == "Candidate 1" & rnum_age <= 0.5 ~ 1,
      . == "Candidate 2" & rnum_age <= 0.5 ~ 0,
      . == "Candidate 2" & rnum_age > 0.5 ~ 1,
      . == "Candidate 1" & rnum_age > 0.5 ~ 0,
      TRUE ~ NA_real_
    ))) %>%
    mutate(chose_younger = select(., str_c("Q", 1:8)) %>%
      rowSums(na.rm = T))
}

create_outcome_var_jobs <- function(df) {
  df %>%
    # = 1 if selecting non-mother
    # = 0 if selecting mother candidate
    mutate(across(str_c("Q", 1:8), ~ case_when(
      . == "Candidate 2" & rnum_mother <= 0.5 ~ 1,
      . == "Candidate 1" & rnum_mother > 0.5 ~ 1,
      . == "Candidate 2" & rnum_mother > 0.5 ~ 0,
      . == "Candidate 1" & rnum_mother <= 0.5 ~ 0,
      TRUE ~ NA_real_
    ))) %>%
    mutate(chose_mother = select(., str_c("Q", 1:8)) %>%
      rowSums(na.rm = T))
}

select_batch <- function(df) {
  df %>%
    arrange(desc(StartDate)) %>%
    head(100)
}

# should be missing if participants do not consent
check_consent <- function(df) {
  no_consent_num <- df %>%
    filter(Consent == "I do not consent to participate") %>%
    nrow()

  if (no_consent_num > 0) {
    warning(str_glue("Dropping {no_consent_num} survey respondents who do not consent"))

    vars <- str_c("Q", 1:8)
    no_consent_with_data <- df %>%
      filter(Consent == "I do not consent to participate") %>%
      select(all_of(vars)) %>%
      is.na() %>%
      rowSums() %>%
      equals(0) %>%
      sum()

    if (no_consent_with_data > 0) {
      warning(str_glue("{no_consent_with_data} survey respondents who do not consent with non-missing responses"))
    }

    df <- df %>%
      filter(Consent != "I do not consent to participate")

    warning(str_glue("{nrow(df)} respondents in the data"))
  }
  return(df)
}

# should be missing if they are not in the US
check_location_screen <- function(df) {
  no_prescreen_num <- df %>%
    filter(PreScreen_Q1 != "Yes") %>%
    nrow()

  if (no_prescreen_num > 0) {
    warning(str_glue("Dropping {no_prescreen_num} survey respondents who are not in the US"))

    vars <- str_c("Q", 1:8)
    no_prescreen_with_data <- df %>%
      filter(PreScreen_Q1 != "Yes") %>%
      select(all_of(vars)) %>%
      is.na() %>%
      rowSums() %>%
      equals(0) %>%
      sum()

    if (no_prescreen_with_data > 0) {
      warning(str_glue("{no_prescreen_with_data} survey respondents who are not in the US with non-missing responses"))
    }

    df <- df %>%
      filter(PreScreen_Q1 == "Yes")

    warning(str_glue("{nrow(df)} respondents in the data"))
  }
  return(df)
}

check_hiring_screen <- function(df) {
  no_hiring_prescreen_num <- df %>%
    filter(Prescreen_Q2 != "Yes") %>%
    nrow()

  if (no_hiring_prescreen_num > 0) {
    warning(str_glue("Dropping {no_hiring_prescreen_num} survey respondents who have not been involved in hiring decisions"))

    vars <- str_c("Q", 1:8)
    no_hiring_prescreen_with_data <- df %>%
      filter(Prescreen_Q2 != "Yes") %>%
      select(all_of(vars)) %>%
      is.na() %>%
      rowSums() %>%
      equals(0) %>%
      sum()

    if (no_hiring_prescreen_with_data > 0) {
      warning(str_glue("{no_hiring_prescreen_with_data} survey respondents who have not been involved in hiring decisions"))
    }

    df <- df %>%
      filter(Prescreen_Q2 == "Yes")

    df %>%
      distinct(Manipulation_Q2_TEXT) %>%
      print()

    warning(str_glue("{nrow(df)} respondents in the data"))
  }
  return(df)
}

check_completion <- function(df) {
  completion_outcome <- df %>%
    select("Finished") %>%
    equals(TRUE) %>%
    all()

  if (completion_outcome != TRUE) {
    num_respondents <- df %>%
      filter(Finished != TRUE) %>%
      nrow()

    # dropping respondents who did not finish
    df <- df %>%
      filter(Finished == TRUE)

    warning(str_glue("Dropping {num_respondents} survey respondents who did not finish"))
    warning(str_glue("{nrow(df)} respondents in the data"))
  }
  return(df)
}

check_commitment <- function(df) {
  commitment_check1 <- df %>%
    distinct(Commitment_Q1) %>%
    equals("Yes, I will") %>%
    all()

  if (commitment_check1 != TRUE | is.na(commitment_check1)) {
    num_respondents <- df %>%
      filter(Commitment_Q1 != "Yes, I will") %>%
      nrow()

    df <- df %>%
      filter(Commitment_Q1 == "Yes, I will")

    warning(str_glue("Dropping {num_respondents} survey respondents did not pass commitment check 1"))
    warning(str_glue("{nrow(df)} respondents in the data"))
  }

  commitment_check2 <- df %>%
    mutate(Commitment_Q2 = str_to_lower(Commitment_Q2) %>% 
             str_replace_all('\\.', '')) %>%
    select(Commitment_Q2) %>%
    equals("purple") %>%
    all()

  if (commitment_check2 != TRUE | is.na(commitment_check2)) {
    num_respondents <- df %>%
      mutate(Commitment_Q2 = str_to_lower(Commitment_Q2) %>% 
               str_replace_all('\\.', '')) %>%
      filter(Commitment_Q2 != "purple") %>%
      nrow()

    df <- df %>%
      filter(str_to_lower(Commitment_Q2) %>% 
               str_replace_all('\\.', '') == "purple")

    warning(str_glue("Dropping {num_respondents} survey respondents who did not pass commitment check 2"))
    warning(str_glue("{nrow(df)} respondents in the data"))
  }
  return(df)
}

check_pi_vars <- function(df, probabilities, num_contexts) {
  comp_df <- df %>%
    select(batch_id, batch_type, all_of(str_c("pi", 1:num_contexts))) %>%
    distinct() %>%
    pivot_longer(-c(batch_id, batch_type),
      names_to = "Embedded data variable", values_to = "CDF_Data"
    ) %>%
    left_join(probabilities, by = c("Embedded data variable", "batch_id" = "Batch", "batch_type" = "Batch Type")) %>%
    mutate(Comparison = CDF_Threshold == CDF_Data)

  check_pi <- comp_df %>%
    pull(Comparison) %>%
    equals(TRUE) %>%
    all()

  if (check_pi == FALSE) {
    warning("Embedded data variables do not match probabilities in log")
    return(comp_df %>%
      filter(Comparison == FALSE))
  }
}

create_context_var_political <- function(df) {
  df %>%
    # create profile context variable
    mutate(
      context = case_when(
        rnum <= pi1 ~ 1,
        rnum > pi1 & rnum <= pi2 ~ 2,
        rnum > pi2 & rnum <= pi3 ~ 3,
        rnum > pi3 & rnum <= pi4 ~ 4,
        rnum > pi4 & rnum <= pi5 ~ 5,
        rnum > pi5 & rnum <= pi6 ~ 6,
        rnum > pi6 & rnum <= pi7 ~ 7,
        rnum > pi7 ~ 8,
        TRUE ~ NA_integer_
      ),
      # create profile context label
      context_label = case_when(
        context == 1 ~ "white_female_high",
        context == 2 ~ "white_female_low",
        context == 3 ~ "black_female_high",
        context == 4 ~ "black_female_low",
        context == 5 ~ "black_male_high",
        context == 6 ~ "black_male_low",
        context == 7 ~ "white_male_high",
        context == 8 ~ "white_male_low",
        TRUE ~ NA_character_
      ),
      context = factor(context,
        levels = c(1:8),
        ordered = TRUE
      )
    )
}

create_context_var_jobs <- function(df, pi) {
  df %>%
    # create 'conext' variable
    mutate(
      context = case_when(
        rnum <= pi1 ~ 1,
        rnum > pi1 & rnum <= pi2 ~ 2,
        rnum > pi2 & rnum <= pi3 ~ 3,
        rnum > pi3 ~ 4,
        TRUE ~ NA_integer_
        # need to add context label
      ),
      # create profile context label
      context_label = case_when(
        context == 1 ~ "black_low",
        context == 2 ~ "black_high",
        context == 3 ~ "white_low",
        context == 4 ~ "white_high",
        TRUE ~ NA_character_
      ),
      context = factor(context,
        levels = c(1:4),
        ordered = TRUE
      )
    ) %>%
    verify(!is.na(context))
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
