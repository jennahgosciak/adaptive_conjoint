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

create_outcome_var <- function(df) {
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
    mutate(Commitment_Q2 = str_to_lower(Commitment_Q2)) %>%
    select(Commitment_Q2) %>%
    equals("purple") %>%
    all()
  
  if (commitment_check2 != TRUE | is.na(commitment_check2)) {
    num_respondents <- df %>%
      mutate(Commitment_Q2 = str_to_lower(Commitment_Q2)) %>%
      filter(Commitment_Q2 != "purple") %>%
      nrow()
    
    df <- df %>%
      filter(str_to_lower(Commitment_Q2) == "purple")
    
    warning(str_glue("Dropping {num_respondents} survey respondents who did not pass commitment check 2"))
    warning(str_glue("{nrow(df)} respondents in the data"))
  }
  return(df)
}

create_context_var <- function(df) {
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
        rnum > pi7 ~ 8
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
        context == 8 ~ "white_male_low"
      ),
      context = factor(context,
        levels = c(1:8),
        ordered = TRUE
      )
    )
}
