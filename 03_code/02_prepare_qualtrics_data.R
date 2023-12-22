df_clean <- clean_political_data(df_survey)

# each question number is the random ordering of the context attributes
df_clean %>%
  select(chose_younger, str_c("Q", 1:8)) %>%
  verify(!is.na(chose_younger))

# create cleaned demographic variables
df_clean %>%
  mutate(older_candidate = if_else(rnum_age <= 0.5, "Candidate 2", "Candidate 1")) %>%
  mutate(pass_attention_check = if_else(Manipulation_Q1 == older_candidate, 1, 0)) %>%
  summarize(per_pass_attention_check = mean(pass_attention_check))

df_clean %>%
  group_by(QD5) %>%
  summarize(count = n())

df_demo <- df_clean %>%
  mutate(hispanic_latino = if_else(QD4 == "Yes", 1, 0)) %>%
  mutate(female = if_else(QD5 == 'Female', 1, 0)) %>% 
  mutate(age = QD2_1_TEXT) %>%
  mutate(across(starts_with("QD3_"), .fns = lst(race_num = ~ if_else(!is.na(.), 1, 0)))) %>%
  mutate(
    race_count = rowSums(select(., ends_with("race_num"))),
    race = case_when(
      race_count > 1 ~ "Two or More Races",
      !is.na(QD3_1) ~ "American Indian or Alaska Native",
      !is.na(QD3_2) ~ "Asian",
      !is.na(QD3_3) ~ "Black or African American",
      !is.na(QD3_4) ~ "Native Hawaiian or Other Pacific Islander",
      !is.na(QD3_5) ~ "White",
      !is.na(QD3_6) ~ "Other",
      !is.na(QD3_7) ~ "Prefer not to disclose"
    ),
    race = factor(race, levels = c("Black or African American",
                                   "White", "American Indian or Alaska Native",
                                   "Asian", "Native Hawaiian or Other Pacific Islander",
                                   "Other", "Two or More Races", "Prefer not to disclose"))
  ) %>% 
  verify(!is.na(race)) %>% 
  mutate(race_white = if_else(race == 'White', 1, 0),
         race_black = if_else(race == 'Black or African American', 1, 0),
         race_aian = if_else(race == 'American Indian or Alaska Native', 1, 0),
         race_asian = if_else(race == 'Asian', 1, 0),
         race_nhpi = if_else(race == 'Native Hawaiian or Other Pacific Islander',1, 0),
         race_other = if_else(race == 'Other', 1, 0),
         race_multi = if_else(race == 'Two or More Races', 1, 0))

df_demo %>% 
  summarize(
    count_hispanic_latino = sum(hispanic_latino),
    per_hispanic_latino = mean(hispanic_latino)
  )

df_demo %>%
  ggplot(aes(age)) +
  geom_histogram()

