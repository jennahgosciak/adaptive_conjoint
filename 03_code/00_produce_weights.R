# Code to prepare population cell sizes for post-stratification
# using ACS microdata

library(tidyverse)
library(ipumsr)

file <- file("./_logs/00_produce_weights.txt", open = "wt")
sink(file, type = "output")
sink(file, type = "message")

config <- config::get()
# set_ipums_api_key(config$ipums_usa_api_key, save = TRUE)

cat("\nLoading data from IPUMS\n")
# define vars in the extract
usa_ext_def <- define_extract_usa(
  description = "Extract for adaptive conjoint experiment",
  samples = c("us2022a"),
  variables = c("AGE", "SEX", "RACE", "STATEFIP", "HISPAN")
)

# check varnames
names(usa_ext_def)

# submit extract and wait for it to become ready
usa_ext_submitted <- submit_extract(usa_ext_def)
usa_ext_complete <- wait_for_extract(usa_ext_submitted)
usa_ext_complete$status

# download extract and save in the input folder
filepath <- download_extract(usa_ext_submitted,
  download_dir = "./_data_private/"
)
ddi <- read_ipums_ddi(filepath)
micro_data <- read_ipums_micro(ddi)

if (typeof(micro_data$AGE) != "integer") {
  warning("Age variable is not an integer")
}

cat("\nMin, max values of age\n")
min(micro_data$AGE)
max(micro_data$AGE)

# codebook for ipums detailed race data
# https://usa.ipums.org/usa-action/variables/RACE#codes_section
df_race_form <- micro_data %>%
  # create race vars in ipums data to match acs
  mutate(
    race = case_when(
      RACE == 1 ~ "White",
      RACE == 2 ~ "Black or African American",
      RACE == 3 ~ "American Indian or Alaska Native",
      RACED %in% c(
        400, 410, 420, 500, 600, 610, 620, 640,
        641, 642, 643, 660, 661, 662, 663, 664,
        665, 666, 667, 669, 670, 671, 673, 674,
        675, 676, 677, 678, 679
      ) ~ "Asian",
      # A person having origins in any of the original peoples of Hawaii,
      # Guam, Samoa, or other Pacific Islands. It includes people who
      # indicate their race as “Native Hawaiian,” “Chamorro,”
      # “Samoan,” and “Other Pacific Islander”
      # or provide other detailed Pacific Islander responses such as
      # Palauan, Tahitian, Chuukese, Pohnpeian, Saipanese, Yapese, etc.
      RACED %in% c(
        630, 680, 682, 685, 689, 690, 698, 699
      ) ~ "Native Hawaiian or Other Pacific Islander",
      RACE == 7 ~ "Other",
      RACE %in% c(8, 9) ~ "Two or More Races",
      TRUE ~ NA_character_
    ),
    race = factor(race, levels = c(
      "Black or African American",
      "White", "American Indian or Alaska Native",
      "Asian", "Native Hawaiian or Other Pacific Islander",
      "Other", "Two or More Races", "Prefer not to disclose"
    ))
  ) %>%
  verify(!is.na(race))

# check not missing (i.e., enumerated all categories)
stopifnot(sum(is.na(df_race_form$race)) == 0)

# check mapping from race (general) to new race var
cat("\nDistinct values of RACE variable\n")
df_race_form %>%
  distinct(RACE, race) %>%
  arrange(RACE) %>%
  table()

# recode/fix other variables in ipums microdata
cat("\nDistinct values of SEX variable\n")
df_race_form %>%
  distinct(SEX)

cat("\nDistinct values of HISPAN variable\n")
df_race_form %>%
  distinct(HISPAN)

df_wgt <- df_race_form %>%
  # create numeric age variable
  mutate(age = as.numeric(AGE)) %>%
  assertr::verify(!is.na(age)) %>%
  filter(age >= 18) %>%
  mutate_if(is.labelled, as_factor) %>%
  # create hispanic or latino variable
  mutate(hispanic = case_when(
    HISPAN == "Not Hispanic" ~ FALSE,
    HISPAN %in% c(
      "Mexican", "Other",
      "Puerto Rican", "Cuban"
    ) ~ TRUE
  )) %>%
  assertr::verify(!is.na(hispanic)) %>%
  # create female variable from sex
  mutate(female = if_else(SEX == "Female", TRUE, FALSE)) %>%
  verify(is.na(female) == is.na(SEX)) %>%
  group_by(race, female, hispanic, age) %>%
  summarize(
    weight = sum(PERWT),
    num = n(),
    .groups = "drop"
  ) %>%
  mutate(weight = weight / sum(weight))

df_wgt

saveRDS(df_wgt, "./00_data/ipums_strata_sizes.RDS")
write_csv(df_wgt, "./00_data/ipums_strata_sizes.csv")

# checking age requirements
# must at least 18 years old or older
cat("\nAge cutoffs\n")
df_wgt %>%
  mutate(age = as.numeric(age)) %>%
  summarize(across(age, .fns = lst(~ min(.), ~ max(.))))

cat("\nWeights by race\n")
df_wgt %>%
  group_by(race) %>%
  summarize(weight = sum(weight)) %>%
  arrange(desc(weight))

# add plot
df_wgt %>%
  group_by(race) %>%
  summarize(weight = sum(weight)) %>%
  arrange(desc(weight)) %>%
  ggplot(aes(
    x = reorder(race, weight),
    y = weight, label = format(round(weight, 2), nsmall = 2)
  )) +
  geom_bar(stat = "identity") +
  geom_text(hjust = -.2) +
  coord_flip() +
  labs(x = "Race", y = "Weight") +
  ylim(c(0, 1))
ggsave("02_output/_figures/ipums_weights_by_race.png", width = 7)

sink()
