# Code to prepare population cell sizes for post-stratification
# using ACS microdata

library(askpass)
library(dplyr)
library(here)
library(ipumsr)

set_ipums_api_key(api_key = askpass("Please enter your API key"), save = FALSE)

# Define variables in the extract
usa_ext_def <- define_extract_micro(
  collection = "usa",
  description = "Extract for adaptive conjoint experiment",
  samples = c("us2022a"),
  variables = c("AGE", "SEX", "RACE", "STATEFIP", "HISPAN")
)

# Submit extract and wait for it to become ready
usa_ext_submitted <- submit_extract(usa_ext_def)
usa_ext_complete <- wait_for_extract(usa_ext_submitted)
usa_ext_complete$status

# Download extract and save it folder
filepath <- download_extract(usa_ext_submitted, download_dir = tempdir())
ddi <- read_ipums_ddi(filepath)
micro_data <- read_ipums_micro(ddi)

if (typeof(micro_data$AGE) != "integer") {
  warning("Age variable is not an integer")
}
cat("\nMin, max values of age\n")
min(micro_data$AGE)
max(micro_data$AGE)

# Codebook for ipums detailed race data
# https://usa.ipums.org/usa-action/variables/RACE#codes_section
df_race_form <- micro_data |>
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
  )

# Drop any missing race entries (should be extremely minimal)
df_race_form <- df_race_form |> filter(!is.na(race))


# check mapping from race (general) to new race var
cat("\nDistinct values of RACE variable\n")
df_race_form |>
  distinct(RACE, race) |>
  arrange(RACE) |>
  table()

# recode/fix other variables in ipums microdata
cat("\nDistinct values of SEX variable\n")
df_race_form |>
  distinct(SEX)

cat("\nDistinct values of HISPAN variable\n")
df_race_form |>
  distinct(HISPAN)

df_wgt <- df_race_form |>
  # create numeric age variable
  mutate(age = as.numeric(AGE)) |>
  filter(age >= 18) |>
  mutate_if(is.labelled, as_factor) |>
  # create hispanic or latino variable
  mutate(hispanic = case_when(
    HISPAN == "Not Hispanic" ~ FALSE,
    HISPAN %in% c(
      "Mexican", "Other",
      "Puerto Rican", "Cuban"
    ) ~ TRUE
  )) |>
  # create female variable from sex
  mutate(female = if_else(SEX == "Female", TRUE, FALSE)) |>
  group_by(race, female, hispanic, age) |>
  summarize(
    weight = sum(PERWT),
    num = n(),
    .groups = "drop"
  ) |>
  mutate(weight = weight / sum(weight))

saveRDS(df_wgt, here("data", "ipums_strata_sizes.RDS"))
