
# Code to prepare population cell sizes for post-stratification
# using ACS data

library(tidyverse)
library(ipumsr)

config <- config::get()
#set_ipums_api_key(config$ipums_usa_api_key, save = TRUE)

# define vars in the extract
usa_ext_def <- define_extract_usa(
  description = "USA extract for API vignette",
  samples = c("us2022a"),
  variables = c("AGE", "SEX", "RACE", "STATEFIP", "HISPAN")
)

# check varnames
names(usa_ext_def)

usa_ext_submitted <- submit_extract(usa_ext_def)
usa_ext_complete <- wait_for_extract(usa_ext_submitted)

usa_ext_complete$status

# download extract
filepath <- download_extract(usa_ext_submitted,
                             download_dir = './01_input/')
ddi <- read_ipums_ddi(filepath)
micro_data <- read_ipums_micro(ddi)

# codebook for ipums detailed race data
# https://usa.ipums.org/usa-action/variables/RACE#codes_section
df_race_form <- micro_data %>% 
  # create race vars in ipums data to match acs
  mutate(race_acs = case_when(RACE == 1 ~ 'white',
                              RACE == 2 ~ "Black or African American",
                              RACE == 3 ~ "American Indian or Alaska Native",
                              RACED %in% c(400, 410, 420, 500, 600, 610, 620, 
                                           640, 641, 642, 643, 660, 661, 662, 663,
                                           664, 665, 666, 667, 668, 669, 670, 671, 672, 673,
                                           674, 675, 676, 677, 678, 679) ~ 'asian',
                              # A person having origins in any of the original peoples of Hawaii, 
                              # Guam, Samoa, or other Pacific Islands. It includes people who 
                              # indicate their race as “Native Hawaiian,” “Chamorro,” 
                              # “Samoan,” and “Other Pacific Islander” 
                              # or provide other detailed Pacific Islander responses such as 
                              # Palauan, Tahitian, Chuukese, Pohnpeian, Saipanese, Yapese, etc.
                              RACED %in% c(630, 680, 681, 682, 683, 684, 685, 686, 687, 688,
                                           689, 690, 691, 692, 698, 699, 700) ~ "Native Hawaiian or Other Pacific Islander",
                              RACE == 7 ~ 'other',
                              RACE %in% c(8,9) ~ 'multi',
                              TRUE ~ NA_character_))

# check not missing (i.e., enumerated all categories)
stopifnot(sum(is.na(df_race_form$race_acs)) == 0)

# check mapping from race (general) to new race var
df_race_form %>% 
  distinct(RACE, race_acs) %>% 
  arrange(RACE) %>% 
  table()

# recode/fix other variables in ipums microdata
df_race_form %>% 
  distinct(SEX)

df_race_form %>% 
  distinct(HISPAN)

df_wgt <- df_race_form  %>% 
  mutate_if(is.labelled, as_factor) %>% 
  mutate(hispanic = case_when(HISPAN == 'Not Hispanic' ~ FALSE,
                              HISPAN %in% c('Mexican', 'Other', 
                                            'Puerto Rican', 'Cuban') ~ TRUE)) %>%
  assertr::verify(!is.na(hispanic)) %>% 
  rename(age = AGE,
         sex = SEX) %>% 
  group_by(race_acs, sex, hispanic, age) %>%
  summarize(weight = sum(PERWT),
            num = n(),
            .groups = "drop") %>%
  mutate(weight = weight / sum(weight))

write_csv(df_wgt, "./02_output/ipums_strata_sizes.csv")
