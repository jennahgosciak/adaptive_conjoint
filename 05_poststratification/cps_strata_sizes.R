
# Code to prepare population cell sizes for post-stratification

library(tidyverse)
library(haven)

data <- read_dta("../data/cps_00076.dta")

aggregated <- data %>%
  mutate_if(is.labelled, as_factor) %>%
  # Collapse racial categories to the ones we collect in our survey
  mutate(race = case_when(race == "american indian/aleut/eskimo" ~ "American Indian or Alaska Native",
                          race == "asian only" ~ "Asian",
                          race == "black" ~ "Black or African American",
                          race == "hawaiian/pacific islander only" ~ "Native Hawaiian or Other Pacific Islander",
                          race == "white" ~ "White",
                          T ~ "Other"),
         sex = case_when(sex == "female" ~ "Female",
                         sex == "male" ~ "Male"),
         hispanic = case_when(hispan == "not hispanic" ~ F,
                              !is.na(hispan) ~ T)) %>%
  group_by(race, sex, hispanic, age) %>%
  summarize(weight = sum(asecwt),
            num = n(),
            .groups = "drop") %>%
  mutate(weight = weight / sum(weight))

write_csv(aggregated, file = "../data/cps_strata_sizes.csv")

aggregated %>%
  group_by(race) %>%
  summarize(weight = sum(weight)) %>%
  mutate(race = fct_reorder(race, weight)) %>%
  ggplot(aes(x = race, y = weight, label = format(round(weight,2),nsmall=2))) +
  geom_bar(stat = "identity") +
  geom_text(hjust = -.2) +
  coord_flip() +
  ylim(c(0,1))
ggsave("../figures/cps_race.pdf",
       height = 4, width = 5)

aggregated %>%
  group_by(sex) %>%
  summarize(weight = sum(weight)) %>%
  ggplot(aes(x = sex, y = weight, label = format(round(weight,2),nsmall=2))) +
  geom_bar(stat = "identity") +
  geom_text(hjust = -.2) +
  coord_flip() +
  ylim(c(0,1))
ggsave("../figures/cps_sex.pdf",
       height = 4, width = 5)

aggregated %>%
  group_by(hispanic) %>%
  summarize(weight = sum(weight)) %>%
  ggplot(aes(x = hispanic, y = weight, label = format(round(weight,2),nsmall=2))) +
  geom_bar(stat = "identity") +
  geom_text(hjust = -.2) +
  coord_flip() +
  ylim(c(0,1))
ggsave("../figures/cps_hispanic.pdf",
       height = 4, width = 5)

aggregated %>%
  group_by(age) %>%
  summarize(weight = sum(weight)) %>%
  ggplot(aes(x = age, y = weight, label = format(round(weight,2),nsmall=2))) +
  geom_bar(stat = "identity") +
  geom_text(hjust = -.2) +
  coord_flip() +
  ylim(c(0,.05))
ggsave("../figures/cps_age.pdf",
       height = 10, width = 2)

