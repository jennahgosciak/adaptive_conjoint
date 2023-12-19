
# Code to prepare population cell sizes for post-stratification
# using ACS data

library(tidyverse)
library(tidycensus)

config <- config::get()
census_api_key(config$census_api_key, overwrite=TRUE)

# load data dictionary
data_dict <- readxl::read_excel('./01_input/ACS2022_Table_Shells.xlsx')

# filter data dictionary to the vars we want
filtered_data_dict <- data_dict %>% 
  filter(`Table ID` %in% c("B01001A", "B01001B", "B01001C", "B01001D", "B01001E",
                           "B01001F", "B01001G", "B01001H", "B01001I")) %>%
  mutate(prefix_data = case_when(`Table ID` == 'B01001A' ~ 'whitealone_',
                                 `Table ID` == 'B01001B' ~ 'black_',
                                 `Table ID` == 'B01001C' ~ 'aian_',
                                 `Table ID` == 'B01001D' ~ 'asian_',
                                 `Table ID` == 'B01001E' ~ 'nhpi_',
                                 `Table ID` == 'B01001F' ~ 'other_',
                                 `Table ID` == 'B01001G' ~ 'multi_',
                                 `Table ID` == 'B01001H' ~ 'whitenothispanic_',
                                 `Table ID` == 'B01001I' ~ 'hisp_'),
         sex = case_when(Line %in% c(3:16) ~ 'male_',
                         Line %in% c(17:31) ~ 'female_'),
         age_desc = Stub %>% 
           str_to_lower() %>% 
           str_replace_all(' ', '')) %>% 
  filter(!is.na(sex)) %>%
  mutate(varname = str_c(prefix_data, sex, age_desc))

# select and name vars
vars <- filtered_data_dict$UniqueID
names(vars) <- filtered_data_dict$varname

# load acs data using the api
df_acs <- get_acs(geography = "us", 
                  year = 2022,
                  variables = vars)

# separate variables names
df_acs_strata <- df_acs %>% 
  separate(variable, c("race", "sex", "age")) %>% 
  select(race, sex, age, estimate)

df_acs_strata %>% 
  write_csv('./02_output/acs_strata_sizes.csv')