Process Qualtrics Data and Treatment Assignment Updating for Political
Candidates Survey
================
2023-11-07

# Setup Data Using Qualtrics API

- Load data directly from Qualtrics
- Can store API Key and credentials in `.renviron`

``` r
library(tidyverse)
library(qualtRics)
library(magrittr)
library(assertr)
library(RColorBrewer)

knitr::opts_chunk$set(cache.extra = 2023)

source("../_functions/data_cleaning.R")
config <- config::get()

# load log of probabilities
probabilities <- read_csv("../../02_output/probabilities_job_applicants.csv")
probabilities
```

    ## # A tibble: 3 × 4
    ##   Batch `Embedded data variable` CDF_Threshold `Batch Type`
    ##   <dbl> <chr>                            <dbl> <chr>       
    ## 1     0 pi1                               0.25 Warmup      
    ## 2     0 pi2                               0.5  Warmup      
    ## 3     0 pi3                               0.75 Warmup

``` r
url <- str_glue("https://{config$datacenter_id}.qualtrics.com")
survey_name <- "Job Applicants"

# can comment out after running once
# qualtrics_api_credentials(api_key = config$api_token,
#                           base_url = url,
#                           install = TRUE,
#                           overwrite=T)

df <- load_qualtrics(survey_name)
```

    ## Loading survey data for Job Applicants
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_character(),
    ##   StartDate = col_datetime(format = ""),
    ##   EndDate = col_datetime(format = ""),
    ##   Progress = col_double(),
    ##   `Duration (in seconds)` = col_double(),
    ##   Finished = col_logical(),
    ##   RecordedDate = col_datetime(format = ""),
    ##   QD2_1_TEXT = col_double(),
    ##   QD3_7 = col_logical(),
    ##   rnum = col_double(),
    ##   pi1 = col_double(),
    ##   pi2 = col_double(),
    ##   pi3 = col_double(),
    ##   `Create New Field or Choose From Dropdown...` = col_logical(),
    ##   rnum_mother = col_double(),
    ##   EmbeddedDataQuestions_DO_Q8 = col_double(),
    ##   EmbeddedDataQuestions_DO_Q7 = col_double(),
    ##   EmbeddedDataQuestions_DO_Q6 = col_double(),
    ##   EmbeddedDataQuestions_DO_Q5 = col_double(),
    ##   EmbeddedDataQuestions_DO_Q4 = col_double(),
    ##   EmbeddedDataQuestions_DO_Introduction = col_double()
    ##   # ... with 3 more columns
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
df %>%
  nrow()
```

    ## [1] 59

``` r
# drop test data
df <- df %>%
  mutate(StartDate_clean = ymd_hms(StartDate)) %>%
  verify(is.na(StartDate_clean) == is.na(StartDate)) %>% 
  filter(StartDate_clean >= ymd_hms("2024-01-15-00-00-00"))

df %>%
  nrow()
```

    ## [1] 50

``` r
# average finished rate
mean(df$Finished)
```

    ## [1] 1

``` r
# average time to complete
mean(df$`Duration (in seconds)`)
```

    ## [1] 138.8

``` r
# compare amount of time spent
df %>%
  summarize(across(c(`Duration (in seconds)`),
    .fns = lst(
      mean = ~ mean(.) / 60,
      min = ~ min(.) / 60,
      max = ~ max(.) / 60,
      median = ~ median(.) / 60
    )
  ))
```

    ## # A tibble: 1 × 4
    ##   `Duration (in seconds)_mean` Duration (in seconds)_mi…¹ Duration (in seconds…²
    ##                          <dbl>                      <dbl>                  <dbl>
    ## 1                         2.31                      0.767                   7.07
    ## # ℹ abbreviated names: ¹​`Duration (in seconds)_min`,
    ## #   ²​`Duration (in seconds)_max`
    ## # ℹ 1 more variable: `Duration (in seconds)_median` <dbl>

## Create Batch ID variable

``` r
# hardcoding this because it should not be changing each time
# we should only update it as we increase batches

# check max date times on each day
df %>% 
  mutate(date = date(StartDate_clean)) %>% 
  group_by(date) %>% 
  summarize(max_date_time = max(StartDate_clean))
```

    ## # A tibble: 2 × 2
    ##   date       max_date_time      
    ##   <date>     <dttm>             
    ## 1 2024-01-16 2024-01-16 23:59:02
    ## 2 2024-01-17 2024-01-17 00:45:20

``` r
df <- df %>%
  # drop data collected incorrectly
  mutate(
    batch_id = case_when(
      # NEED TO CHANGE
      StartDate_clean <= ymd_hms("2024-01-17-12-00-00") ~ 0,
      TRUE ~ NA_integer_
    ),
    batch_type = case_when(
      batch_id == 0 ~ "Warmup",
      TRUE ~ NA_character_
    )
  ) %>% 
  verify(!is.na(batch_id)) %>% 
  verify(!is.na(batch_type))
  
df %>%
  group_by(batch_type, batch_id) %>%
  summarize(n = n())
```

    ## `summarise()` has grouped output by 'batch_type'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 1 × 3
    ## # Groups:   batch_type [1]
    ##   batch_type batch_id     n
    ##   <chr>         <dbl> <int>
    ## 1 Warmup            0    50

## Survey Validation

``` r
# check ID uniquely identifies rows in data
if (length(unique(df$`Prolific ID Q`)) != nrow(df)) {
  warning("ID is not unique")
}

if (!all(unique(df$Status) == "IP Address")) {
  warning("Test/spam data included in the analysis file")
  print(str_glue("Number of observations in data: {nrow(df)}"))
  print(str_glue("Status values in data: {str_c(unique(df$Status), collapse=', ')}"))
  df <- df %>% 
    filter(Status == "IP Address")
  print(str_glue("Number of observations left in data: {nrow(df)}"))
}
```

``` r
# check embedded data variables match probabilities for all iterative batches
df %>%
  check_pi_vars(probabilities, 3)
```

``` r
# check distinct probabilities based on embedded data
df %>% 
  select(batch_type, batch_id, str_c("pi", 1:3)) %>% 
  distinct()
```

    ## # A tibble: 1 × 5
    ##   batch_type batch_id   pi1   pi2   pi3
    ##   <chr>         <dbl> <dbl> <dbl> <dbl>
    ## 1 Warmup            0  0.25   0.5  0.75

``` r
# check consent means their responses are missing
df %>%
  filter(Consent == "I do not consent to participate") %>% 
    select(Consent, `Duration (in seconds)`, Finished, batch_id, batch_type)
```

    ## # A tibble: 0 × 5
    ## # ℹ 5 variables: Consent <ord>, Duration (in seconds) <dbl>, Finished <lgl>,
    ## #   batch_id <dbl>, batch_type <chr>

``` r
df <- df %>%
  check_consent()
```

``` r
# check that all completed
df %>%
  filter(Finished != TRUE) %>% 
  select(StartDate_clean, EndDate, `Duration (in seconds)`, Finished, Consent, PreScreen_Q1:QD5)
```

    ## # A tibble: 0 × 27
    ## # ℹ 27 variables: StartDate_clean <dttm>, EndDate <dttm>,
    ## #   Duration (in seconds) <dbl>, Finished <lgl>, Consent <ord>,
    ## #   PreScreen_Q1 <ord>, Commitment_Q1 <ord>, Commitment_Q2 <chr>, Q1 <ord>,
    ## #   Q2 <ord>, Q3 <ord>, Q4 <ord>, Q5 <ord>, Q6 <ord>, Q7 <ord>, Q8 <ord>,
    ## #   QD2 <ord>, QD2_1_TEXT <dbl>, QD3_1 <chr>, QD3_2 <chr>, QD3_3 <chr>,
    ## #   QD3_4 <chr>, QD3_5 <chr>, QD3_6 <chr>, QD3_7 <lgl>, QD4 <ord>, QD5 <ord>

``` r
df <- df %>%
  check_completion()
```

``` r
df <- df %>%
  check_location_screen()
```

``` r
# visually assessing why these respondents failed the commitment check
df %>%
  filter(Commitment_Q1 != "Yes, I will") %>% 
    select(Commitment_Q1, Commitment_Q2, `Duration (in seconds)`, Finished, batch_id, batch_type)
```

    ## # A tibble: 0 × 6
    ## # ℹ 6 variables: Commitment_Q1 <ord>, Commitment_Q2 <chr>,
    ## #   Duration (in seconds) <dbl>, Finished <lgl>, batch_id <dbl>,
    ## #   batch_type <chr>

``` r
df %>%
  filter(str_to_lower(Commitment_Q2) != "purple") %>% 
    select(Commitment_Q1, Commitment_Q2, `Duration (in seconds)`, Finished, batch_id, batch_type)
```

    ## # A tibble: 0 × 6
    ## # ℹ 6 variables: Commitment_Q1 <ord>, Commitment_Q2 <chr>,
    ## #   Duration (in seconds) <dbl>, Finished <lgl>, batch_id <dbl>,
    ## #   batch_type <chr>

``` r
df <- df %>%
  check_commitment()
```

``` r
# check ID is unique again
if (length(unique(df$`Prolific ID Q`)) != nrow(df)) {
  warning("ID is not unique")
  print(str_glue("Number of rows in data: {nrow(df)}"))
  df <- df %>% 
    group_by(`Prolific ID Q`) %>% 
    arrange(`Prolific ID Q`, StartDate_clean) %>% 
    mutate(flag_first_obs = if_else(row_number() == 1, 1, 0)) %>% 
    ungroup()
  
  df %>% 
    group_by(`Prolific ID Q`) %>% 
    mutate(n = n()) %>% 
    arrange(desc(n), StartDate_clean) %>% 
    ungroup() %>% 
    select(n, StartDate_clean, EndDate, flag_first_obs, `Duration (in seconds)`, Finished, batch_id, batch_type) %>% 
    print()
  
  df <- df %>% 
    filter(flag_first_obs == 1)
  
  print(str_glue("Number of rows in data after dropping duplicate ID: {nrow(df)}"))
}

stopifnot(length(unique(df$`Prolific ID Q`)) == nrow(df))
```

## Create Context and Context Label Variables

``` r
# create context variable
df <- df %>%
  create_context_var_jobs() %>% 
  verify(!is.na(context)) # %>% 
  # verify(!is.na(context_label))

df %>%
  group_by(batch_type, context) %>%
  summarize(n = n())
```

    ## `summarise()` has grouped output by 'batch_type'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 4 × 3
    ## # Groups:   batch_type [1]
    ##   batch_type context     n
    ##   <chr>      <ord>   <int>
    ## 1 Warmup     1          17
    ## 2 Warmup     2           9
    ## 3 Warmup     3          15
    ## 4 Warmup     4           9

``` r
df %>%
  group_by(batch_type, batch_id) %>%
  summarize(n = n())
```

    ## `summarise()` has grouped output by 'batch_type'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 1 × 3
    ## # Groups:   batch_type [1]
    ##   batch_type batch_id     n
    ##   <chr>         <dbl> <int>
    ## 1 Warmup            0    50

``` r
df %>%
  group_by(context, batch_type, batch_id) %>%
  filter(batch_type %in% c("Warmup", "Iterative Batch Phase: Max")) %>% 
  summarize(n = n()) %>%
  group_by(batch_type, batch_id) %>% 
  mutate(n = n/sum(n)) %>% 
  ggplot(aes(context, n)) +
  geom_col() +
  theme_classic() +
  facet_wrap(~batch_id)  +
  labs(title = "Sample Sizes in Iterative Batch Phase: Max",
       y = "Share of total")
```

    ## `summarise()` has grouped output by 'context', 'batch_type'. You can override
    ## using the `.groups` argument.

![](00_get_qualtrics_data_job_applicants_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
df %>%
  group_by(context, batch_type, batch_id) %>%
  filter(batch_type %in% c("Warmup", "Iterative Batch Phase: Min")) %>% 
  summarize(n = n()) %>%
  group_by(batch_type, batch_id) %>% 
  mutate(n = n/sum(n)) %>%
  ggplot(aes(context, n)) +
  geom_col() +
  theme_classic() +
  facet_wrap(~batch_id) +
  labs(title = "Sample Sizes in Iterative Batch Phase: Min",
       y = "Share of total")
```

    ## `summarise()` has grouped output by 'context', 'batch_type'. You can override
    ## using the `.groups` argument.

![](00_get_qualtrics_data_job_applicants_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
df %>%
  group_by(context, batch_type, batch_id) %>%
  filter(batch_type %in% c("Warmup", "Iterative Batch Phase: Max")) %>% 
  summarize(n = n()) %>%
  group_by(batch_id, batch_type) %>% 
  arrange(context) %>% 
  mutate(n = cumsum(n)/sum(n))  %>% 
  ggplot(aes(context, n)) +
  geom_col() +
  theme_classic() +
  facet_wrap(~batch_id)  +
  labs(title = "Cumulative Sample Sizes in Iterative Batch Phase: Max")
```

    ## `summarise()` has grouped output by 'context', 'batch_type'. You can override
    ## using the `.groups` argument.

![](00_get_qualtrics_data_job_applicants_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
df %>%
  group_by(context, batch_type, batch_id) %>%
  filter(batch_type %in% c("Warmup", "Iterative Batch Phase: Min")) %>% 
  summarize(n = n()) %>%
  group_by(batch_id, batch_type) %>% 
  arrange(context) %>% 
  mutate(n = cumsum(n)/sum(n))  %>% 
  ggplot(aes(context, n)) +
  geom_col() +
  theme_classic() +
  facet_wrap(~batch_id)  +
  labs(title = "Cumulative Sample Sizes in Iterative Batch Phase: Min")
```

    ## `summarise()` has grouped output by 'context', 'batch_type'. You can override
    ## using the `.groups` argument.

![](00_get_qualtrics_data_job_applicants_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
df  %>%
  filter(batch_type %in% c("Warmup", "Iterative Batch Phase: Max")) %>%
  group_by(batch_id, batch_type) %>%
  select(str_c("pi", 1:3)) %>% 
  distinct() %>% 
  pivot_longer(-c(batch_id, batch_type)) %>% 
  mutate(context = str_replace_all(name, "pi", "")) %>%
  ggplot(aes(context, value)) +
  geom_col() +
  theme_classic() +
  facet_wrap(~batch_id)
```

    ## Adding missing grouping variables: `batch_id`, `batch_type`

![](00_get_qualtrics_data_job_applicants_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
df  %>%
  filter(batch_type %in% c("Warmup", "Iterative Batch Phase: Min")) %>% 
  group_by(batch_id, batch_type) %>%
  select(str_c("pi", 1:3)) %>% 
  distinct() %>% 
  pivot_longer(-c(batch_id, batch_type)) %>% 
  mutate(context = str_replace_all(name, "pi", "")) %>%
  ggplot(aes(context, value)) +
  geom_col() +
  theme_classic() +
  facet_wrap(~batch_id) +
  labs(title = "CDF for Iterative Batch Phase: Min",
       y = "Cumulative Probability")
```

    ## Adding missing grouping variables: `batch_id`, `batch_type`

![](00_get_qualtrics_data_job_applicants_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

## Clean Qualtrics Data

``` r
df_clean <- create_outcome_var_jobs(df)

# each question number is the random ordering of the context attributes
df_clean %>%
  select(chose_mother, str_c("Q", 1:8), context) %>%
  verify(!is.na(chose_mother))
```

    ## # A tibble: 50 × 10
    ##    chose_mother    Q1    Q2    Q3    Q4    Q5    Q6    Q7    Q8 context
    ##           <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <ord>  
    ##  1            1     1    NA    NA    NA    NA    NA    NA    NA 1      
    ##  2            0    NA    NA     0    NA    NA    NA    NA    NA 1      
    ##  3            1    NA    NA    NA    NA    NA    NA    NA     1 3      
    ##  4            1    NA    NA    NA    NA    NA    NA    NA     1 4      
    ##  5            1    NA    NA    NA    NA    NA    NA    NA     1 4      
    ##  6            0    NA     0    NA    NA    NA    NA    NA    NA 4      
    ##  7            1     1    NA    NA    NA    NA    NA    NA    NA 2      
    ##  8            1    NA    NA    NA    NA    NA    NA    NA     1 2      
    ##  9            1    NA     1    NA    NA    NA    NA    NA    NA 3      
    ## 10            1    NA    NA    NA    NA    NA     1    NA    NA 3      
    ## # ℹ 40 more rows

``` r
df_clean %>%
  group_by(context) %>%
  summarize(
    n = n(),
    resp1 = sum(chose_mother),
    resp0 = sum(chose_mother == 0)
  )
```

    ## # A tibble: 4 × 4
    ##   context     n resp1 resp0
    ##   <ord>   <int> <dbl> <int>
    ## 1 1          17     7    10
    ## 2 2           9     4     5
    ## 3 3          15     7     8
    ## 4 4           9     6     3

``` r
# identify context desc
df_clean %>%
  select(context, context_label, starts_with("name"), starts_with("education")) %>%
  distinct() %>%
  arrange(context)
```

    ## # A tibble: 4 × 6
    ##   context context_label name1           name2            education1   education2
    ##   <ord>   <chr>         <chr>           <chr>            <chr>        <chr>     
    ## 1 1       black_low     Tanisha Rivers  Keisha Mosely    B.S. in Bus… B.A. in M…
    ## 2 2       black_high    Tanisha Rivers  Keisha Mosely    B.S. in Bus… B.A. in M…
    ## 3 3       white_low     Jessica Schmitt Ashley O’Connell B.S. in Bus… B.A. in M…
    ## 4 4       white_high    Jessica Schmitt Ashley O’Connell B.S. in Bus… B.A. in M…

``` r
# no manipulation question!!
```

``` r
df_clean <- df_clean %>%
  verify(is.numeric(QD2_1_TEXT)) %>%
  rename(age = QD2_1_TEXT) %>%
  mutate(
    hispanic = if_else(QD4 == "Yes", TRUE, FALSE),
    female = if_else(QD5 == "Female", TRUE, FALSE)
  ) %>%
  mutate(across(starts_with("QD3_"), .fns = lst(race_num = ~ if_else(!is.na(.), 1, 0)))) %>%
  mutate(
    race_count = rowSums(select(., ends_with("race_num"))),
    race = case_when(
      race_count > 1 ~ "Multiracial",
      !is.na(QD3_1) ~ "American Indian or Alaskan Native",
      !is.na(QD3_2) ~ "Asian",
      !is.na(QD3_3) ~ "Black or African American",
      !is.na(QD3_4) ~ "Native Hawaiian",
      !is.na(QD3_5) ~ "White",
      !is.na(QD3_6) ~ "Other",
      !is.na(QD3_7) ~ "Prefer not to disclose"
    )
  ) %>%
  verify(!is.na(race))

df_clean %>%
  group_by(race) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(per = n / sum(n)) %>%
  arrange(desc(per))
```

    ## # A tibble: 6 × 3
    ##   race                                  n   per
    ##   <chr>                             <int> <dbl>
    ## 1 White                                30  0.6 
    ## 2 Asian                                10  0.2 
    ## 3 Black or African American             6  0.12
    ## 4 Multiracial                           2  0.04
    ## 5 American Indian or Alaskan Native     1  0.02
    ## 6 Other                                 1  0.02

``` r
df_clean %>%
  ggplot(aes(age)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](00_get_qualtrics_data_job_applicants_files/figure-gfm/demog-1.png)<!-- -->

``` r
df_clean %>%
  group_by(female) %>%
  summarize(count = n())
```

    ## # A tibble: 2 × 2
    ##   female count
    ##   <lgl>  <int>
    ## 1 FALSE     25
    ## 2 TRUE      25

``` r
df_clean %>%
  summarize(
    count_hispanic = sum(hispanic),
    per_hispanic = mean(hispanic)
  )
```

    ## # A tibble: 1 × 2
    ##   count_hispanic per_hispanic
    ##            <int>        <dbl>
    ## 1              6         0.12

## Clean Data Validation

``` r
# check every respondent has exactly one non-missing value
check_allmissing <- df_clean %>%
  select(str_c("Q", 1:8)) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  is.na() %>%
  rowSums(na.rm = T) %>%
  equals(8) %>%
  any()

if (check_allmissing == TRUE) {
  warning("Some rows are missing outcome responses\n")
}

# check row total is not more than 1
# 0 if selected the older candidate
check_rowtotal <- df_clean %>%
  select(str_c("Q", 1:8)) %>%
  rowSums(na.rm = T) %>%
  is_weakly_less_than(1) %>%
  all()

if (check_rowtotal == FALSE) {
  warning("Total of outcome responses is more than 1\n")
}
```

``` r
# create ID with a random ordering
df_clean <- df_clean %>% 
  mutate(rand_sort = runif(nrow(df_clean))) %>% 
  arrange(batch_id, rand_sort) %>% 
  mutate(unique_id = row_number()) %>% 
  select(-rand_sort)
```

``` r
df_clean %>%
  select(unique_id, context, context_label, batch_id, batch_type, chose_mother, race, age, female, hispanic) %>%
  saveRDS("../../02_output/job_applicants_data_clean.RDS")

df_clean %>%
  select(unique_id, context, context_label, batch_id, batch_type, chose_mother, race, age, female, hispanic) %>%
  write_csv("../../02_output/job_applicants_data_clean.csv")
```
