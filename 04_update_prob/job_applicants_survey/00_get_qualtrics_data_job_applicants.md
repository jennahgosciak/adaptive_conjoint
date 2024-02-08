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

    ## # A tibble: 6 × 4
    ##   Batch `Embedded data variable` CDF_Threshold `Batch Type`              
    ##   <dbl> <chr>                            <dbl> <chr>                     
    ## 1     0 pi1                              0.25  Warmup                    
    ## 2     0 pi2                              0.5   Warmup                    
    ## 3     0 pi3                              0.75  Warmup                    
    ## 4     1 pi1                              0.401 Iterative Batch Phase: Max
    ## 5     1 pi2                              0.734 Iterative Batch Phase: Max
    ## 6     1 pi3                              0.805 Iterative Batch Phase: Max

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

    ## [1] 93

``` r
# drop test data
df <- df %>%
  mutate(StartDate_clean = ymd_hms(StartDate)) %>%
  verify(is.na(StartDate_clean) == is.na(StartDate)) %>%
  filter(StartDate_clean >= ymd_hms("2024-02-08-00-00-00"))

df %>%
  nrow()
```

    ## [1] 23

``` r
# average finished rate
mean(df$Finished)
```

    ## [1] 1

``` r
# average time to complete
mean(df$`Duration (in seconds)`)
```

    ## [1] 185.6957

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
    ## 1                         3.09                       0.15                   18.4
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

    ## # A tibble: 1 × 2
    ##   date       max_date_time      
    ##   <date>     <dttm>             
    ## 1 2024-02-08 2024-02-08 14:41:26

``` r
df <- df %>%
  # drop data collected incorrectly
  mutate(
    batch_id = case_when(
      # NEED TO CHANGE
      StartDate_clean <= ymd_hms("2024-02-09-00-00-00") ~ 0,
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
    ## 1 Warmup            0    23

## Survey Validation

``` r
# check ID uniquely identifies rows in data
if (length(unique(df$`Prolific ID Q`)) != nrow(df)) {
  warning("ID is not unique")
}
```

    ## Warning: ID is not unique

``` r
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

    ## # A tibble: 1 × 5
    ##   Consent                    Duration (in seconds…¹ Finished batch_id batch_type
    ##   <ord>                                       <dbl> <lgl>       <dbl> <chr>     
    ## 1 I do not consent to parti…                      9 TRUE            0 Warmup    
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`

``` r
df <- df %>%
  check_consent()
```

    ## Warning in check_consent(.): Dropping 1 survey respondents who do not consent

    ## Warning in check_consent(.): 1 survey respondents who do not consent with
    ## non-missing responses

    ## Warning in check_consent(.): 22 respondents in the data

``` r
# check that all completed
df %>%
  filter(Finished != TRUE) %>%
  select(StartDate_clean, EndDate, `Duration (in seconds)`, Finished, Consent, PreScreen_Q1:QD5)
```

    ## # A tibble: 0 × 32
    ## # ℹ 32 variables: StartDate_clean <dttm>, EndDate <dttm>,
    ## #   Duration (in seconds) <dbl>, Finished <lgl>, Consent <ord>,
    ## #   PreScreen_Q1 <ord>, Commitment_Q1 <ord>, Commitment_Q2 <chr>, Q1 <ord>,
    ## #   Q2 <ord>, Q3 <ord>, Q4 <ord>, Q5 <ord>, Q6 <ord>, Q7 <ord>, Q8 <ord>,
    ## #   Manipulation_Q1_1 <chr>, Manipulation_Q1_2 <chr>, Manipulation_Q1_3 <chr>,
    ## #   Manipulation_Q2 <ord>, Manipulation_Q2_TEXT <chr>, QD2 <ord>,
    ## #   QD2_1_TEXT <dbl>, QD3_1 <chr>, QD3_2 <chr>, QD3_3 <chr>, QD3_4 <chr>, …

``` r
df <- df %>%
  check_completion()
```

``` r
df <- df %>%
  check_location_screen()
```

    ## Warning in check_location_screen(.): Dropping 1 survey respondents who are not
    ## in the US

    ## Warning in check_location_screen(.): 1 survey respondents who are not in the US
    ## with non-missing responses

    ## Warning in check_location_screen(.): 21 respondents in the data

``` r
# visually assessing why these respondents failed the commitment check
df %>%
  filter(Commitment_Q1 != "Yes, I will") %>%
  select(Commitment_Q1, Commitment_Q2, `Duration (in seconds)`, Finished, batch_id, batch_type)
```

    ## # A tibble: 1 × 6
    ##   Commitment_Q1           Commitment_Q2 Duration (in seconds…¹ Finished batch_id
    ##   <ord>                   <chr>                          <dbl> <lgl>       <dbl>
    ## 1 I can't promise either… purple                            55 TRUE            0
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`
    ## # ℹ 1 more variable: batch_type <chr>

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

    ## Warning in check_commitment(.): Dropping 1 survey respondents did not pass
    ## commitment check 1

    ## Warning in check_commitment(.): 20 respondents in the data

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
  verify(!is.na(context))   %>%
  verify(!is.na(context_label))

df %>%
  group_by(batch_type, context, context_label) %>%
  summarize(n = n())
```

    ## `summarise()` has grouped output by 'batch_type', 'context'. You can override
    ## using the `.groups` argument.

    ## # A tibble: 4 × 4
    ## # Groups:   batch_type, context [4]
    ##   batch_type context context_label     n
    ##   <chr>      <ord>   <chr>         <int>
    ## 1 Warmup     1       black_low         8
    ## 2 Warmup     2       black_high        5
    ## 3 Warmup     3       white_low         4
    ## 4 Warmup     4       white_high        3

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
    ## 1 Warmup            0    20

``` r
df %>%
  group_by(context, batch_type, batch_id) %>%
  filter(batch_type %in% c("Warmup", "Iterative Batch Phase: Max")) %>%
  summarize(n = n()) %>%
  group_by(batch_type, batch_id) %>%
  mutate(n = n / sum(n)) %>%
  ggplot(aes(context, n)) +
  geom_col() +
  theme_classic() +
  facet_wrap(~batch_id) +
  labs(
    title = "Sample Sizes in Iterative Batch Phase: Max",
    y = "Share of total"
  )
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
  mutate(n = n / sum(n)) %>%
  ggplot(aes(context, n)) +
  geom_col() +
  theme_classic() +
  facet_wrap(~batch_id) +
  labs(
    title = "Sample Sizes in Iterative Batch Phase: Min",
    y = "Share of total"
  )
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
  mutate(n = cumsum(n) / sum(n)) %>%
  ggplot(aes(context, n)) +
  geom_col() +
  theme_classic() +
  facet_wrap(~batch_id) +
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
  mutate(n = cumsum(n) / sum(n)) %>%
  ggplot(aes(context, n)) +
  geom_col() +
  theme_classic() +
  facet_wrap(~batch_id) +
  labs(title = "Cumulative Sample Sizes in Iterative Batch Phase: Min")
```

    ## `summarise()` has grouped output by 'context', 'batch_type'. You can override
    ## using the `.groups` argument.

![](00_get_qualtrics_data_job_applicants_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
df %>%
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
df %>%
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
  labs(
    title = "CDF for Iterative Batch Phase: Min",
    y = "Cumulative Probability"
  )
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

    ## # A tibble: 20 × 10
    ##    chose_mother    Q1    Q2    Q3    Q4    Q5    Q6    Q7    Q8 context
    ##           <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <ord>  
    ##  1            0     0    NA    NA    NA    NA    NA    NA    NA 4      
    ##  2            1     1    NA    NA    NA    NA    NA    NA    NA 1      
    ##  3            0    NA    NA    NA    NA    NA    NA    NA     0 2      
    ##  4            1    NA    NA    NA    NA    NA     1    NA    NA 1      
    ##  5            0    NA    NA    NA    NA    NA     0    NA    NA 4      
    ##  6            1    NA    NA    NA    NA    NA    NA    NA     1 1      
    ##  7            0    NA    NA    NA     0    NA    NA    NA    NA 1      
    ##  8            1    NA    NA    NA    NA     1    NA    NA    NA 2      
    ##  9            1    NA     1    NA    NA    NA    NA    NA    NA 2      
    ## 10            0    NA    NA    NA    NA    NA    NA    NA     0 3      
    ## 11            1    NA    NA    NA     1    NA    NA    NA    NA 4      
    ## 12            1    NA    NA    NA     1    NA    NA    NA    NA 1      
    ## 13            0    NA    NA    NA    NA    NA    NA    NA     0 3      
    ## 14            1    NA    NA    NA    NA    NA     1    NA    NA 1      
    ## 15            0    NA    NA    NA    NA    NA     0    NA    NA 1      
    ## 16            0    NA    NA    NA    NA    NA    NA     0    NA 1      
    ## 17            1    NA     1    NA    NA    NA    NA    NA    NA 2      
    ## 18            0    NA    NA    NA     0    NA    NA    NA    NA 3      
    ## 19            0    NA    NA    NA    NA    NA    NA     0    NA 3      
    ## 20            0    NA    NA    NA     0    NA    NA    NA    NA 2

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
    ## 1 1           8     5     3
    ## 2 2           5     3     2
    ## 3 3           4     0     4
    ## 4 4           3     1     2

``` r
# identify context desc
df_clean %>%
  select(context, context_label, starts_with("name"), starts_with("education")) %>%
  distinct() %>%
  arrange(context)
```

    ## # A tibble: 4 × 6
    ##   context context_label name1          name2             education1   education2
    ##   <ord>   <chr>         <chr>          <chr>             <chr>        <chr>     
    ## 1 1       black_low     Tanisha Rivers Keisha Mosely     B.S. in Bus… B.A. in M…
    ## 2 2       black_high    Tanisha Rivers Keisha Mosely     B.S. in Bus… B.A. in M…
    ## 3 3       white_low     Laurie Schmitt Allison O’Connell B.S. in Bus… B.A. in M…
    ## 4 4       white_high    Laurie Schmitt Allison O’Connell B.S. in Bus… B.A. in M…

``` r
# check manipulation questions
df_clean %>%
  mutate(
    candidate_mother = if_else(rnum_mother <= 0.5, "Candidate 2", "Candidate 1"),
    manipulation_check_missing = rowSums(select(., starts_with("Manipulation_Q1_")) %>%
      is.na())
  ) %>%
  mutate(pass_attention_check = case_when(
    (rnum_mother <= 0.5) &
      (Manipulation_Q1_2 == "Candidate 2") &
      # only when they've selected one response, i.e. missing = 2
      (manipulation_check_missing == 2) ~ 1,
    (rnum_mother > 0.5) &
      (Manipulation_Q1_2 == "Candidate 1") &
      # only when they've selected one response, i.e. missing = 2
      (manipulation_check_missing == 2) ~ 1,
    TRUE ~ 0
  ),
  unsure_attention_check = if_else(Manipulation_Q1_3 == "Not sure" & !is.na(Manipulation_Q1_3), 1, 0)) %>%
  summarize(per_pass_attention_check = mean(pass_attention_check),
            per_unsure = mean(unsure_attention_check))
```

    ## # A tibble: 1 × 2
    ##   per_pass_attention_check per_unsure
    ##                      <dbl>      <dbl>
    ## 1                      0.3        0.4

``` r
df_clean %>% 
  mutate(candidate_mother = if_else(rnum_mother <= 0.5, "Candidate 2", "Candidate 1")) %>% 
  select(rnum_mother, candidate_mother, starts_with("volunteer"), starts_with("Manipulation_Q1_"))
```

    ## # A tibble: 20 × 7
    ##    rnum_mother candidate_mother volunteer1          volunteer2 Manipulation_Q1_1
    ##          <dbl> <chr>            <chr>               <chr>      <chr>            
    ##  1    0.640    Candidate 1      Parent Teacher Ass… Volunteer… <NA>             
    ##  2    0.450    Candidate 2      Volunteer Treasure… Parent Te… <NA>             
    ##  3    0.147    Candidate 2      Volunteer Treasure… Parent Te… <NA>             
    ##  4    0.291    Candidate 2      Volunteer Treasure… Parent Te… <NA>             
    ##  5    0.000294 Candidate 2      Volunteer Treasure… Parent Te… <NA>             
    ##  6    0.917    Candidate 1      Parent Teacher Ass… Volunteer… Candidate 1      
    ##  7    0.878    Candidate 1      Parent Teacher Ass… Volunteer… Candidate 1      
    ##  8    0.362    Candidate 2      Volunteer Treasure… Parent Te… Candidate 1      
    ##  9    0.944    Candidate 1      Parent Teacher Ass… Volunteer… <NA>             
    ## 10    0.472    Candidate 2      Volunteer Treasure… Parent Te… <NA>             
    ## 11    0.0252   Candidate 2      Volunteer Treasure… Parent Te… <NA>             
    ## 12    0.748    Candidate 1      Parent Teacher Ass… Volunteer… <NA>             
    ## 13    0.595    Candidate 1      Parent Teacher Ass… Volunteer… Candidate 1      
    ## 14    0.510    Candidate 1      Parent Teacher Ass… Volunteer… <NA>             
    ## 15    0.450    Candidate 2      Volunteer Treasure… Parent Te… <NA>             
    ## 16    0.478    Candidate 2      Volunteer Treasure… Parent Te… <NA>             
    ## 17    0.447    Candidate 2      Volunteer Treasure… Parent Te… <NA>             
    ## 18    0.356    Candidate 2      Volunteer Treasure… Parent Te… <NA>             
    ## 19    0.201    Candidate 2      Volunteer Treasure… Parent Te… <NA>             
    ## 20    0.586    Candidate 1      Parent Teacher Ass… Volunteer… Candidate 1      
    ## # ℹ 2 more variables: Manipulation_Q1_2 <chr>, Manipulation_Q1_3 <chr>

``` r
df_clean %>% 
  select(Manipulation_Q2, Manipulation_Q2_TEXT) %>% 
  mutate(Fail_Manipulation_Q2 = if_else(Manipulation_Q2 != "Yes", 1, 0)) %>% 
  summarize(across(Fail_Manipulation_Q2, .fns = lst(n = ~sum(.), per = ~mean(.))))
```

    ## # A tibble: 1 × 2
    ##   Fail_Manipulation_Q2_n Fail_Manipulation_Q2_per
    ##                    <dbl>                    <dbl>
    ## 1                      2                      0.1

``` r
df_clean %>% 
  distinct(Manipulation_Q2_TEXT)
```

    ## # A tibble: 19 × 1
    ##    Manipulation_Q2_TEXT                                                         
    ##    <chr>                                                                        
    ##  1 Her resume was slightly better written.                                      
    ##  2 I based my decision on experience. Candidate 2 had some experience in doing …
    ##  3 I sat with a panel to interview and hire teachers for the school I worked fo…
    ##  4 The person who gave more info on the SEO website creation is the one I went …
    ##  5 I was a general manager for jackson hewitt. I had to interview candidates an…
    ##  6 <NA>                                                                         
    ##  7 After interviewing an individual, I would asses whether they were well-spoke…
    ##  8 I’ve hired for my company for seven years.                                   
    ##  9 Interviewing                                                                 
    ## 10 I chose which employees I'd like to interview and who we would ultimitaly ch…
    ## 11 I made the decision based off her school. I had previous candidates that wer…
    ## 12 I was a participant in panel interviews where I contributed me ranking of ea…
    ## 13 Hiring caseworkers for American Red Cross                                    
    ## 14 Candidate 1 had experience that was in line with the job.                    
    ## 15 The top candidate had both marketing and digital marketing experience and th…
    ## 16 I interviewed applicants for a software development team.                    
    ## 17 Candidate 1 had a better work history for the position being applied for     
    ## 18 I chose the first candidate as they had a major in marketing but also a mino…
    ## 19 I’m in charge of hiring for my department.

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

    ## # A tibble: 3 × 3
    ##   race                          n   per
    ##   <chr>                     <int> <dbl>
    ## 1 White                        16  0.8 
    ## 2 Asian                         3  0.15
    ## 3 Black or African American     1  0.05

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
    ## 1 FALSE      8
    ## 2 TRUE      12

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
    ## 1              1         0.05

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
