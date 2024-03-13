Process Qualtrics Data and Treatment Assignment Updating for Political
Candidates Survey
================
2023-11-07

# Setup Data Using Qualtrics API

- Load data directly from Qualtrics
- Can store API Key and credentials in `.renviron`

``` r
library(tidyverse)
library(magrittr)
library(assertr)

knitr::opts_chunk$set(cache.extra = 2023)

source("../_functions/experiment_functions.R")
source("../_functions/populate_probs.R")

# load log of probabilities
probabilities <- read_csv("../../02_output/probabilities_job_applicants.csv")
probabilities
```

    ## # A tibble: 126 × 4
    ##    Batch `Embedded data variable` CDF_Threshold `Batch Type`              
    ##    <dbl> <chr>                            <dbl> <chr>                     
    ##  1     0 pi1                              0.25  Warmup                    
    ##  2     0 pi2                              0.5   Warmup                    
    ##  3     0 pi3                              0.75  Warmup                    
    ##  4     1 pi1                              0.313 Iterative Batch Phase: Max
    ##  5     1 pi2                              0.367 Iterative Batch Phase: Max
    ##  6     1 pi3                              0.889 Iterative Batch Phase: Max
    ##  7     2 pi1                              0.378 Iterative Batch Phase: Max
    ##  8     2 pi2                              0.398 Iterative Batch Phase: Max
    ##  9     2 pi3                              0.91  Iterative Batch Phase: Max
    ## 10     3 pi1                              0.569 Iterative Batch Phase: Max
    ## # ℹ 116 more rows

``` r
df_clean <- readRDS("../../02_output/job_applicants_data_clean.RDS")

# current batch type that we want (whether min or max discriminatory context)
current_batch_type <- "Iterative Batch Phase: Min"
survey_id <- config$job_applicants_survey_id

# current batch num should just be the last batch number (if we are just updating sequentially)
current_batch_num <- df_clean %>%
  filter(batch_type %in% c("Warmup", current_batch_type)) %>%
  pull(batch_id) %>%
  max()
```

# Update treatment probabilities

``` r
# define with comments
num_contexts <- 4 # number of contexts
num_sim <- 1e6 # number of simulations for Monte Carlo simulation

# init to 0 each time since we are using the full data
# to update the number of outcomes = 1 and = 0
num_outcome1 <- integer(num_contexts)
num_outcome0 <- integer(num_contexts)
print(str_glue("Number of rows in data: {nrow(df_clean)}"))
```

    ## Number of rows in data: 4053

``` r
# filter for correct batch types (whether max discriminatory or min)
df_clean <- df_clean %>%
  filter(batch_type %in% c("Warmup", current_batch_type))

# if we want to filter for less data (e.g., just the warmup data)
if (current_batch_num != max(df_clean$batch_id)) {
  print(str_glue("\nFiltering data for batch id <= {current_batch_num} and batch types 'Warmup' or '{current_batch_type}'"))
  df_clean <- df_clean %>%
    filter(batch_id <= current_batch_num)
}

print(str_glue("Current batch ID: {current_batch_num}"))
```

    ## Current batch ID: 20

``` r
print(str_glue("Number of rows left in data: {nrow(df_clean)}"))
```

    ## Number of rows left in data: 2102

``` r
print(str_glue("Only batch type in data: {str_c(unique(df_clean$batch_type), collapse=', ')}"))
```

    ## Only batch type in data: Warmup, Iterative Batch Phase: Min

``` r
set.seed(2023)
nrow(df_clean)
```

    ## [1] 2102

``` r
# generate prob of most (or least) discriminatory context
output <- update_ts(df_clean, "chose_mother", num_sim, num_contexts, num_outcome1,
  num_outcome0,
  cdf = TRUE,
  type = current_batch_type
)
```

    ## [1] 1e+06
    ## [1] "Predicting the least discriminatory context: taking the argmin"
    ## [1] "PDF: 0.571564,0.076013,0.053581,0.298842"
    ## [1] "CDF: 0.571564,0.647577,0.701158,1"

``` r
output
```

    ## $pi
    ## [1] 0.571564 0.647577 0.701158 1.000000
    ## 
    ## $num_outcome1
    ## [1] 333 173 106 438
    ## 
    ## $num_outcome0
    ## [1] 355 158  90 449

``` r
# check total is equal to number of observations in data
if ((sum(output$num_outcome1) + sum(output$num_outcome0)) != nrow(df_clean)) {
  warning("Total of outcome responses does not equal number of rows in data")
}
```

``` r
# vector of new probabilities
# assign names (impt for checking replacement in survey flow)
pi_ts <- round(output$pi[1:(num_contexts - 1)], 3)
names(pi_ts) <- str_c("pi", 1:(num_contexts - 1))
```

``` r
# Retrieve current survey flow (full structure)
current_flow <- get_survey_flow(config$api_token, survey_id, config$datacenter_id)
# Extract just the flow part for modification
current_flow_data <- current_flow$result$Flow

# Update the flow data with new probabilities
modified_flow_data <- update_flow_with_probabilities(
  current_flow_data,
  "FL_10",
  pi_ts,
  names(pi_ts)
)
```

    ## For pi1, replacing old probability 0.59 with new probability 0.572
    ## For pi2, replacing old probability 0.661 with new probability 0.648
    ## For pi3, replacing old probability 0.704 with new probability 0.701

``` r
# Reconstruct the full survey configuration with the modified flow part
current_flow$result$Flow <- modified_flow_data

# Convert the entire modified survey configuration to JSON
json_payload <- toJSON(current_flow$result, auto_unbox = TRUE)

# Make the PUT request to update the survey flow
update_response <- update_survey_flow(config$api_token, survey_id, current_flow$result, config$datacenter_id)
```

    ## JSON Payload for PUT Request:

``` r
# Check the response
print(update_response)
```

    ## $meta
    ## $meta$httpStatus
    ## [1] "200 - OK"
    ## 
    ## $meta$requestId
    ## [1] "4a5bc3d0-b039-4505-9a66-4953b3af6dc1"

``` r
tibble(
  Batch = rep(current_batch_num + 1, (num_contexts - 1)),
  "Embedded data variable" = str_c("pi", 1:(num_contexts - 1)),
  "CDF_Threshold" = pi_ts,
  `Batch Type` = current_batch_type,
) %>%
  bind_rows(probabilities %>%
    # drop if previously updated with same batch number
    filter(!(Batch == (current_batch_num + 1) & `Batch Type` == current_batch_type))) %>%
  mutate(`Batch Type` = factor(`Batch Type`,
    levels = c("Warmup", "Iterative Batch Phase: Max", "Iterative Batch Phase: Min"),
    ordered = TRUE
  )) %>%
  arrange(`Batch Type`, Batch) %>%
  write_csv("../../02_output/probabilities_job_applicants.csv")
```
