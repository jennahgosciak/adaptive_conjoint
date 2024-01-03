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

source("./_functions/experiment_functions.R")
source("./_functions/populate_probs.R")

# load log of probabilities
probabilities <- read_csv("../02_output/probabilities.csv")
probabilities
```

    ## # A tibble: 63 × 4
    ##    Batch `Embedded data variable` CDF_Threshold `Batch Type`              
    ##    <dbl> <chr>                            <dbl> <chr>                     
    ##  1     0 pi1                              0.125 Warmup                    
    ##  2     0 pi2                              0.25  Warmup                    
    ##  3     0 pi3                              0.375 Warmup                    
    ##  4     0 pi4                              0.5   Warmup                    
    ##  5     0 pi5                              0.625 Warmup                    
    ##  6     0 pi6                              0.75  Warmup                    
    ##  7     0 pi7                              0.875 Warmup                    
    ##  8     1 pi1                              0.071 Iterative Batch Phase: Max
    ##  9     1 pi2                              0.241 Iterative Batch Phase: Max
    ## 10     1 pi3                              0.264 Iterative Batch Phase: Max
    ## # ℹ 53 more rows

``` r
df_clean <- readRDS("../02_output/political_candidates_data_clean.RDS")

# current batch type that we want (whether min or max discriminatory context)
current_batch_type <- "Iterative Batch Phase: Max"

# current batch num should just be the last batch number (if we are just updating sequentially)
current_batch_num <- df_clean %>%
  filter(batch_type %in% c("Warmup", current_batch_type)) %>%
  pull(batch_id) %>%
  max()
```

# Update treatment probabilities

``` r
# define with comments
num_contexts <- 8 # number of contexts
num_sim <- 1e6 # number of simulations for Monte Carlo simulation

# init to 0 each time since we are using the full data
# to update the number of outcomes = 1 and = 0
num_outcome1 <- integer(num_contexts)
num_outcome0 <- integer(num_contexts)
print(str_glue("Number of rows in data: {nrow(df_clean)}"))
```

    ## Number of rows in data: 1017

``` r
# filter for correct batch types (whether max discriminatory or min)
df_clean <- df_clean %>%
  filter(batch_type %in% c("Warmup", current_batch_type))

# if we want to filter for less data (e.g., just the warmup data)
if (current_batch_num != max(df_clean$batch_id)) {
  print(str_glue("\nFiltering data for batch id <= {current_batch_num} and batch types 'Warmup' or '{batch_type}'"))
  df_clean <- df_clean %>%
    filter(batch_id <= current_batch_num)
}

print(str_glue("Current batch ID: {current_batch_num}"))
```

    ## Current batch ID: 4

``` r
print(str_glue("Number of rows left in data: {nrow(df_clean)}"))
```

    ## Number of rows left in data: 716

``` r
set.seed(2023)

# generate prob of most (or least) discriminatory context
output <- update_ts(df_clean, num_sim, num_contexts, num_outcome1,
  num_outcome0,
  cdf = TRUE,
  type = current_batch_type
)
```

    ## [1] 1e+06
    ## [1] "Predicting the most discriminatory context: taking the argmax"
    ## [1] "PDF: 0.031566,0.027018,0.006979,0.013139,0.82026,0.035434,0.015992,0.049612"
    ## [1] "CDF: 0.031566,0.058584,0.065563,0.078702,0.898962,0.934396,0.950388,1"

``` r
output
```

    ## $pi
    ## [1] 0.031566 0.058584 0.065563 0.078702 0.898962 0.934396 0.950388 1.000000
    ## 
    ## $num_outcome1
    ## [1]  33  57  32  26 238  54  34  62
    ## 
    ## $num_outcome0
    ## [1] 12 22 15 11 64 20 14 22

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
current_flow <- get_survey_flow(config$api_token, config$pol_candidates_survey_id, config$datacenter_id)
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

    ## For pi1, replacing old probability 0.071 with new probability 0.032
    ## For pi2, replacing old probability 0.241 with new probability 0.059
    ## For pi3, replacing old probability 0.264 with new probability 0.066
    ## For pi4, replacing old probability 0.277 with new probability 0.079
    ## For pi5, replacing old probability 0.579 with new probability 0.899
    ## For pi6, replacing old probability 0.741 with new probability 0.934
    ## For pi7, replacing old probability 0.768 with new probability 0.95

``` r
# Reconstruct the full survey configuration with the modified flow part
current_flow$result$Flow <- modified_flow_data

# Convert the entire modified survey configuration to JSON
json_payload <- toJSON(current_flow$result, auto_unbox = TRUE)

# Make the PUT request to update the survey flow
update_response <- update_survey_flow(config$api_token, config$pol_candidates_survey_id, current_flow$result, config$datacenter_id)
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
    ## [1] "f238095a-6657-4a66-897b-a88e958b2712"

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
  arrange(Batch, `Batch Type`) %>%
  write_csv("../02_output/probabilities.csv")
```
