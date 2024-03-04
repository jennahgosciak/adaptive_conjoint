Demographic Breakdowns by Experimental Phase: Job Applicants Experiment
================
2024-01-09

# Loading survey data

- Includes warmup, iterative batch phases, and validation

``` r
df <- readRDS("../../02_output/job_applicants_data_clean.RDS")

df <- df %>%
  mutate(batch_type = factor(batch_type,
    levels = c("Warmup", "Iterative Batch Phase: Max", "Iterative Batch Phase: Min", "Validation"),
    ordered = TRUE
  ))

# look at sample counts by batch id and batch type
df %>%
  group_by(batch_type, batch_id) %>%
  summarize(n = n())
```

    ## `summarise()` has grouped output by 'batch_type'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 21 × 3
    ## # Groups:   batch_type [2]
    ##    batch_type                 batch_id     n
    ##    <ord>                         <dbl> <int>
    ##  1 Warmup                            0   154
    ##  2 Iterative Batch Phase: Max        1    99
    ##  3 Iterative Batch Phase: Max        2    99
    ##  4 Iterative Batch Phase: Max        3    94
    ##  5 Iterative Batch Phase: Max        4    97
    ##  6 Iterative Batch Phase: Max        5    97
    ##  7 Iterative Batch Phase: Max        6    97
    ##  8 Iterative Batch Phase: Max        7    98
    ##  9 Iterative Batch Phase: Max        8    96
    ## 10 Iterative Batch Phase: Max        9    95
    ## # ℹ 11 more rows

``` r
df %>%
  group_by(batch_type) %>%
  summarize(n = n())
```

    ## # A tibble: 2 × 2
    ##   batch_type                     n
    ##   <ord>                      <int>
    ## 1 Warmup                       154
    ## 2 Iterative Batch Phase: Max  1951

## Examining female and hispanicity

- There are some differences in the percent female between the different
  phases (around 10 percentage points)

``` r
## look at female and hispanicity
df %>%
  group_by(batch_type) %>%
  summarize(across(c("female", "hispanic"), .fns = lst(
    n = ~ sum(.),
    per = ~ mean(.)
  )))
```

    ## # A tibble: 2 × 5
    ##   batch_type                 female_n female_per hispanic_n hispanic_per
    ##   <ord>                         <int>      <dbl>      <int>        <dbl>
    ## 1 Warmup                           81      0.526         18       0.117 
    ## 2 Iterative Batch Phase: Max     1000      0.513        149       0.0764

## Examining differences in ages

- Distribution seems similar across phases

``` r
# age
df %>%
  group_by(batch_type, age) %>%
  summarize(n = n()) %>%
  group_by(batch_type) %>%
  mutate(per = n / sum(n)) %>% 
  arrange(desc(age))
```

    ## `summarise()` has grouped output by 'batch_type'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 110 × 4
    ## # Groups:   batch_type [2]
    ##    batch_type                     age     n      per
    ##    <ord>                        <dbl> <int>    <dbl>
    ##  1 Iterative Batch Phase: Max 3000000     1 0.000513
    ##  2 Iterative Batch Phase: Max      90     1 0.000513
    ##  3 Warmup                          78     1 0.00649 
    ##  4 Iterative Batch Phase: Max      78     3 0.00154 
    ##  5 Iterative Batch Phase: Max      77     2 0.00103 
    ##  6 Iterative Batch Phase: Max      76     5 0.00256 
    ##  7 Iterative Batch Phase: Max      75     6 0.00308 
    ##  8 Iterative Batch Phase: Max      74     7 0.00359 
    ##  9 Warmup                          73     1 0.00649 
    ## 10 Iterative Batch Phase: Max      73     6 0.00308 
    ## # ℹ 100 more rows

``` r
df %>%
  # remove outlier
  filter(age < 100) %>% 
  ggplot(aes(age, after_stat(density), fill = batch_type)) +
  geom_histogram() +
  facet_wrap(~batch_type)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](03_demographic_breakdowns_job_applicants_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Examining differences in race

- Distribution also is similar across batches

``` r
# race
df %>%
  group_by(batch_type) %>%
  group_by(race) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(per = n / sum(n)) %>%
  arrange(desc(per))
```

    ## # A tibble: 8 × 3
    ##   race                                  n      per
    ##   <chr>                             <int>    <dbl>
    ## 1 White                              1515 0.720   
    ## 2 Black or African American           274 0.130   
    ## 3 Asian                               154 0.0732  
    ## 4 Multiracial                         104 0.0494  
    ## 5 Other                                38 0.0181  
    ## 6 American Indian or Alaskan Native    11 0.00523 
    ## 7 Prefer not to disclose                7 0.00333 
    ## 8 Native Hawaiian                       2 0.000950

``` r
df %>%
  group_by(batch_type, race) %>%
  summarize(n = n()) %>%
  group_by(batch_type) %>%
  mutate(per = n / sum(n)) %>%
  arrange(desc(per)) %>%
  ggplot(aes(reorder(race, desc(per)), per, fill = batch_type)) +
  geom_col() +
  facet_wrap(~batch_type) +
  labs(
    x = "Race",
    y = "Percent of total"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

    ## `summarise()` has grouped output by 'batch_type'. You can override using the
    ## `.groups` argument.

![](03_demographic_breakdowns_job_applicants_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
