Demographic Breakdowns by Experimental Phase
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

    ## # A tibble: 1 × 3
    ## # Groups:   batch_type [1]
    ##   batch_type batch_id     n
    ##   <ord>         <dbl> <int>
    ## 1 Warmup            0    50

``` r
df %>%
  group_by(batch_type) %>%
  summarize(n = n())
```

    ## # A tibble: 1 × 2
    ##   batch_type     n
    ##   <ord>      <int>
    ## 1 Warmup        50

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

    ## # A tibble: 1 × 5
    ##   batch_type female_n female_per hispanic_n hispanic_per
    ##   <ord>         <int>      <dbl>      <int>        <dbl>
    ## 1 Warmup           25        0.5          6         0.12

## Examining differences in ages

- Distribution seems similar across phases

``` r
# age
df %>%
  group_by(batch_type, age) %>%
  summarize(n = n()) %>%
  group_by(batch_type) %>%
  mutate(per = n / sum(n))
```

    ## `summarise()` has grouped output by 'batch_type'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 27 × 4
    ## # Groups:   batch_type [1]
    ##    batch_type   age     n   per
    ##    <ord>      <dbl> <int> <dbl>
    ##  1 Warmup        21     2  0.04
    ##  2 Warmup        22     5  0.1 
    ##  3 Warmup        23     5  0.1 
    ##  4 Warmup        24     2  0.04
    ##  5 Warmup        25     3  0.06
    ##  6 Warmup        26     3  0.06
    ##  7 Warmup        27     6  0.12
    ##  8 Warmup        28     1  0.02
    ##  9 Warmup        29     1  0.02
    ## 10 Warmup        30     1  0.02
    ## # ℹ 17 more rows

``` r
df %>%
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
