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
    ## 1 Warmup            0    20

``` r
df %>%
  group_by(batch_type) %>%
  summarize(n = n())
```

    ## # A tibble: 1 × 2
    ##   batch_type     n
    ##   <ord>      <int>
    ## 1 Warmup        20

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
    ## 1 Warmup           12        0.6          1         0.05

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

    ## # A tibble: 16 × 4
    ## # Groups:   batch_type [1]
    ##    batch_type   age     n   per
    ##    <ord>      <dbl> <int> <dbl>
    ##  1 Warmup        26     2  0.1 
    ##  2 Warmup        28     1  0.05
    ##  3 Warmup        29     1  0.05
    ##  4 Warmup        32     1  0.05
    ##  5 Warmup        33     1  0.05
    ##  6 Warmup        34     2  0.1 
    ##  7 Warmup        35     1  0.05
    ##  8 Warmup        38     1  0.05
    ##  9 Warmup        39     2  0.1 
    ## 10 Warmup        42     2  0.1 
    ## 11 Warmup        47     1  0.05
    ## 12 Warmup        52     1  0.05
    ## 13 Warmup        53     1  0.05
    ## 14 Warmup        56     1  0.05
    ## 15 Warmup        66     1  0.05
    ## 16 Warmup        68     1  0.05

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

    ## # A tibble: 3 × 3
    ##   race                          n   per
    ##   <chr>                     <int> <dbl>
    ## 1 White                        16  0.8 
    ## 2 Asian                         3  0.15
    ## 3 Black or African American     1  0.05

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
