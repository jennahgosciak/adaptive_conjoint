sim-power-multiarm
================
2025-08-07

## Static/fixed 16-arm experiment

``` r
mde <- 0.75 - 0.64
arms <- c(1:16)
true_p <- c(c(0.64, 0.75), runif(length(arms)-2, 0.66, 0.73))

draw_sample <- function(n_size, true_p) {
  # return(unlist(map(true_p, ~rbinom(n=1, size=1, prob=.))))
  return(rbinom(n=n_size*length(true_p), size=1, prob=true_p))
}

draw_adaptive_sample <- function(pi, true_p) {
  profile_draw <- rmultinom(1, 1, rep(1,16)/1)
  stopifnot(sum(profile_draw)==1)
  context <- which.max(profile_draw)
  
  output <- rep(NA, 16)
  draw <- rbinom(n=1, size=1, prob=true_p[context])
  output[context] <- draw
  return(output)
}

format_sample_df <- function(sample_draws) {
  sample_draws %>%
    as_tibble() %>%
    mutate(context = ((row_number()-1) %% 16) + 1)
}

produce_est <- function(n) {
    # print("#############")
    # print(str_glue("N={n}"))

  est <- lst()
  for (sim in c(1:500)) {
    outcomes <- draw_sample(n, true_p)
    outcomes_df <- outcomes %>% 
      format_sample_df()
    
    outcomes_df_grpd <- outcomes_df %>% 
      group_by(context) %>% 
      summarize(mean_est = mean(value)) %>% 
      mutate(n_sim = sim) %>% 
      ungroup()

    test_res <- outcomes_df %>% 
      filter(context %in% c(1,2)) %>% 
      t.test(value ~ context, data = .)
    
    est[[sim]] <- outcomes_df_grpd %>% 
      mutate(reject = case_when(test_res$p.value < 0.05 & context == 1 ~ 1, 
                                test_res$p.value >= 0.05 & context == 1 ~ 0,
                                TRUE ~ NA))
  }
  est_df <- bind_rows(est) %>% 
    mutate(n_size = n,
           power = mean(reject, na.rm = TRUE))
  return(est_df)
}
```

``` r
est_df <- map_dfr(seq(100,10000,1000), ~produce_est(.))
est_df %>% 
  group_by(n_size, context) %>% 
  summarize(mean = mean(mean_est),
         min = min(mean_est),
         max = max(mean_est)) %>% 
  group_by(n_size) %>% 
  mutate(max_est = max(mean),
         max_est_arm = which.max(mean)) %>% 
  #filter(row %in% c(1,2)) %>% 
  mutate(context_label = if_else(context==1, "Min arm", "Max arm")) %>% 
  mutate(total_n = 16*n_size) %>% 
  mutate(context = factor(context)) %>% 
  ungroup() %>% 
  ggplot() +
  #geom_hline(aes(yintercept=0.7), color='red', linetype='dashed') +
  geom_ribbon(data = ~filter(., (context %in% c(3:16))),
                            aes(total_n, ymin=min, ymax=max, fill=context), alpha=0.3) +
  geom_ribbon(data = ~filter(., (context %in% c(1,2))),
                            aes(total_n, ymin=min, ymax=max, group=context), alpha=0.6) +
  geom_line(data = ~filter(., (context %in% c(3:16))),
            aes(total_n, mean, color=context), alpha=0.3) +
  geom_line(data = ~filter(., (context %in% c(1,2))),
            aes(total_n, mean, group=context)) +
  theme_classic() +
  #facet_grid(cols = vars(row_label)) +
  labs(y = 'Mean arm estimate variation',
       x = 'Total number of participants')
```

    ## `summarise()` has grouped output by 'n_size'. You can override using the
    ## `.groups` argument.

![](sim-power-multiarm_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Adaptive experiment simulation

``` r
update_outcomes <- function(df, num_contexts) {
  # vector of num outcomes for either 1 or 0
  df %>% 
    mutate(chose_educ0 = if_else(chose_educ==0 & !is.na(chose_educ), 1, 0)) %>% 
    group_by(context) %>% 
    summarize(chose1 = sum(chose_educ, na.rm = TRUE),
              chose0 = sum(chose_educ0, na.rm = TRUE))
}

update_ts <- function(df, num_sim, num_contexts, type) {
  # with the data provided
  # calculated the observed outcomes = 1, and outcomes = 0
  
  df_outcomes <- df %>% 
    update_outcomes(., num_contexts)
  
  num_outcome1 <- df_outcomes %>% 
    pull(chose1)
  
  num_outcome0 <- df_outcomes %>% 
    pull(chose0)

  # calculate the probability that each arm is the best
  draws <- replicate(num_sim, rbeta(num_contexts, num_outcome1 + 1, num_outcome0 + 1))
  if (type == "max") {
    #print("Predicting the most discriminatory context: taking the argmax")
    # calculate argmax across draws
    arg <- apply(draws, 2, which.max)
  } else if (type == "min") {
    #print("Predicting the least discriminatory context: taking the argmin")
    # calculate argmin across draws
    arg <- apply(draws, 2, which.min)
  } else {
    print("Direction 'type' is unclear")
  }

  # generate new pi
  # print(arg)
  # print(table(cut(arg, 0:num_contexts)))
  pi <- unname(table(cut(arg, 0:num_contexts)) / num_sim)
  #print(str_c("PDF: ", str_c(pi, collapse = ",")))
  stopifnot(length(pi) == num_contexts)
  return(pi)
}

adaptive_phase <- function(warmup_df, n_total, n_sim=1000) {
  pi <- rep(1, 16)/16
  df <- warmup_df
  for (i in c(1:n_total)) {
    adaptive_draw <- draw_adaptive_sample(pi, true_p) %>% 
      format_sample_df() %>% 
      rename(chose_educ = value)
    
    df <- bind_rows(df, adaptive_draw)
    pi <- update_ts(df, n_sim, 16, "max")
    stopifnot(abs(sum(pi)-1) < 0.05)
  }
  outcomes <- df %>% 
    update_outcomes(16)
  
  return(lst(outcomes, pi))
}

full_experiment <- function(n_warmup, n_adaptive, true_p) {
  warmup_df <- draw_sample(n_warmup/16, true_p) %>% 
    format_sample_df() %>% 
    rename(chose_educ = value)
  return(adaptive_phase(warmup_df, n_adaptive))
}
```

``` r
adaptive_warmupfixed <- function(n, true_p) {
  print("###########")
  print(str_glue("Running for n={n}"))
  adaptive_res <- full_experiment(125*16, n, true_p)
  final_pi <- adaptive_res$pi
  final_outcomes <- adaptive_res$outcomes
  mean_estimates_df <- final_outcomes %>% 
    rowwise() %>% 
    mutate(chose_n = chose1 + chose0,
           mean_est = chose1 / chose_n) %>% 
    ungroup() %>% 
    mutate(n_adaptive = n)
  return(mean_estimates_df)
}

# testing for fixed warmup
n_sim <- 20
tic()
mean_estimates_warmupfixed <- future_map_dfr(1:n_sim, ~adaptive_warmupfixed(1000, true_p), seed=TRUE) %>% 
  bind_rows(future_map_dfr(1:n_sim, ~adaptive_warmupfixed(3000, true_p), seed=TRUE)) %>% 
  bind_rows(future_map_dfr(1:n_sim, ~adaptive_warmupfixed(5000, true_p), seed=TRUE)) %>% 
  bind_rows(future_map_dfr(1:n_sim, ~adaptive_warmupfixed(7000, true_p), seed=TRUE))
```

    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000

    ## Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
    ## numbers without specifying argument 'seed'. There is a risk that those random
    ## numbers are not statistically sound and the overall results might be invalid.
    ## To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this
    ## check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".

    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000
    ## [1] "###########"
    ## Running for n=1000

    ## Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
    ## numbers without specifying argument 'seed'. There is a risk that those random
    ## numbers are not statistically sound and the overall results might be invalid.
    ## To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this
    ## check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".

    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000

    ## Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
    ## numbers without specifying argument 'seed'. There is a risk that those random
    ## numbers are not statistically sound and the overall results might be invalid.
    ## To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this
    ## check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".

    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000
    ## [1] "###########"
    ## Running for n=3000

    ## Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
    ## numbers without specifying argument 'seed'. There is a risk that those random
    ## numbers are not statistically sound and the overall results might be invalid.
    ## To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this
    ## check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".

    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000

    ## Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
    ## numbers without specifying argument 'seed'. There is a risk that those random
    ## numbers are not statistically sound and the overall results might be invalid.
    ## To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this
    ## check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".

    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000
    ## [1] "###########"
    ## Running for n=5000

    ## Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
    ## numbers without specifying argument 'seed'. There is a risk that those random
    ## numbers are not statistically sound and the overall results might be invalid.
    ## To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this
    ## check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".

    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000

    ## Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
    ## numbers without specifying argument 'seed'. There is a risk that those random
    ## numbers are not statistically sound and the overall results might be invalid.
    ## To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this
    ## check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".

    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000
    ## [1] "###########"
    ## Running for n=7000

    ## Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
    ## numbers without specifying argument 'seed'. There is a risk that those random
    ## numbers are not statistically sound and the overall results might be invalid.
    ## To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this
    ## check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".

``` r
toc()
```

    ## 1264.773 sec elapsed

``` r
mean_estimates_warmupfixed
```

    ## # A tibble: 1,280 × 6
    ##    context chose1 chose0 chose_n mean_est n_adaptive
    ##      <dbl>  <int>  <dbl>   <dbl>    <dbl>      <dbl>
    ##  1       1    110     68     178    0.618       1000
    ##  2       2    151     44     195    0.774       1000
    ##  3       3    126     65     191    0.660       1000
    ##  4       4    126     60     186    0.677       1000
    ##  5       5    122     67     189    0.646       1000
    ##  6       6    134     56     190    0.705       1000
    ##  7       7    139     54     193    0.720       1000
    ##  8       8    125     52     177    0.706       1000
    ##  9       9    147     46     193    0.762       1000
    ## 10      10    133     53     186    0.715       1000
    ## # ℹ 1,270 more rows

``` r
mean_estimates_warmupfixed  %>% 
  group_by(n_adaptive, context) %>% 
  summarize(mean = mean(mean_est),
         min = min(mean_est),
         max = max(mean_est)) %>% 
  group_by(n_adaptive) %>% 
  mutate(max_est = max(mean),
         max_est_arm = which.max(mean)) %>% 
  #filter(row %in% c(1,2)) %>% 
  mutate(context_label = if_else(context==1, "Min arm", "Max arm")) %>% 
  mutate(total_n = n_adaptive + (125*16)) %>% 
  #filter(context %in% c(1,2)) %>% 
  mutate(context = factor(context)) %>% 
  ungroup() %>% 
  ggplot() +
  #geom_hline(aes(yintercept=0.7), color='red', linetype='dashed') +
  geom_ribbon(data = ~filter(., (context %in% c(3:16))),
                            aes(n_adaptive, ymin=min, ymax=max, fill=context), alpha=0.3) +
  geom_ribbon(data = ~filter(., (context %in% c(1,2))),
                            aes(n_adaptive, ymin=min, ymax=max, group=context), alpha=0.6) +
  geom_line(data = ~filter(., (context %in% c(3:16))),
            aes(n_adaptive, mean, color=context), alpha=0.3) +
  geom_line(data = ~filter(., (context %in% c(1,2))),
            aes(n_adaptive, mean, group=context)) +
  theme_classic() +
  facet_grid(cols = vars(context)) +
  labs(y = 'Mean arm estimate variation',
       x = 'Total number of participants\n(fixed warmup phase n=2,000)')
```

![](sim-power-multiarm_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
