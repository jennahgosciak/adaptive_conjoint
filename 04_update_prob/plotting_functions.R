plot_selected_profiles <- function(profiles_selected, name = "Thompson Sampling") {
  tibble("profiles" = factor(profiles_selected, levels = c(1:c))) %>%
    group_by(profiles) %>%
    summarize(n = n()) %>%
    bind_rows(tibble(profiles = factor(c(1:c), levels = c(1:c)), n = 0)) %>%
    ggplot(aes(profiles, n, label = if_else(n > 0, n, NaN))) +
    geom_col() +
    geom_text(
      binwidth = 1,
      geom = "text",
      color = "black",
      vjust = -0.5
    ) +
    scale_x_discrete(breaks = c(1:c), labels = c(1:c)) +
    labs(
      x = "Profile", y = "Number of profiles selected",
      title = name
    ) +
    theme_classic()
}

plot_observed_profiles <- function(observed_profiles) {
  observed_profiles %>%
    summarize(across(-c(profile), ~ sum(., na.rm = T))) %>%
    pivot_longer(everything(), names_to = "profiles", values_to = "n") %>%
    mutate(profiles = as.factor(str_replace(profiles, "Q", ""))) %>%
    ggplot(aes(profiles, n, label = if_else(n > 0, n, NaN))) +
    geom_col() +
    geom_text(
      binwidth = 1,
      geom = "text",
      color = "black",
      vjust = -0.5
    ) +
    scale_x_discrete(breaks = c(1:10), labels = c(1:10)) +
    labs(
      x = "Profile", y = "Number of Profiles Selected",
      title = "Real actions observed in the data"
    ) +
    theme_classic()
}

plot_dist <- function(numbers_of_rewards_1, numbers_of_rewards_0, c) {
  beta_dist <- map2(
    numbers_of_rewards_1 + 1, numbers_of_rewards_0 + 1,
    ~ dbeta(
      x = seq(0.01, 1, 0.01),
      shape1 = .x,
      shape2 = .y
    )
  )

  beta_dist %>%
    map(as_tibble) %>%
    reduce(bind_cols) %>%
    rename_all(~ str_c("profile", c(1:c))) %>%
    mutate(theta_k_hat = seq(0.01, 1, 0.01)) %>%
    pivot_longer(-theta_k_hat,
      names_to = "profiles", values_to = "density"
    ) %>%
    mutate(profiles = factor(profiles, levels = str_c("profile", c(1:c)))) %>%
    ggplot(aes(theta_k_hat, density, color = profiles)) +
    geom_line() +
    theme_classic() +
    scale_colour_brewer(palette = "Set3")
}

plot_regret <- function(regret, name = "TS Regret") {
  tibble("regret" = regret) %>%
    mutate(t = row_number()) %>%
    unnest() %>%
    ggplot(aes(t, regret)) +
    geom_line() +
    theme_classic() +
    labs(title = name)
}
