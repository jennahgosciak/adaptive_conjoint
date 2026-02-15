library(dplyr)
library(ggplot2)
library(here)
library(stringr)

#########
# Produces figures for appendix D

outcomes500 <- readRDS(here("data/simulation-data/fixed_sample_adaptive_sim_1000_500.RDS"))
outcomes1000 <- readRDS(here("data/simulation-data/fixed_sample_adaptive_sim_1000_1000.RDS"))
outcomes500 |> 
  mutate(n_resp = 'Number of respondents: 500') |> 
  bind_rows(outcomes1000 |> 
              mutate(n_resp = 'Number of respondents: 1,000')) |> 
  mutate(n_resp = factor(n_resp, levels = c('Number of respondents: 500',
                                            'Number of respondents: 1,000'))) |> 
  mutate(type = if_else(type == 'Equal', 'Fixed', type)) |> 
  # we only want one row for each simulation, select the row with the max value of true_p (i.e., context == num_arms)
  filter(context == num_arms) |> 
  mutate(type = str_to_sentence(type)) |> 
  group_by(type, num_arms, n_resp) |> 
  # calculate percent of time chose correctly, 95% confidence intervals
  summarize(mean_correct = mean(chose_correct),
            ci_low = mean_correct - (qnorm(0.975) * sqrt(mean_correct * (1 - mean_correct) / n())),
            ci_high = mean_correct + (qnorm(0.975) * sqrt(mean_correct * (1 - mean_correct) / n()))) |> 
  ggplot(aes(num_arms, mean_correct, color = type)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = ci_low, ymax=ci_high, color=type), width=0.3) +
  geom_text(aes(y = mean_correct + 0.05,
                label = str_c(round(100*mean_correct), "%")),
            size=3) +
  theme_bw() +
  scale_x_continuous(breaks=seq(9,30,3)) +
  labs(y = "Percent of simulations with the correct arm selection (out of 1,000)",
       x = "Number of contexts",
       color = "Sampling method") +
  scale_y_continuous(limits = c(0,1.05),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels=c("0", "25%", "50%", "75%", "100%")) +
  facet_wrap(~n_resp)

ggsave(here("figures/figure13.png"), dpi = 500, width=8, height=6)

#########
df_outcomes_adaptive <- readRDS(here("data/simulation-data/fixed_effect_adaptive_sim_1000.RDS"))

n_adaptive_summary <- df_outcomes_adaptive |> 
  # we only need one observation per context
  # select the one with the max value of true_p (i.e., num_arms == context)
  filter(num_arms == context) |> 
  group_by(num_arms) |> 
  # compute mean and 95% confidence interval for the sample size
  summarize(mean_n = mean(n_total),
            ci_low = mean_n - qnorm(0.975) * sd(n_total) / sqrt(n()),
            ci_high = mean_n + qnorm(0.975) * sd(n_total) / sqrt(n())) |> 
  mutate(method = 'adaptive')
n_adaptive_summary

df_outcomes_equal <- readRDS(here("data/simulation-data/fixed_effect_equal_sim_1000.RDS"))
n_equal_summary <- df_outcomes_equal |> 
  # we only need one observation per sample size
  filter(num_arms == context) |> 
  group_by(num_arms) |> 
  # compute mean and 95% confidence interval for the sample size
  summarize(mean_n = mean(n_total),
            ci_low = mean_n - qnorm(0.975) * sd(n_total) / sqrt(n()),
            ci_high = mean_n + qnorm(0.975) * sd(n_total) / sqrt(n())) |> 
  mutate(method = 'equal')

n_adaptive_summary |> 
  bind_rows(n_equal_summary) |> 
  mutate(method = if_else(method=='equal', 'Fixed', str_to_sentence(method))) |>
  filter(num_arms %in% seq(9, 27, by = 6)) |>
  ggplot(aes(num_arms, mean_n, color=method)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width=0.5) +
  geom_text(aes(y = ci_high + 200,
                label = format(round(mean_n), big.mark = ",", scientific = FALSE)),
            size=3) +
  scale_y_continuous(limits=c(0, 18000),
                     breaks = c(0, 5000, 10000, 15000),
                     labels = c("0", "5,000", "10,000", "15,000")) +
  scale_x_continuous(breaks=seq(9, 27, 3), limits = c(8.5, 27.5)) +
  labs(x = 'Number of contexts',
       y = 'Mean number of samples to obtain 95% posterior probability') +
  theme_bw() +
  labs(color = "Sampling type")

ggsave(here('figures/figure14.png'), dpi = 500, width=10, height=10)
