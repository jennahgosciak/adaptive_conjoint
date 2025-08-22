library(tidyverse)
library(ggplot2)

#########
outcomes500 <- readRDS("./simulation/sim-data/fixed_sample_adaptive_sim_1000_500_diffpt5.RDS")
outcomes1000 <- readRDS("./simulation/sim-data/fixed_sample_adaptive_sim_1000_1000_diffpt5.RDS")
outcomes500 %>% 
  mutate(n_resp = 'Number of respondents: 500') %>% 
  bind_rows(outcomes1000 %>% 
              mutate(n_resp = 'Number of respondents: 1,000')) %>% 
  mutate(n_resp = factor(n_resp, levels = c('Number of respondents: 500',
                                            'Number of respondents: 1,000'))) %>% 
  mutate(type = if_else(type == 'Equal', 'Fixed', type)) %>% 
  filter(context == num_arms) %>% 
  mutate(type = str_to_sentence(type)) %>% 
  group_by(type, num_arms, n_resp) %>% 
  summarize(mean_correct = mean(chose_correct),
            ci_low = mean_correct - (qnorm(0.975) * mean_correct * (1 - mean_correct)) / sqrt(n()),
            ci_high = mean_correct + (qnorm(0.975) * mean_correct * (1 - mean_correct)) / sqrt(n())) %>% 
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
                     labels=c("0", "25%", "50%", "75%", "100%")) +
  facet_wrap(~n_resp)

ggsave("./simulation/fixed_sample_100_across_arms.pdf", width=8, height=6)

#########
df_outcomes_adaptive <- readRDS("./simulation/sim-data/fixed_effect_adaptive_sim_1000.RDS")

n_adaptive_summary <- df_outcomes_adaptive %>% 
  mutate(n_total = n_total - 1 + 100) %>% 
  group_by(num_arms) %>% 
  summarize(mean_n = mean(n_total),
            ci_low = mean_n - qnorm(0.975) * sd(n_total) / sqrt(n()),
            ci_high = mean_n + qnorm(0.975) * sd(n_total) / sqrt(n())) %>% 
  mutate(method = 'adaptive')
n_adaptive_summary

df_outcomes_adaptive %>% 
  rowwise() %>% 
  mutate(y_total = y1 + y0) %>% 
  ungroup() %>% 
  group_by(num_arms, context) %>% 
  summarize(mean_y = mean(y_total))

df_outcomes_equal <- readRDS("./simulation/sim-data/fixed_effect_equal_sim_1000.RDS")
n_equal_summary <- df_outcomes_equal %>% 
  group_by(num_arms) %>% 
  summarize(mean_n = mean(n_total),
            ci_low = mean_n - qnorm(0.975) * sd(n_total) / sqrt(n()),
            ci_high = mean_n + qnorm(0.975) * sd(n_total) / sqrt(n())) %>% 
  mutate(method = 'equal')

df_outcomes_equal %>% 
  rowwise() %>% 
  mutate(y_total = y1 + y0) %>% 
  ungroup() %>% 
  group_by(num_arms, context) %>% 
  summarize(mean_y = mean(y_total))

n_adaptive_summary %>% 
  bind_rows(n_equal_summary) %>% 
  mutate(method = if_else(method=='equal', 'Fixed', str_to_sentence(method))) %>% 
  ggplot(aes(num_arms, mean_n, color=method)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width=0.5) +
  geom_text(aes(y = mean_n + 260,
                label = format(round(mean_n), big.mark = ",", scientific = FALSE)),
            size=3) +
  scale_y_continuous(limits=c(0, 7000),
                     breaks = c(0, 2000, 4000, 6000),
                     labels = c("0", "2,000", "4,000", "6,000")) +
  scale_x_continuous(breaks=seq(9,30,3)) +
  labs(x = 'Number of contexts',
       y = 'Mean number of samples to obtain 95% posterior probability') +
  theme_bw() +
  labs(color = "Sampling type")

ggsave('./simulation/sample_size_adaptive_comparison_1000sim.pdf', width=10, height=10)

n_adaptive_summary %>% 
  bind_rows(n_equal_summary) %>% 
  mutate(method = if_else(method=='equal', 'Fixed', str_to_sentence(method))) %>% 
  pivot_wider(names_from = "method", values_from = c("mean_n", "ci_low", "ci_high")) %>% 
  mutate(diff = mean_n_Fixed - mean_n_Adaptive) %>% 
  ggplot(aes(num_arms, diff)) +
  geom_line()
