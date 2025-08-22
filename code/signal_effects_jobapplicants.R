library(assertr)
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(forcats)

# Load cleaned job applicants file
df_job_app_clean <- read_csv(here("/data/job_applicants_data_clean_2025_08_01.csv")) %>% 
  rename(arm_id = context,
         arm_label = context_label)

# Prepare for computing choices
df_job_app_clean <- df_job_app_clean %>% 
  pivot_longer(c(str_c("Q", 1:8, "_orig")), names_to = "order", values_to = "selection") %>% 
  filter(!is.na(selection)) %>% 
  mutate(chose_cand1 = if_else(selection == 'Candidate 1', 1, 0),
         order = str_replace_all(order, "Q", "") %>% 
           str_replace_all("_orig", "") %>% 
           as.numeric()) %>% 
  mutate(cand1_educ_signal = if_else(order %in% c(1, 2, 3, 4), education1, education2),
         cand1_name_signal = if_else(order %in% c(1, 4, 5, 7), name1, name2),
         cand1_exp_signal = if_else(order %in% c(1, 2, 5, 6), exp1, exp2),
         cand1_exp_desc_signal = if_else(order %in% c(1, 2, 5, 6), exp1_desc, exp2_desc)) %>% 
  mutate(chose_educ1 = case_when((cand1_educ_signal == education1) & (chose_cand1 == 1) ~ 1,
                                 (cand1_educ_signal != education1) & (chose_cand1 == 0) ~ 1,
                                 TRUE ~ 0),
         chose_name1 = case_when((cand1_name_signal == name1) & (chose_cand1 == 1) ~ 1,
                                 (cand1_name_signal != name1) & (chose_cand1 == 0) ~ 1,
                                 TRUE ~ 0),
         chose_exp1 = case_when((cand1_exp_signal == exp1) & (chose_cand1 == 1) ~ 1,
                                 (cand1_exp_signal != exp1) & (chose_cand1 == 0) ~ 1,
                                 TRUE ~ 0))

distinct_labels <- df_job_app_clean %>% 
  # note: volunteer1 is always shown first
  distinct(cand1_educ_signal, cand1_exp_signal, cand1_name_signal, volunteer1)  %>% 
  mutate(label = as.integer(row_number()))

df_job_app_clean %>% 
  left_join(distinct_labels,
            by = c("cand1_educ_signal", "cand1_exp_signal",
                   "cand1_name_signal", "volunteer1")) %>% 
  group_by(arm_id, arm_label, label) %>% 
  summarize(n = n()) %>% 
  ggplot() +
  geom_col(aes(label, n)) +
  facet_wrap(~arm_label) +
  labels(x = "Unique combination of order X treatment")

# Within each context, determine which percent chose each signal
df_chose_signal <- df_job_app_clean %>% 
  group_by(arm_id, arm_label, education1, name1, exp1,
           education2, name2, exp2, exp1_desc, exp2_desc, chose_mother) %>% 
  summarize(across(c(chose_cand1, chose_educ1, chose_name1, chose_exp1), .fns = lst(mean = ~mean(.),
                                                                                     n = ~n()))) %>% 
  verify(chose_cand1_n == chose_educ1_n) %>% 
  verify(chose_cand1_n == chose_exp1_n) %>%
  rename(n = chose_cand1_n) %>% 
  ungroup() %>% 
  select(-str_c(c("chose_educ1", "chose_name1", "chose_exp1"), "_n")) %>% 
  pivot_longer(str_c(c("chose_cand1", "chose_educ1", "chose_name1", "chose_exp1"), "_mean"),
               names_to = "signal_type", values_to = "chose_signal_est") %>% 
  mutate(
    se = sqrt(chose_signal_est * (1 - chose_signal_est) / n),
    ci.min = chose_signal_est - qnorm(.975) * se,
    ci.max = chose_signal_est + qnorm(.975) * se
  ) %>% 
  group_by(signal_type) %>% 
  arrange(chose_signal_est) %>% 
  mutate(ranked_context_position = row_number()) %>% 
  # The name will be used in the graph
  # to say the percent choosing Signal B
  # within this context
  mutate(
    ranked_name = paste0(
      round(100*chose_signal_est),"% Chose Signal 1"
    ),
    ranked_name = fct_reorder(ranked_name, ranked_context_position),
    signal_type_label = case_when(signal_type == 'chose_cand1_mean' ~ 'Candidate 1',
                            signal_type == 'chose_educ1_mean' ~ paste0('Education'),
                            signal_type == 'chose_exp1_mean' ~ paste0('Experience'),
                            signal_type == 'chose_name1_mean' ~ paste0('Name')
  ))

# Bar graph: Rate of choosing each signal
df_chose_signal %>% 
  ggplot(aes(x = -ranked_context_position, y = chose_signal_est)) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  geom_bar(stat = "identity", alpha = .6) +
  geom_errorbar(aes(ymin = ci.min, ymax = ci.max), width = .2) +
  geom_text(aes(label = scales::label_percent(accuracy = 1)(chose_signal_est)),
             y = .05, color = "white", fontface = "bold") +
  theme_bw() +
  coord_flip() +
  scale_x_continuous(breaks = -(1:4), labels = \(x) paste0("Context ",-x)) +
  scale_y_continuous(
    name = "Proportion Choosing Signal 1",
    labels = scales::label_percent(accuracy = 1)
  ) +
  facet_wrap(~signal_type_label) +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())
ggsave(
  "figures/signals_effects_jobapplicants.pdf",
  height = 4.2, width = 8.6
)

# Signal values graph: Text in facets showing the values of different signals
df_distinct_signals <- df_chose_signal %>% 
  ungroup() %>% 
  filter(signal_type  != 'chose_cand1_mean') %>% 
  select(c("arm_id", "education1", "education2", "name1", "name2",
           "exp1", "exp2")) %>% 
  distinct() %>% 
  pivot_longer(c("education1", "education2", "name1", "name2",
                 "exp1", "exp2")) %>% 
  mutate(signal_order = if_else(as.numeric(str_extract(name, "[0-9]+"))==1, 1,
                                           0),
         signal_name = str_extract(name, "[A-z]+")) %>% 
  #mutate(value = str_replace_all(value, "\\|", "\n")) %>%
  mutate(signal_type = case_when(signal_name == 'education' ~ 'chose_educ1_mean',
                                 signal_name == 'exp' ~ 'chose_exp1_mean',
                                 signal_name == 'name' ~ 'chose_name1_mean')) %>% 
  select(-name) 

df_chose_signal %>% 
  select(arm_id, arm_label, n, signal_type, signal_type_label, chose_signal_est,
         ranked_name, ranked_context_position) %>% 
  mutate(arm_label_full = str_c("Context: ", arm_id, "\nRace : ", 
                                str_replace(arm_label, "_", "\nRank : "))) %>% 
  left_join(df_distinct_signals, ., by=c("arm_id", "signal_type")) %>% 
  mutate(x = 0) %>% 
  ggplot() +
  geom_text(
    aes(y = as.integer(signal_order), x = x, label = value),
    hjust = 0,
    size = 1.5
  ) +
  geom_text(
    aes(y = 2, x = x, label=ranked_name),
    hjust = 0,
    size = 1.5,
    fontface = "bold"
  ) +
  facet_grid(rows = vars(arm_label_full),
             cols = vars(signal_type_label),
             scales = "free_y"
  ) +
  xlim(c(0,7)) +
  ylim(c(-0.5,2.5)) +
  annotate(
    geom = "text", fontface = "bold",
    x = 0,
    y = 0.4,
    label = "Signal 2:",
    hjust = 0,
    size = 1.5
  ) +
  annotate(
    geom = "text", fontface = "bold",
    x = 0,
    y = 1.4,
    label = "Signal 1:",
    hjust = 0,
    size = 1.5
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(angle = 0, hjust = 0, vjust = 1, size=7)
  )

ggsave(
  here("figures/signals_effects_text_jobapplicants.pdf"),
  height = 4.2, width = 8.68
)

