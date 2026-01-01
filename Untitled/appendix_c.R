library(assertr)
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(tidyr)

### Figure 11

# Load metadata for which signals go with each assignment
immigrants_metadata <- read_csv(
  here("data/immigrants_main_metadata.csv"),
  col_types = "ccciicc"
) |>
  select(-education) |>
  group_by(arm_id) |>
  mutate(option = c(0L, 1L)) |>
  ungroup()

# Load responses from participants
immigrants_response <- read_csv(
  here("data/immigrants_main_response.csv"),
  col_types = "ilciiccccililildicc"
) |>
  filter(!garbage) |>
  mutate(
    education = case_when(
      discriminated ~ "College degree",
      !discriminated ~ "No formal education"
    )
  )

# Within each context, determine the percent to choose option 1
# (which we will call Primary Signal in the graphs)
chose_option_1 <- immigrants_response |>
  group_by(arm_id) |>
  summarize(
    chose_option_1 = mean(option_preference == 1),
    n = n()
  ) |>
  mutate(
    se = sqrt(chose_option_1 * (1 - chose_option_1) / n),
    ci.min = chose_option_1 - qnorm(.975) * se,
    ci.max = chose_option_1 + qnorm(.975) * se
  ) |>
  # Create a ranked version of contexts to ease presentation
  arrange(chose_option_1) |>
  mutate(ranked_context_position = 1:n()) |>
  # The name will be used in the graph
  # to say the percent choosing the primary signal
  # within this context
  mutate(
    ranked_name = paste0(
      "Context ",ranked_context_position,": ",
      round(100*chose_option_1),"% Chose Primary Signal"
    ),
    ranked_name = fct_reorder(ranked_name, ranked_context_position)
  )

# Bar graph: Rate of choosing option 1 (Primary Signal)
chose_option_1 |>
  ggplot(aes(x = -ranked_context_position, y = chose_option_1)) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  geom_bar(stat = "identity", alpha = .6) +
  geom_errorbar(aes(ymin = ci.min, ymax = ci.max), width = .2) +
  geom_text(aes(label = scales::label_percent(accuracy = 1)(chose_option_1)),
             y = .05, color = "white", fontface = "bold") +
  theme_bw() +
  coord_flip() +
  scale_x_continuous(breaks = -(1:16), labels = \(x) paste0("Context ",-x)) +
  scale_y_continuous(
    name = "Proportion Choosing the Profile\nwith the Primary Signal Vector\n(designation as primary vs\nsecondary signal is arbitrary)",
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())

ggsave(
  filename = here("figures", "figure11a.png"),
  height = 6,
  width = 4,
  dpi = 500
)

# Signal values graph: Text in facets showing the values
# of Primary Signal and Secondary Signal within each context
immigrants_metadata |>
  pivot_longer(cols = c("prior_trips", "origin", "reason", "profession")) |>
  #filter(arm_id == 1) |>
  mutate(y = case_when(
    name == "origin" ~ 4,
    name == "profession" ~ 3,
    name == "reason" ~ 2,
    name == "prior_trips" ~ 1
  )) |>
  left_join(chose_option_1, by = join_by(arm_id)) |>
  ggplot() +
  geom_text(
    aes(y = y, x = 1 - option, label = value),
    hjust = 0,
    size = 2.5
  ) +
  xlim(c(0,2)) +
  ylim(c(.5,5.5)) +
  annotate(
    geom = "text", fontface = "bold",
    x = 1,
    y = 5,
    label = "Secondary Signal",
    hjust = 0,
    size = 2.5
  ) +
  annotate(
    geom = "text", fontface = "bold",
    x = 0,
    y = 5,
    label = "Primary Signal",
    hjust = 0,
    size = 2.5
  ) +
  facet_wrap(
    ~factor(ranked_name),
    ncol = 1
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(angle = 0, hjust = 0, vjust = 1)
  )

ggsave(
  filename = here("figures","figure11b.png"),
  height = 15, 
  width = 6.5,
  dpi = 500
)

### Figure 12

# Load cleaned job applicants file
df_job_app_clean <- read_csv(here("data/job_applicants_data_clean_2025_08_01.csv")) %>% 
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

# Within each context, determine which percent chose each signal
df_chose_signal <- df_job_app_clean %>% 
  group_by(arm_id, arm_label, education1, name1, exp1,
           education2, name2, exp2, exp1_desc, exp2_desc) %>% 
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
  # begin new code that doesn't change context definitions
  mutate(
    context_name = paste0(
      round(100*chose_signal_est),"% Chose Primary Signal"
    ),
    signal_type_label = case_when(signal_type == 'chose_cand1_mean' ~ 'Chose Left\nProfile',
                                  signal_type == 'chose_educ1_mean' ~ paste0('Chose Primary Signal\nof Education'),
                                  signal_type == 'chose_exp1_mean' ~ paste0('Chose Primary Signal\nof Experience'),
                                  signal_type == 'chose_name1_mean' ~ paste0('Chose Primary Signal\nof Name'))
  )

# Bar graph: Rate of choosing each signal
df_chose_signal %>% 
  ggplot(aes(x = -arm_id, y = chose_signal_est)) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  geom_bar(stat = "identity", alpha = .6) +
  geom_errorbar(aes(ymin = ci.min, ymax = ci.max), width = .2) +
  geom_text(aes(label = scales::label_percent(accuracy = 1)(chose_signal_est)),
             y = .05, color = "white", fontface = "bold") +
  theme_bw() +
  coord_flip() +
  scale_x_continuous(breaks = -(1:4), labels = \(x) paste0("Context ",-x)) +
  scale_y_continuous(
    name = "Proportion Choosing Primary Signal\n(primary and secondary distinction is arbitrary)",
    labels = scales::label_percent(accuracy = 1)
  ) +
  facet_wrap(~signal_type_label) +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())

ggsave(
  "figures/figure12a.png",
  height = 4.2,
  width = 8.6,
  dpi = 500
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
  mutate(signal_type = case_when(signal_name == 'education' ~ 'chose_educ1_mean',
                                 signal_name == 'exp' ~ 'chose_exp1_mean',
                                 signal_name == 'name' ~ 'chose_name1_mean')) %>% 
  select(-name) 

df_chose_signal %>% 
  select(arm_id, arm_label, n, signal_type, signal_type_label, chose_signal_est,
         context_name) %>%
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
    aes(y = 2, x = x, label=context_name),
    hjust = 0,
    size = 1.5,
    fontface = "bold"
  ) +
  facet_grid(rows = vars(arm_label_full),
             cols = vars(signal_type_label),
             scales = "free_y",
             labeller = as_labeller(\(x) str_remove(x,"Chose Primary Signal\nof "))
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
  here("figures/figure12b.png"),
  height = 4.2,
  width = 8.68,
  dpi = 500
)