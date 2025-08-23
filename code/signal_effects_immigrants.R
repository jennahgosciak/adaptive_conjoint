
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)

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
  filename = here("figures","signal_effects_immigrant_bars.pdf"),
  height = 6, 
  width = 4
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
  filename = here("figures","signal_effects_immigrant_profiles.pdf"),
  height = 15, 
  width = 6.5
)

