# R script to run poststratification analysis
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(here)
library(readr)
library(tidyr)

df_analysis <- read_csv(here("data", "job_app_clean_all_phases.csv"))

# Adaptive phase estimates
adaptive_estimates <- df_analysis |>
  group_by(context, context_label) |>
  summarize(alpha = sum(1 - chose_mother) + 10,
            beta = sum(chose_mother) + 10,
            n = n()) |>
  mutate(
    mu = alpha / (alpha + beta),
    lb = qbeta(.025, shape1 = alpha, shape2 = beta),
    ub = qbeta(.975, shape1 = alpha, shape2 = beta)
  ) |>
  separate(context_label, into = c("race", "education")) |>
  mutate(
    x_lab = case_when(
      context == 1 ~ paste0(
        c("**Black** job candidate", "**Mid-ranked** educational institution"),
        collapse = "<br>"
      ),
      context == 2 ~ paste0(
        c("**Black** job candidate", "**Highly ranked** educational institution"),
        collapse = "<br>"
      ),
      context == 3 ~ paste0(
        c("**White** job candidate", "**Mid-ranked** educational institution"),
        collapse = "<br>"
      ),
      context == 4 ~ paste0(
        c("**White** job candidate", "**Highly ranked** educational institution"),
        collapse = "<br>"
      )
    )
  )

estimated_discrim_all_job_app <- ggplot(
    adaptive_estimates,
    aes(x = x_lab, y = mu, ymin = lb, ymax = ub)
  ) +
  geom_point() +
  geom_errorbar(width = 0.1) +
  geom_label(aes(label = paste0("N = ", n), y = 0.44)) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(hjust = 0),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(x = "", y = "Probability of preferring the non-mother job candidate") +
  scale_y_continuous(limits = c(0.41, 0.57))

ggsave(
  plot = estimated_discrim_all_job_app,
  filename = here("figures", "estimated_discrimination_all_job_app.png"),
  width = 7,
  height = 4,
  dpi = 500
)

