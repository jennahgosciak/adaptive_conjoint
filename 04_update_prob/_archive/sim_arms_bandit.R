library(tidyverse)
library(foreach)
theme_set(theme_minimal())

set.seed(14850)

theta_0 <- 1
theta_1 <- 1
true_pi <- .2
for (i in 1:21) {
  if (i <= 5 | i %% 5 == 1) {
    data.frame(pi = seq(0, 1, .01)) %>%
      mutate(y = dbeta(pi, theta_0, theta_1)) %>%
      ggplot(aes(x = pi, y = y)) +
      geom_line() +
      ggtitle(paste("After", i - 1, "observations: Beta(", theta_0, ", ", theta_1, ")")) +
      xlab("Unknown pi = P(Agree)") +
      ylab("Posterior Density")
    ggsave(paste0("beta_sim_", i - 1, ".pdf"),
      height = 3, width = 5
    )
  }
  y <- rbinom(1, 1, true_pi)
  if (y == 1) {
    theta_1 <- theta_1 + 1
  } else {
    theta_0 <- theta_0 + 1
  }
}

# theta will be a p x 2 matrix
theta <- rbind(c(1, 1), c(1, 1), c(1, 1))
true_pi <- c(.2, .5, .8)
for (index in 1:200) {
  forplot <- foreach(i = 1:nrow(theta), .combine = "rbind") %do% {
    data.frame(pi = seq(0, 1, .01)) %>%
      mutate(
        f_pi = dbeta(pi, theta[i, 1], theta[i, 2]),
        arm = paste("Treatment", LETTERS[i])
      )

    data.frame(pi = seq(0, 1, .01)) %>%
      mutate(
        f_pi = dbeta(pi, theta[i, 1], theta[i, 2]),
        arm = paste("Treatment", LETTERS[i])
      ) %>%
      print()
  }
  sampled <- data.frame(
    arm = paste("Treatment", LETTERS[1:3]),
    pi = apply(theta, 1, function(x) rbeta(1, x[1], x[2]))
  )
  chosen <- which(sampled$pi == max(sampled$pi))
  # Or should we sample Y also, or just pi?
  if (index <= 10 | index %% 50 == 0) {
    p <- forplot %>%
      ggplot(aes(x = pi, y = f_pi)) +
      geom_line() +
      facet_wrap(~arm, ncol = 1)
    ggsave(paste0("thompson_p1_", index, ".pdf"),
      plot = p,
      height = 4, width = 5
    )
    p2 <- p +
      geom_vline(
        data = sampled,
        aes(xintercept = pi)
      )
    ggsave(paste0("thompson_p2_", index, ".pdf"),
      plot = p2,
      height = 4, width = 5
    )
    p3 <- p2 +
      geom_vline(
        data = sampled %>% filter(pi == max(pi)),
        aes(xintercept = pi),
        color = "blue"
      )
    ggsave(paste0("thompson_p3_", index, ".pdf"),
      plot = p3,
      height = 4, width = 5
    )
  }
  y <- rbinom(1, 1, prob = true_pi[chosen])
  if (y == 1) {
    theta[chosen, 2] <- theta[chosen, 2] + 1
  } else {
    theta[chosen, 1] <- theta[chosen, 1] + 1
  }
}
