if (!require(pak, quietly = TRUE)) {
  # Install pak binary if not already installed
  install.packages("pak", repos = sprintf(
    "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
    .Platform$pkgType,
    R.Version()$os,
    R.Version()$arch
  ))
}

library(pak)

pkg_install(c(
  "askpass@1.2.1",
  "assertr@3.0.1",
  "dplyr@1.1.4",
  "forcats@1.0.1",
  "furrr@0.3.1",
  "ggplot2@4.0.1",
  "ggtext@0.1.2",
  "here@1.0.2",
  "ipumsr@0.9.0",
  "modelr@0.1.11",
  "progressr@0.16.0",
  "readr@2.1.5",
  "stringr@1.5.2",
  "tidyr@1.3.1"
))