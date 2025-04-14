#  Adaptive Randomization in Conjoint Survey Experiments

## Folder structure:
* `/data`: Data collected from all experiments.
* `/code`: Code for replicating figures from collected data.
* `/figures`: Output folder for figures.

## Replicating figures

Either of the methods below require Docker to be installed
(these approaches may not work on Windows).
All resulting figures will be stored in the `adaptive_conjoint/figures/` directory.

### Using R

All figures can be replicated using R.

```r
install.packages(pkgs = c("here", "jetty"))

# Replicate results in ./code/immigrants_plots.R
jetty::run_script(
  file = here::here("code/immigrants_plots.R"),
  context = here::here(),
  install_dependencies = TRUE,
  r_profile = NULL
)

# Replicate results in ./code/job_applicants_plots.R
jetty::run_script(
  file = here::here("code/job_applicants_plots.R"),
  context = here::here(),
  install_dependencies = TRUE,
  r_profile = NULL
)
```

### Using Docker

If desired, you can also use Docker directly to replicate the figures.

Set `adaptive_conjoint/` as your working directory. Then execute the
following Docker command:
```
docker run --rm -v ./figures:/adaptive_conjoint/figures/ djmolitor/adaptive_conjoint /bin/bash -c "cd /adaptive_conjoint/ && Rscript code/immigrants_plots.R && Rscript code/job_applicants_plots.R"
```