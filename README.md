#  Data-Adaptive Experimentation to Find Contexts with the Most and Least Discrimination

## Internal folder structure:
* `./01_input`: input data files
* `./02_output`: output data files and formatted resumes
* `./03_automate_profiles`: generate resumes and political candidate tables automatically
* `./04_update_prob`: update treatment assignment probabilities

## Information on accessing Qualtrics data with API
* https://cran.r-project.org/web/packages/qualtRics/vignettes/qualtRics.html

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
  install_dependencies = TRUE
)

# Replicate results in ./code/job_applicants_plots.R
jetty::run_script(
  file = here::here("code/job_applicants_plots.R"),
  context = here::here(),
  install_dependencies = TRUE
)
```

### Using Docker

If desired, you can also use Docker directly to replicate the figures.

Set `adaptive_conjoint/` as your working directory. Then execute the
following Docker command:
```
docker run --rm -v ./figures:/adaptive_conjoint/figures/ djmolitor/adaptive_conjoint /bin/bash -c "cd /adaptive_conjoint/ && Rscript code/immigrants_plots.R && Rscript code/job_applicants_plots.R"
```