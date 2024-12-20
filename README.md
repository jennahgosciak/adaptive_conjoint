#  Data-Adaptive Experimentation to Find Contexts with the Most and Least Discrimination

## Internal folder structure:
* `./01_input`: input data files
* `./02_output`: output data files and formatted resumes
* `./03_automate_profiles`: generate resumes and political candidate tables automatically
* `./04_update_prob`: update treatment assignment probabilities

## Information on accessing Qualtrics data with API
* https://cran.r-project.org/web/packages/qualtRics/vignettes/qualtRics.html

## Building with Docker

Set `adaptive_conjoint/` as your working directory. Then execute the
following Docker command:
```
docker run --rm -v ./figures:/adaptive_conjoint/figures/ djmolitor/adaptive_conjoint /bin/bash -c "cd /adaptive_conjoint/ && Rscript code/immigrants_plots.R && Rscript code/job_applicants_plots.R"
```

All resulting figures will be stored in the `adaptive_conjoint/figures/` directory.