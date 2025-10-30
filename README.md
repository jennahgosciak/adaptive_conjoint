#  Adaptive Randomization in Conjoint Survey Experiments

<!-- badges: start -->
[![Launch RStudio Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/jennahgosciak/adaptive_conjoint/main?urlpath=rstudio)
<!-- badges: end -->

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

## Generating poststratification weights

> [!NOTE]  
> The poststratification weights used to generate all figures in this paper
> are stored at `data/ipums_strata_sizes.RDS`. You don’t need to regenerate
> them to reproduce the figures, but if you’d like to, follow the steps below.

To re-generate the poststratification weights used in our analysis, you'll need
an IPUMS API key. [Go here](https://account.ipums.org/api_keys) to generate a
new key. Next, interactively execute the `code/download_ps_weights.R` script.
The updated file should be in `data/ipums_strata_sizes.RDS`. To generate these
weights, the script takes the following steps:

1. Load data from IPUMS US via the API.
    - Define the data extract. Sample is from US2022a. The variables we need are AGE, SEX, RACE, STATEFIP, HISPAN.
    - Submit, wait, and then download the data from IPUMS
2. Check AGE is an integer; rename to `age`.
3. Create `race` variable in the following way (based on codebook values from [IPUMS website](https://usa.ipums.org/usa-action/variables/RACE#description_section)):
    - RACE = 1, `race` = "White"
    - RACE = 2, `race` = "Black or African American"
    - RACE = 3, `race` = "American Indian or Alaska Native"
    - RACED in the following list of values (400, 410, 420, 500, 600, 610, 620, 640, 641, 642, 643, 660, 661, 662, 663, 664, 665, 666, 667, 669, 670, 671, 673, 674, 675, 676, 677, 678, 679), `race` = "Asian". Note, these categories are: 400=Chinese, 410=Taiwanese, 420=Chinese and Taiwanese, 500=Japanese, 600=Filipino, 610=Asian Indian (Hindu 1920_1940), 620=Korean, 640=Vietnamese, 641=Bhutanese, 642=Mongolian, 643=Nepalese, 660=Cambodian, 661=Hmong, 662=Laotian, 663=Thai, 664=Bangladeshi, 665=Burmese, 666=Indonesian, 667=Malaysian, 669=Pakistani, 670=Sri Lankan, 671=All other Asian, n.e.c., 673=Chinese and Japanese, 674=Chinese and Filipino, 675=Chinese and Vietnamese, 676=Chinese and Asian write_in; Chinese and Other Asian, 677=Japanese and Filipino, 678=Asian Indian and Asian write_in, 679=Other Asian race combinations.
    - RACED in the following list of values (630, 680, 682, 685, 689, 690, 698, 699), `race` = "Native Hawaiian or Other Pacific Islander". Note, these categories are: 630=Native Hawaiian, 680=Samoan, 682	Tongan, 685=Chamorro, 689=One or more other Micronesian races (2000,ACS), 690=Fijian, 699=Pacific Islander (PI), n.s.
    - RACE = 7, `race` = "Other"
    - RACE is either 8 or 9, `race` = "Two or More Races"
4. Filter for `age` greater than 18
5. Create `hispanic` variable = `FALSE` if HISPAN is "Not Hispanic", = `TRUE` if HISPAN is either ("Mexican", "Other", "Puerto Rican", "Cuban")
6. Create `female` = `TRUE` if SEX == "Female", else `FALSE`
7. Grouping by race, female, hispanic, and age, create the following summarized variables:
    - `weight sum(PERWT)`
    - `num = n()`
    - Then recompute `weight`, outside the summarize so that it is a fraction of the total weight =  `weight / sum(weight)`
