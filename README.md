#  Adaptive Randomization in Conjoint Survey Experiments

<!-- badges: start -->
[![Launch RStudio Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/jennahgosciak/adaptive_conjoint/main?urlpath=rstudio)
<!-- badges: end -->

## Data and code description

All raw data collected from our experiments can be found in the `data/` directory.
This comprises all data needed to replicate the figures in the paper (except
poststratification weights; see below). Corresponding code can be found in the
`code/` directory. All figures are in the `figures/` directory.

### Generating poststratification weights

> [!NOTE]  
> The poststratification weights used to generate Appendix Figure 10 in this paper
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

## Computational requirements (and versions used)

So as to not pollute the global environment, we recommend replicating this project
within an R project using renv. This way all dependencies will be installed in a
separate local environment.

- R (4.5.1)
- R packages:
    - askpass (1.2.1)
    - assertr (3.0.1)
    - dplyr (1.1.4)
    - forcats (1.0.1)
    - furrr (0.3.1)
    - ggplot2 (4.0.1)
    - ggtext (0.1.2)
    - gtsummary (2.4.0)
    - here (1.0.2)
    - ipumsr (0.9.0)
    - modelr (0.1.11)
    - progressr (0.16.0)
    - readr (2.1.5)
    - stringr (1.5.2)
    - tidyr (1.3.1)

### Hardware specification

All figures were generated on a Macbook Pro (M3 Max) running MacOS Sequoia, with 36 GB of RAM
and 14 cores. We utilize 13 cores for parallelizing our appendix simulations. Each core used
~ 2 GB of RAM when running simulations, equaling a total of ~ 26 GB of RAM. On some (many?)
machines, this may be prohibitive. In this case, each of the simulation scripts has an
easily-changed parameter for number of cores used that can be set to as few as 1 (in which
case total RAM needed for the analysis should be ~ 3GB). However, this will likely cause the
simulation run time to increase prohibitively (> 1 day at a minimum). Required disk space
for all replication files (including data) is < 50 MB. The computing time on
the Macbook Pro took ~ 684 minutes (11.4 hours).
> [!NOTE]  
> The VAST majority of compute time is required by the simulation scripts for
> the appendix simulations. These scripts are the ones beginning with
> `code/appendix_d_simulations_{...}.R`. All figures can be replicated without re-running
> the simulations because we have stored the intermediate simulation results in the
> `data/simulation-data/` directory. You can comment out the corresponding simulation
> scripts in `main.R` and the replication should generate all figures and in a fraction of
> the time (~ 1 minute). However, in it's current form, `code/main.R` will replicate
> everything, including the time-intensive simulations.

## Replicating figures

To replicate figures, execute the `code/main.R` script from your shell of choice:
```r
Rscript ./code/main.R
```

## Table of contents
```
.
├── code
│   ├── appendix_c.R                                         # Plot Appendix C figures
│   ├── appendix_d_plots.R                                   # Plot Appendix D figures
│   ├── appendix_d_simulations_fixed_effect_adaptive_1000.R  # Run simulations for figure 14
│   ├── appendix_d_simulations_fixed_effect_equal_1000.R     # Run simulations for figure 14
│   ├── appendix_d_simulations_fixed_sample_adaptive_1000.R  # Run simulations for figure 13
│   ├── appendix_e.R                                         # Simulate model described in Appendix E
│   ├── download_ps_weights.R                                # Re-generate poststratification weights (not necessary)
│   ├── immigrants_plots.R                                   # Plot main example figures (figure 5, 6, 10, 11)
│   └── job_applicants_plots.R                               # Plot supplementary example figures (figures 9, 12)
├── data
│   ├── immigrants_main_bandit.csv     # Raw data from the immigrants (primary) experiment
│   ├── immigrants_main_batch.csv      # Raw data from the immigrants (primary) experiment
│   ├── immigrants_main_metadata.csv   # Raw data from the immigrants (primary) experiment
│   ├── immigrants_main_noconsent.csv  # Raw data from the immigrants (primary) experiment
│   ├── immigrants_main_parameters.csv # Raw data from the immigrants (primary) experiment
│   ├── immigrants_main_pi.csv         # Raw data from the immigrants (primary) experiment
│   ├── immigrants_main_response.csv   # Raw data from the immigrants (primary) experiment
│   ├── immigrants_max_bandit.csv      # Raw data from the immigrants (primary) experiment
│   ├── immigrants_max_batch.csv       # Raw data from the immigrants (primary) experiment
│   ├── immigrants_max_metadata.csv    # Raw data from the immigrants (primary) experiment
│   ├── immigrants_max_noconsent.csv   # Raw data from the immigrants (primary) experiment
│   ├── immigrants_max_parameters.csv  # Raw data from the immigrants (primary) experiment
│   ├── immigrants_max_pi.csv          # Raw data from the immigrants (primary) experiment
│   ├── immigrants_max_response.csv    # Raw data from the immigrants (primary) experiment
│   ├── immigrants_min_bandit.csv      # Raw data from the immigrants (primary) experiment
│   ├── immigrants_min_batch.csv       # Raw data from the immigrants (primary) experiment
│   ├── immigrants_min_metadata.csv    # Raw data from the immigrants (primary) experiment
│   ├── immigrants_min_noconsent.csv   # Raw data from the immigrants (primary) experiment
│   ├── immigrants_min_parameters.csv  # Raw data from the immigrants (primary) experiment
│   ├── immigrants_min_pi.csv          # Raw data from the immigrants (primary) experiment
│   ├── immigrants_min_response.csv    # Raw data from the immigrants (primary) experiment
│   ├── ipums_strata_sizes.RDS         # Poststratification weights for Figure 10
│   ├── job_app_clean_all_phases.csv   # Clean data from the job applicants experiment
|   ├── job_applicants_data_clean_2025_08_01.csv # Same data as above but more columns included
│   ├── README.md                      # More detail on the data!
│   └── simulation-data
│       ├── fixed_effect_adaptive_sim_1000.RDS       # Simulation results for Appendix D, Figure 14
│       ├── fixed_effect_equal_sim_1000.RDS          # Simulation results for Appendix D, Figure 14
│       ├── fixed_sample_adaptive_sim_1000_1000.RDS  # Simulation results for Appendix D, Figure 13
│       └── fixed_sample_adaptive_sim_1000_500.RDS   # Simulation results for Appendix D, Figure 13
├── figures
│   ├── figure10.png
│   ├── figure11a.png
│   ├── figure11b.png
│   ├── figure12a.png
│   ├── figure12b.png
│   ├── figure13.png
│   ├── figure14.png
│   ├── figure5.png
│   ├── figure6.png
│   └── figure9.png
└── README.md
```