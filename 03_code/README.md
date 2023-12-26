# 00_produce_weights.R
1. Load data from IPUMS US via the API.
- Define the data extract. Sample is from US2022a. The variables we need are AGE, SEX, RACE, STATEFIP, HISPAN.
- Submit, wait, and then download the data from IPUMS
2. Check AGE is an integer; rename to `age`.
3. Create `race` variable in the following way (based on codebook values from [IPUMS website](https://usa.ipums.org/usa-action/variables/RACE#description_section)):
- RACE = 1, `race` = "White"
- RACE = 2, `race` = "Black or African American"
- RACE = 3, `race` = "American Indian or Alaska Native"
- RACED in the following list of values (400, 410, 420, 500, 600, 610, 620, 640, 641, 642, 643, 660, 661, 662, 663, 664, 665, 666, 667, 669, 670, 671, 673, 674, 675, 676, 677, 678, 679), `race` = "Asian". Note, these categories are: 400=Chinese, 410=Taiwanese, 420=Chinese and Taiwanese, 500=Japanese, 600=Filipino, 610=Asian Indian (Hindu 1920_1940), 620=Korean, 640=Vietnamese, 641=Bhutanese, 642=Mongolian, 643=Nepalese, 660=Cambodian, 661=Hmong, 662=Laotian, 663=Thai, 
664=Bangladeshi, 665=Burmese, 666=Indonesian, 667=Malaysian, 669=Pakistani, 670=Sri Lankan, 671=All other Asian, n.e.c., 673=Chinese and Japanese, 674=Chinese and Filipino, 675=Chinese and Vietnamese, 676=Chinese and Asian write_in; Chinese and Other Asian, 677=Japanese and Filipino, 678=Asian Indian and Asian write_in, 679=Other Asian race combinations.
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

# 01_get_qualtrics_data.R

**Purpose:** loads the data from Qualtrics via the API.

1. API key is stored in `config` file. Retrieved via `config::get()`.
2. We exclude test data (currently, anything created before `2023-12-14-17-20-00`). This should leave us with only 50 observations in the data.
3. We then implement the following checks:
- The Prolific ID uniquely identifies the data.
- The `Survey` variable only indicates that data is coming from an `IP Address`.
- Check consent; drop individuals who do not consent.
- Check completion; drop individuals for whom `Finished` does not equal `TRUE`.
- Check location screen; drop individuals who say they are not in the US (this should *not* happen).
- Check commitment questions; do not drop any individuals.
4. Create a new, randomly generated unique ID for the data.

# 02_prepare_qualtrics_data

1. Load survey data
2. Produce summary of attention check question
3. Create a binary outcome variable (based on `rnum_age` and whether they select "Candidate 1" or "Candidate 2"; binary outcome denoting whether the respondent selected the younger candidate.
4. Create the two variables `context` and `context_label` (note, `context` is an ordered factor with values from 1-8):
- When `rnum <= pi1`, assign `context` = 1 and `context_label` = "white_female_high"
- When `rnum > pi1 & rnum <= pi2`, assign `context` = 2 and `context_label` = "white_female_low"
- When `rnum > pi2 & rnum <= pi3`, assign `context` = 3 and `context_label` = "black_female_high"
- When `rnum > pi3 & rnum <= pi4`, assign `context` = 4 and `context_label` = "black_female_low"
- When `rnum > pi4 & rnum <= pi5`, assign `context` = 5 and `context_label` = "black_male_high"
- When `rnum > pi5 & rnum <= pi6`, assign `context` = 6 and `context_label` = "black_male_low"
- When `rnum > pi6 & rnum <= pi7`, assign `context` = 7 and `context_label` = "white_male_high"
- When `rnum > pi7`, assign `context` = 8 and `context_label` = "white_male_low"
5. Then create demographic variables:
- `hispanic` if QD4 in the survey is equal to "Yes" = `TRUE`, else `FALSE`
- `female` if QD5 in the survey is equal to "Female" = `TRUE`, else `FALSE`
- Confirm `QD2_1_TEXT` is numeric.
- For `race`: first count the number of non-missing values for all variables that start with `QD3_`. Do this by creating indicator variables ending in `race_num` if any of the `QD3_` variables is non-missing. Sum the indicator variables rowwise and store in the variable called `race_count`.
- If `race_count` > 1, `race` = "Two or More Races," otherwise:
- If `QD3_1` is non-missing, `race` = "American Indian or Alaska Native"
- If `QD3_2` is non-missing, `race` = "Asian"
- If `QD3_3` is non-missing, `race` = "Black or African American"
- If `QD3_4` is non-missing, `race` = "Native Hawaiian or Other Pacific Islander"
- If `QD4_5` is non-missing, `race` = "White"
- If `QD4_6` is non-missing, `race` = "Other"
- If `QD4_7` is non-missing, `race` = "Prefer not to disclose"
- `race` is a factor variable with the following levels: "Black or African American", "White", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Other Pacific Islander", "Other", "Two or More Races", "Prefer not to disclose"
- Create `drop_demo_flag`: this is an indicator variable that flags any respondent who checked "Prefer not to disclose" for `QD4` (hispanicity), `QD5` (sex), `QD2` (age), or `QD3_7` (race category = "Prefer not to disclose"). We will drop respondents based on this flag prior to doing poststratification.

# 03_analysis.R
1. Load survey data.
2. Create two vectors `c_val` and `c_desc` storing the unique profile contexts and profile context labels. This will allow us to flexibly work with data that has any range of profile contexts.
3. Load population weights from the ACS.
4. Simple mean estimate: grouping by `context` and `context_label`, produce mean estimates of the outcome variable `chose_younger`.
5. Produce the SE and 95% Confidence Interval values based on the following formulas:
- `se = sqrt((mean_estimate * (1 - mean_estimate)) / length(chose_younger))`
- `ci_min = mean_estimate - (qnorm(.975) * se)`
- `ci_max = mean_estimate + (qnorm(.975) * se)`
6. Poststratification:
- Prior to poststratifying, keep only respondents for whom `drop_demo_flag = FALSE`
- Filter the weights to include only race categories that are in the study dataset
- `glm_drop_cons_factors` is a function that checks if race has two or more levels, otherwise it does not include it in the regression formula.
- Run a logistic regression using `glm` with `family = "binomial"`
- `compute_weighted_prob` is a function that drops race categories from the population weights if they are not present in a subset of the data for a given `context` and `context_label`. This occurs prior to running predict. It then predicts the probability of choosing the younger candidate and returns a weighted mean.
- Produce point estimates of the poststratified results.
7. Bootstrapping:
- Filter for the correct context (we will iterate through all contexts and labels in the data). Sample the data 1,000 times using `bootstrap` from the `modelr` package.
- Apply `glm_drop_cons_factor` to each bootstrapped dataset. Then apply `compute_weighted_prob` to get the weighted probabilities.
- Compute the SE as the standard deviation of the weighted probabilities and store in a tibble.
- With this output, which is a tibble produced by iterating across all `context` and `context_label` values in the data, join to the point estimate data.
- Compute the 95% Confidence Interval values in the same ways as for the simple mean estimate.
9. Bind rows of both the simple mean and bootstrapped estimates.
