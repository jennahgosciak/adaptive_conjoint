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
4. Grouping by `context` and `context_label`, produce mean estimates of the outcome variable `chose_younger`.
5. Produce the SE and 95% Confidence Interval values based on the following formulas:
- `se = sqrt((mean_estimate * (1 - mean_estimate)) / length(chose_younger))`
- `ci_min = mean_estimate - (qnorm(.975) * se)`
- `ci_max = mean_estimate + (qnorm(.975) * se)`
6. Prior to poststratifying, keep only respondents for whom `drop_demo_flag = FALSE`
