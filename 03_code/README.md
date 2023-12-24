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
