# 01_get_qualtrics_data.R

**Purpose:** loads the data from Qualtrics via the API.

1. API key is stored in `config` file. Retrieved via `config::get()`.
2. We exclude test data (currently, anything created before `2023-12-14-17-20-00`). This should leave us with only 50 observations in the data.
3. We then implement the following checks:
  a. The Prolific ID uniquely identifies the data.
  b. 
