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
