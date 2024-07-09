There are two levels to the data files in here.

# Study Phase

There were three phases of the study: the main phase (encapsulating warmup, adaptive max, and adaptive min), the max validation phase, and the min validation phase.

- `immigrants_main_...csv`: These files correspond to the main phase
- `immigrants_max_...csv`: These files correspond to the max validation phase
- `immigrants_min_...csv`: These files correspond to the min validation phase

The data for each of these three phases are also backed up in separate volumes in AWS.

# Phase-level Data

In each phase of the study, the following seven tables were collected:
- **Bandit**: A small table that contains the `label` and `id` for each bandit arm.
- **Batch**: A table that contains the `id` and some corresponding (but useless) information for each batch of the study. This table is really only useful as a linking table
since several other tables use `batch_id` as a foreign key.
- **Metadata**: This contains all the immigrant profile metadata that was shown to respondents. Each bandit arm had two profiles, so this table has (2 * `nrow(Bandit)`) rows.
- **NoConsent**: This contains sparse entries for all respondents who did not consent to participate. For these folks we collected no data, so this is really only useful to
know the N of how many refused to consent.
- **Parameters**: Each (batch x arm) from has a set of posterior Beta parameters. Each row in this table has two foreign keys: `arm_id` linking the parameters
to one of the bandit arms, and `batch_id` linking the parameters to a specific batch.
- **Pi**: Exactly like the **Parameters** table except instead of posterior parameters, this contains the MC calculated probability of discriminating (or not discriminating
depending on the phase).
- **Response**: This contains all the responses and corresponding information.

# Usage

As a simple example, to get a table of responses for all the phases of the study and filter out the bad responses, do the following:
```r
library(dplyr)
library(here)
library(readr)

responses_main <- read_csv(here("data/immigrants_main_response.csv"))
responses_max <- read_csv(here("data/immigrants_max_response.csv")) |> mutate(phase = "validation_max")
responses_min <- read_csv(here("data/immigrants_min_response.csv")) |> mutate(phase = "validation_min")

responses <- responses_main |>
  bind_rows(responses_max) |>
  bind_rows(responses_min) |>
  filter(!garbage)
```
