#  Data-Adaptive Experimentation to Find Contexts with the Most and Least Discrimination

## Internal folder structure:
* `./01_input`: input data files
* `./02_output`: output data files and formatted resumes
* `./03_automate_profiles`: generate resumes and political candidate tables automatically
* `./04_update_prob`: update treatment assignment probabilities

Create `./config.yml` file:
```
default:
  api_token:
  pol_candidates_survey_id:
  job_applicants_survey_id:
  datacenter_id:
```
## Information on accessing Qualtrics data with API
* https://cran.r-project.org/web/packages/qualtRics/vignettes/qualtRics.html
