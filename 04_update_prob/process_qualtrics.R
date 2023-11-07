
library(tidyverse)
library(qualtRics)

url <- "https://ca1.qualtrics.com"
api_key <- "3f2XnLK61vaO3nnMcpw4ulWNF8rdlYU8eAHAlAG0"

qualtrics_api_credentials(api_key = api_key, 
                          base_url = url,
                          install = TRUE,
                          overwrite=T)

surveys <- all_surveys() 
pc_id <- surveys[surveys['name']=='Political Candidates',][['id']]

pc_survey <- fetch_survey(surveyID = pc_id, 
                         verbose = TRUE)

names(pc_survey)

for (n in 1:N) { # for N runs
  ad <- 0
  max_random <- 0
  for (i in 1:k) {
    # draw from prior distribution for theta
    # for each arm
    random_theta <- rbeta(
      n = 1,
      shape1 = numbers_of_rewards_1[i] + 1,
      shape2 = numbers_of_rewards_0[i] + 1
    )
    if (random_theta > max_random) {
      # if arm is better than all the previous ones
      # this arm is selected, choose theta
      max_random <- random_theta
      ad <- i
    }
  }
  # after selecting the argmax, add to selected ad
  ads_selected <- append(ads_selected, ad)
  
  # checking with data (what is *observed*)
  reward <- df[n, ad]
  if (reward == 1) {
    numbers_of_rewards_1[ad] <- numbers_of_rewards_1[ad] + 1
  } else {
    numbers_of_rewards_0[ad] <- numbers_of_rewards_0[ad] + 1
  }
  total_reward <- total_reward + reward
}

# observed clicks
numbers_of_rewards_1