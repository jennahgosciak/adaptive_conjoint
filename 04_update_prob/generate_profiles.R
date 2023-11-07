
library(tidyverse)

# To do: Formalize rules for choosing these names

race_gender <- data.frame(
  race = "white",
  gender = "man",
  name = c("Thomas Wagner","Richard Hoffman")
) %>%
  bind_rows(data.frame(
    race = "white",
    gender = "woman",
    name = c("Mary Meyer","Sarah Schmidt")
  )) %>%
  bind_rows(data.frame(
    race = "black",
    gender = "man",
    name = c("Jermaine Wood","Darnell Jackson")
  )) %>%
  bind_rows(data.frame(
    race = "black",
    gender = "woman",
    name = c("Lakisha Jackson","Tamika Williams")
  )) %>%
  mutate(person = rep(c("person_1","person_2"),n() / 2)) %>%
  pivot_wider(names_from = "person",
              values_from = "name")

political_experience <- data.frame(
  treatment = c("has_political_experience","no_political_experience"),
  person_1 = c("State Legislator","None"),
  person_2 = c("Member of Congress","None")
)

ages <- data.frame(person_1 = c(42,71),
                   person_2 = c(71,42))
for (i in 1:nrow(race_gender)) {
  for (j in 1:nrow(political_experience)) {
    #for (k in 1:nrow(ages)) {
      cat(paste("## Context:", paste(race_gender[i,1:2], collapse = " "), political_experience[j,1], collapse = " "))
      cat("\n")
      profile_case <- rbind(c("Name",race_gender[i,3:4]),
                            #c("Age",ages[k,]),
                            c("Age","[age1]","[age2]"),
                            c("Political Experience",political_experience[j,2:3]),
                            c("Career Experience","Educator","Small Business Owner"))
      colnames(profile_case) <- c("","Candidate 1","Candidate 2")
      print(xtable::xtable(profile_case), include.rownames = F)
    #}
  }
}
  

x <- profile_values %>%
  filter(race == "white" & gender == "man" & political_experience == "experienced")


  
  
  