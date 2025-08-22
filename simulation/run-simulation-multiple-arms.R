# library(tidyverse)
# library(furrr)
# library(tictoc)
# plan(multisession, workers = 8)
# knitr::opts_chunk$set(echo = TRUE)
# 
# theme_set(theme_bw())
# library(foreach)
# library(doParallel)
# library(doRNG)
# library(assertr)
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# 
# for (n in seq(100, 20000, 200)) {
#   
# }