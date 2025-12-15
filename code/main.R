## This script installs all dependencies and plots all figures from the paper
## To be clear, our paper has no tables, only figures.

zz <- file("log.txt", open = "wt")
sink(zz, split = TRUE)
sink(zz, type = "message")
on.exit({
  try(sink(type = "message"), silent = TRUE)
  try(sink(), silent = TRUE)
  close(zz)
}, add = TRUE)

.start_time <- Sys.time()

# Replicate all figures from the immigrants experiment
cat("\n---------------------------- ./code/immigrants_plots.R -------\n")
source("./code/immigrants_plots.R")

# Replicate all figures from the job applicants experiment
cat("\n---------------------------- ./code/job_applicants_plots.R ---\n")
source("./code/job_applicants_plots.R")

# Replicate figures in Appendix C
cat("\n---------------------------- ./code/appendix_c.R -------------\n")
source("./code/appendix_c.R")

# Replicate simulations for Appendix D

# ----------------------------------------------------------------------------------------------------

### NOTE: This part is what takes BY FAR the longest (hours of compute).
### If you uncomment lines 36, 39, and 42, the simulations for Appendix D will replicate.
### However, we recommend just using the saved intermediate files and this will run in ~ 1 minute.

# cat("\n---------------------------- ./code/appendix_d_simulations_fixed_effect_adaptive_1000.R --\n")
# source("./code/appendix_d_simulations_fixed_effect_adaptive_1000.R")

# cat("\n---------------------------- ./code/appendix_d_simulations_fixed_effect_equal_1000.R -----\n")
# source("./code/appendix_d_simulations_fixed_effect_equal_1000.R")

# cat("\n---------------------------- ./code/appendix_d_simulations_fixed_sample_adaptive_1000.R -----\n")
# source("./code/appendix_d_simulations_fixed_sample_adaptive_1000.R")

# ----------------------------------------------------------------------------------------------------

# Replicate figures in Appendix D
cat("\n---------------------------- ./code/appendix_d_plots.R -------\n")
source("./code/appendix_d_plots.R")

cat("\n--------------------------------------------------------------\n")
cat("Timestamp results were generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")

cat("\n--------------------------------------------------------------\n")
cat(
  "Total runtime (minutes):",
  round(as.numeric(difftime(Sys.time(), .start_time, units = "mins")), 2),
  "\n"
)