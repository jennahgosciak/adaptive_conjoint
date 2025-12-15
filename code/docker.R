library(tugboat)

dockerfile <- create(
  FROM = paste0("posit/r-base:", R.version$major, ".", R.version$minor, "-noble"),
  exclude = c("renv/", "replication/", ".Rprofile", ".Renviron")
)

build(
  image_name = "adaptive_conjoint",
  push = TRUE,
  dh_username = Sys.getenv("DOCKER_UNAME"),
  dh_password = Sys.getenv("DOCKER_PWD")
)