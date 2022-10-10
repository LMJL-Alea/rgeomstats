# global reference to geomstats (will be initialized in .onLoad)
gs <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  # use superassignment to update global reference to geomstats
  # Sys.setenv(GEOMSTATS_BACKEND = "autograd")
  gs <<- reticulate::import("geomstats", delay_load = TRUE, convert = TRUE)
}
