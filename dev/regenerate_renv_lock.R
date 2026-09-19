# Regenerate renv.lock on a machine where ALL dependencies are installed.
# The lockfile in the repository was generated in a cloud sandbox that could not
# reach CRAN, so 'dampack' was not installed and is NOT pinned there.
# Run from the package root:
#   install.packages(c("dampack", "DT", "shiny", "testthat", "renv"))
#   source("dev/regenerate_renv_lock.R")
renv::snapshot(project = ".", type = "explicit", dev = TRUE,
               lockfile = "renv.lock", prompt = FALSE)
