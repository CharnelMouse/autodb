library(hedgehog)
if (!isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false"))))
  options(hedgehog.tests = 20)
