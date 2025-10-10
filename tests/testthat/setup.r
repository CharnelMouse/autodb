library(hedgehog)
options(
  warnPartialMatchArgs = TRUE,
  warnPartialMatchDollar = TRUE,
  warnPartialMatchAttr = TRUE
)
if (!isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false"))))
  options(hedgehog.tests = 20)
