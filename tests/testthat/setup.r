library(hedgehog)
options(
  warnPartialMatchArgs = TRUE,
  warnPartialMatchDollar = TRUE,
  warnPartialMatchAttr = TRUE
)
env <- Sys.getenv("NOT CRAN")
on_cran <- if (identical(env, "")) {
  !interactive()
}else
  !isTRUE(as.logical(env))
if (on_cran)
  options(hedgehog.tests = 20)
