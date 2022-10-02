nudge <- read.csv(
  "data-raw/nudge.csv",
  colClasses = c(
    "integer",
    "integer",
    "integer",
    "character",
    "character",
    "integer",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "integer",
    "integer",
    "integer",
    "integer",
    "integer",
    "integer",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "integer",
    "integer"
  )
)
bool_cols <- c("binary_outcome", "approximation", "wansink")
nudge[, bool_cols] <- as.logical(unlist(nudge[, bool_cols]))
usethis::use_data(nudge, overwrite = TRUE)

nudge_database_nonumeric <- autonormalise::autonorm(
  nudge,
  accuracy = 1,
  name = "Nudge",
  check_key = FALSE,
  exclude_class = "numeric"
)
usethis::use_data(nudge_database_nonumeric, overwrite = TRUE)
