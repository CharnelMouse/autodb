nudge <- read.csv(
  "data-raw/nudge.csv",
  colClasses = c(
    "integer",
    "integer",
    "integer",
    "character",
    "character",
    "integer",
    "integer",
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
location_categories <- c("outside US", "inside US")
nudge$location <- factor(
  location_categories[nudge$location + 1L],
  location_categories
)
population_categories <- c("children and/or adolescents", "adults")
nudge$population <- factor(
  population_categories[nudge$population + 1L],
  population_categories
)
usethis::use_data(nudge, overwrite = TRUE)
