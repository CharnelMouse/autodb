df_join <- function(x, y, ...) {
  # can't sort since there might be recursive columns
  merge(x, y, sort = FALSE, ...)
}
