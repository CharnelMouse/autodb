choose_index <- function(keys) {
  # Chooses an index / primary key from a list of integer-type keys.
  # Order of priority:
  # 1) shortest length
  # 2) has the attribute furthest to the left, i.e. smallest minimum
  # Input must not have duplicated keys.
  if (length(keys) == 0)
    return(NA_integer_)
  lens <- lengths(keys)
  min_length <- min(lens)
  options <- keys[lens == min_length]

  comp <- options
  while (length(options) > 1) {
    min_els <- vapply(comp, min, integer(1))
    mm <- min(min_els)
    options <- options[min_els == mm]
    comp <- lapply(comp[min_els == mm], setdiff, mm)
  }
  options[[1]]
}
