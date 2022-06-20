choose_index <- function(keys, df) {
  # Chooses an index / primary key from a list of keys.
  # Order of priority:
  # 1) shortest length
  # 2) has "id" prefix/suffix in the name of any attribute
  # 3) has the attribute furthest to the left in the table
  if (length(keys) == 0)
    return(NA_character_)
  sort_key <- keys[order(lengths(keys))]
  m <- length(sort_key[[1]])
  options <- sort_key[lengths(sort_key) == m]
  for (key in options) {
    for (attr in key) {
      if (any(vapply(
        c("_id", " id", "id _", "id "),
        \(s) grepl(s, tolower(attr), fixed = TRUE),
        logical(1)
      )))
        return(key)
    }
  }
  if (isTRUE(is.na(df)))
    return(options[[1]])

  for (col in colnames(df)) {
    includes <- options[vapply(options, \(opt) col %in% opt, logical(1))]
    if (length(includes) == 1)
      return(includes[[1]])
    if (length(includes) > 1)
      options <- includes
  }
  options[[1]]
}
