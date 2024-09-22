with_number <- function(n, prefix, suffix_1, suffix_else) {
  paste0(n, " ", by_number(n, prefix, suffix_1, suffix_else))
}

by_number <- function(n, prefix, suffix_1, suffix_else) {
  paste0(prefix, if (n == 1L) suffix_1 else suffix_else)
}

stop_with_elements_if <- function(
  conds,
  mess,
  prefix = "element",
  suffix_1 = "",
  suffix_else = "s"
) {
  w <- which(conds)
  if (any(conds))
    stop(paste(
      paste0(mess, ":"),
      by_number(
        length(w),
        prefix,
        suffix_1,
        suffix_else
      ),
      toString(w)
    ))
}

stop_with_values_if <- function(
  x,
  conds,
  mess,
  prefix = "element",
  suffix_1 = "",
  suffix_else = "s",
  unique = TRUE
) {
  v <- x[conds]
  if (unique)
    v <- unique(v)
  if (length(v) > 0)
    stop(paste(
      paste0(mess, ":"),
      by_number(
        length(v),
        prefix,
        suffix_1,
        suffix_else
      ),
      toString(v)
    ))
}
