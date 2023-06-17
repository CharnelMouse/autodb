with_number <- function(n, prefix, suffix_1, suffix_else) {
  paste0(n, " ", prefix, if (n == 1L) suffix_1 else suffix_else)
}
