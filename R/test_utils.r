expect_superset_of_dependency <- function(dep1, dep2) {
  dep1 <- dep1[names(dep2)]
  stopifnot(sort(names(dep1)) == sort(names(dep2)))
  dep2_in_dep1 <- mapply(
    function(one, two) {
      all(vapply(
        two,
        \(two_el) {
          any(vapply(one, \(one_el) all(two_el %in% one_el), logical(1)))
        },
        logical(1)
      ))
    },
    dep1,
    dep2
  )
  testthat::expect_true(all(dep2_in_dep1))
}

expect_identical_unordered_table <- function(new, original) {
  stopifnot(all(names(original) %in% names(new)))
  df2_reordered <- new[, names(original), drop = FALSE]
  expect_identical(
    `rownames<-`(df2_reordered[do.call(order, df2_reordered), ], NULL),
    `rownames<-`(original[do.call(order, original), ], NULL)
  )
}

gen_df <- function(nrow, ncol, nonempty = FALSE, remove_dup_rows = FALSE) {
  gen_ncol_inc <- gen.sample(seq.int(nonempty, ncol), 1)
  gen_len_inc <- gen.sample(seq.int(nonempty, nrow), 1)
  gen_lst <- generate(
    for (n_col_inc in gen_ncol_inc) {
      generate(
        for (len_inc in gen_len_inc) {
          rep(
            list(gen.sample(c(FALSE, TRUE), len_inc, replace = TRUE)),
            n_col_inc
          ) |>
            setNames(make.unique(rep_len(LETTERS, n_col_inc)))
        }
      )
    }
  )
  generate(for (lst in gen_lst) {
    if (remove_dup_rows)
      unique(as.data.frame(lst))
    else
      as.data.frame(lst)
  })
}

gen_nonempty_df <- function(nrow, ncol) gen_df(nrow, ncol, nonempty = TRUE)
