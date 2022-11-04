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

gen_df_vary_classes <- function(nrow, ncol, nonempty = FALSE, remove_dup_rows = FALSE) {
  gen_ncol_inc <- gen.sample(seq.int(nonempty, ncol), 1)
  gen_len_inc <- gen.sample(seq.int(nonempty, nrow), 1)
  gen_classes <- generate(for (ncol in gen_ncol_inc) {
    classes <- c("logical", "integer", "numeric", "character")
    gen.sample(classes, ncol, replace = TRUE)
  })
  gen_lst <- generate(
    for (classes in gen_classes) {
      generate(
        for (len_inc in gen_len_inc) {
          lapply(
            classes,
            \(class) gen.sample(
              as(c(FALSE, TRUE), class),
              len_inc,
              replace = TRUE
            )
          ) |>
            setNames(make.unique(rep_len(LETTERS, length(classes))))
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

gen_nonempty_subsequence <- function(x) {
  generate(for (n in gen.int(length(x))) {
    generate(for (sample in gen.sample(x, n)) {
      sort(sample)
    })
  })
}

gen_det <- function(n, attr) {
  gen_nonempty_subsequence(LETTERS[seq_len(n)][-attr])
}

gen_dets <- function(n, attr, max_dets) {
  gen.list(
    gen_det(n, attr),
    from = 0,
    to = min(max_dets, n - 1)
  )
}

gen_unique_dets <- function(n, attr, max_dets) {
  # should also check no redundancy
  generate(for (dets in gen_dets(n, attr, min(max_dets, n - 1))) {
    unique(dets)
  })
}

gen_unnamed_flat_deps <- function(n, max_dets) {
  generate(for (n_attrs in gen.sample(seq.int(0, n), 1)) {
    lapply(
      seq_len(n_attrs),
      function(attr) gen_unique_dets(n_attrs, attr, min(max_dets, n_attrs - 1))
    )
  })
}

gen_flat_deps <- function(n, max_dets) {
  generate(for (unnamed_deps in gen_unnamed_flat_deps(n, max_dets)) {
      attrs <- LETTERS[seq_along(unnamed_deps)]
      names(unnamed_deps) <- attrs
      flatten(list(
        dependencies = unnamed_deps,
        attrs = attrs
      ))
  })
}
