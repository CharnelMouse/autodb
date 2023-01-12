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
  expect_true(df_equiv(new, original))
}

gen_df <- function(nrow, ncol, nonempty = FALSE, remove_dup_rows = FALSE) {
  gen_ncol_inc <- gen.sample(seq.int(nonempty, ncol), 1)
  gen_len_inc <- gen.sample(seq.int(nonempty, nrow), 1)
  gen_lst <- generate(
    for (n_col_inc in gen_ncol_inc) {
      generate(
        for (len_inc in gen_len_inc) {
          generate(
            for (nms in gen_attr_names(n_col_inc, 9)) {
              rep(
                list(gen.sample(c(FALSE, TRUE, NA), len_inc, replace = TRUE)),
                n_col_inc
              ) |>
                setNames(nms)
            }
          )
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
          generate(
            for (nms in gen_attr_names(length(classes), 9)) {
              lapply(
                classes,
                \(class) gen.sample(
                  as(c(FALSE, TRUE, NA), class),
                  len_inc,
                  replace = TRUE
                )
              ) |>
                setNames(nms)
            }
          )
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

gen_attr_name <- function(len) {
  generate(for (clen in gen.int(len)) {
    generate(for (attr_name in gen.sample(c(letters, "_", " ", "."), clen)) {
      paste(attr_name, collapse = "")
    })
  })
}

gen_attr_names <- function(n, len) {
  generate(for (attr_names in gen.c(gen_attr_name(len), of = n)) {
    make.unique(as.character(attr_names)) # as.character for length-0 NULL value
  })
}

gen_subsequence <- function(x) {
  generate(for (n in gen.sample(seq.int(0, length(x)), 1)) {
    generate(for (sample in gen.sample(x, n)) {
      sample[order(match(sample, x))]
    })
  })
}

gen_det <- function(n_attrs, n) {
  gen_subsequence(setdiff(seq_len(n_attrs), n))
}

gen_dets <- function(n_attrs, n, max_dets) {
  gen.list(
    gen_det(n_attrs, n),
    from = 0,
    to = min(max_dets, n_attrs - 1)
  )
}

gen_unique_dets <- function(n_attrs, n, max_dets) {
  # should also check no redundancy
  generate(for (dets in gen_dets(
    n_attrs,
    n,
    min(max_dets, n_attrs - 1)
  )) {
    unique(dets)
  })
}

gen_unnamed_flat_deps <- function(n_attrs, max_dets) {
  md <- min(max_dets, n_attrs - 1)
  gen.structure(lapply(
    seq_len(n_attrs),
    function(n) {
      gen_unique_dets(n_attrs, n, md)
    }
  ))
}

gen_flat_deps <- function(n, max_dets, len = 9) {
  generate(for (attrs in gen_attr_names(n, len)) {
    generate(for (unnamed_deps in gen_unnamed_flat_deps(length(attrs), max_dets)) {
      unindexed_deps <- lapply(unnamed_deps, \(ud) lapply(ud, \(cs) attrs[cs]))
      names(unindexed_deps) <- attrs
      flatten(list(
        dependencies = unindexed_deps,
        attrs = attrs
      ))
    })
  })
}
