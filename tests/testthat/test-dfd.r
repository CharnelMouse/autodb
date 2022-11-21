# Possible additional properties
# Invariants
# + Running DFD twice gives equivalent results
# doesn't change results
# + FDs with the same RHS can't be LHSs where one is a subset of the other
# + invariant to permuting/changing values for an attribute (incl. NA)
# + invariant to changing type for an attribute
# + invariant to changing attribute order, except for order in dep elements and
# determinant sets
# + invariant to accuracy within same nrow threshold
# + removing a column removes FDs involving that column, the rest stay the same
# + removing a row keeps subsets of all previous FDs, might add more
# + doesn't have attribute in determinant sets if excluded
# + excluding type === excluding all attributes of that type
# Metamorphic/model
# + change attribute names then DFD === DFD then change attribute names

# Various tests should have random accuracy draws, probably with lots of
# weight on 1

library(R.utils)
library(hedgehog)

describe("dfd", {
  expect_equiv_deps <- function(deps1, deps2) {
    expect_identical(deps1$attrs, deps2$attrs)
    expect_identical(names(deps1$dependencies), names(deps2$dependencies))
    for (n in seq_along(deps1$dependencies))
      expect_setequal(deps1$dependencies[[n]], deps2$dependencies[[n]])
  }
  expect_permutation_deps <- function(deps1, deps2) {
    expect_setequal(deps1$attrs, deps2$attrs)
    perm <- match(deps1$attrs, deps2$attrs)
    expect_identical(names(deps1$dependencies), names(deps2$dependencies)[perm])
    for (n in seq_along(deps1$dependencies))
      expect_setequal(
        deps1$dependencies[[n]],
        lapply(
          deps2$dependencies[[perm[n]]],
          \(nms) nms[order(match(nms, deps1$attrs))]
        )
      )
  }
  expect_subset_deps <- function(deps1, deps2) {
    expect_identical(deps1$attrs, deps2$attrs)
    expect_identical(names(deps1$dependencies), names(deps2$dependencies))
    for (n in seq_along(deps1$dependencies))
      expect_true(all(is.element(deps1$dependencies[[n]], deps2$dependencies[[n]])))
  }
  expect_equiv_deps_except_names <- function(deps1, deps2) {
    new_names <- deps2$attrs
    new_deps <- deps1
    names(new_deps$dependencies) <- new_names
    new_deps$dependencies <- lapply(
      new_deps$dependencies,
      \(ds) lapply(ds, \(attrs) deps2$attrs[match(attrs, deps1$attrs)])
    )
    new_deps$attrs <- new_names
    expect_equiv_deps(new_deps, deps2)
  }
  expect_equiv_non_removed_attr_deps <- function(deps1, deps2) {
    removed_attr <- setdiff(deps1$attrs, deps2$attrs)
    expect_length(removed_attr, 1)
    filtered <- deps1
    filtered$attrs <- setdiff(deps1$attrs, removed_attr)
    filtered$dependencies <- filtered$dependencies[
      names(filtered$dependencies) != removed_attr
    ]
    filtered$dependencies <- lapply(
      filtered$dependencies,
      Filter,
      f = function(ds) !is.element(removed_attr, ds)
    )
    expect_equiv_deps(filtered, deps2)
  }
  expect_det_subsets_kept <- function(deps1, deps2) {
    expect_identical(deps1$attrs, deps2$attrs)
    expect_identical(names(deps1$dependencies), names(deps2$dependencies))
    for (n in seq_along(deps1$dependencies))
      expect_true(all(vapply(
        deps1$dependencies[[n]],
        \(ds) any(vapply(
          deps2$dependencies[[n]],
          \(ds2) all(is.element(ds2, ds)),
          logical(1)
        )),
        logical(1)
      )))
  }
  terminates_then <- function(fn, accuracy, ...) {
    function(df) {
      res <- withTimeout(dfd(df, accuracy, ...), timeout = 5, onTimeout = "silent")
      expect_true(!is.null(res))
      fn(res)
    }
  }
  both_terminate_then <- function(fn, accuracy, ...) {
    function(df1, df2) {
      res1 <- withTimeout(dfd(df1, accuracy, ...), timeout = 5, onTimeout = "silent")
      expect_true(!is.null(res1))
      res2 <- withTimeout(dfd(df2, accuracy, ...), timeout = 5, onTimeout = "silent")
      expect_true(!is.null(res2))
      fn(res1, res2)
    }
  }

  it("gives a deterministic result, except for per-dependent dependency order", {
    two_copies <- function(fn) {
      function(df) {
        fn(df, df)
      }
    }
    forall(
      gen_df(4, 6),
      two_copies(both_terminate_then(expect_equiv_deps, accuracy = 1))
    )
  })
  it("returns dependencies where shared dependent <=> not sub/supersets for determinants", {
    has_non_nested_determinant_sets <- function(deps) {
      for (det_sets in deps$dependencies) {
        len <- length(det_sets)
        if (len <= 1)
          succeed()
        else{
          for (n in seq_len(max(0, len - 1))) {
            for (m in seq.int(n + 1L, len)) {
              expect_true(length(setdiff(det_sets[[n]], det_sets[[m]])) > 0)
              expect_true(length(setdiff(det_sets[[m]], det_sets[[n]])) > 0)
            }
          }
        }
      }
    }
    forall(gen_df(4, 6), terminates_then(has_non_nested_determinant_sets, 1))
  })
  it("is invariant to an attribute's values being permuted", {
    gen_perm <- function(vals) {
      uniq <- sort(unique(vals))
      matches <- match(vals, uniq)
      pool <- union(uniq, NA)
      generate(for (perm in gen.sample(pool, length(uniq))) {
        perm[matches]
      })
    }
    gen_df_and_value_perm <- function(
      nrow,
      ncol,
      remove_dup_rows = FALSE
    ) {
      generate(for (df in gen_df(nrow, ncol, nonempty = TRUE, remove_dup_rows)) {
        generate(for (attr in gen.int(ncol(df))){
          generate(for (permuted_attr in gen_perm(df[, attr])) {
            permed <- df
            permed[, attr] <- permuted_attr
            list(df, permed)
          })
        })
      })
    }
    forall(
      gen_df_and_value_perm(4, 6),
      both_terminate_then(expect_equiv_deps, 1),
      curry = TRUE
    )
  })
  it("is invariant to an attribute's class being changed (without exclusions)", {
    gen_df_and_type_change <- function(
      nrow,
      ncol,
      remove_dup_rows = FALSE
    ) {
      classes <- c("logical", "integer", "numeric", "character")
      generate(for (df in gen_df(nrow, ncol, nonempty = TRUE, remove_dup_rows)) {
        generate(for (attr in gen.int(ncol(df))) {
          generate(for (new_class in gen.element(
            setdiff(classes, class(df[, attr])))
          ) {
            permed <- df
            permed[, attr] <- as(permed[, attr], new_class)
            list(df, permed)
          })
        })
      })
    }
    forall(
      gen_df_and_type_change(4, 6),
      both_terminate_then(expect_equiv_deps, 1),
      curry = TRUE
    )
  })
  it("is invariant, under reordering, to attributes being reordered", {
    gen_df_and_attr_perm <- function(
      nrow,
      ncol,
      remove_dup_rows = FALSE
    ) {
      generate(for (df in gen_df(nrow, ncol, nonempty = TRUE, remove_dup_rows)) {
        generate(for (perm in sample.int(ncol(df))) {
          list(df, df[, perm, drop = FALSE])
        })
      })
    }
    forall(
      gen_df_and_attr_perm(4, 6),
      both_terminate_then(expect_permutation_deps, 1),
      curry = TRUE
    )
  })
  it("loses FDs involving a removed attribute, keeps the rest", {
    gen_df_and_remove_col <- function(nrow, ncol, remove_dup_rows = FALSE) {
      generate(for (df in gen_df(nrow, ncol, nonempty = TRUE, remove_dup_rows)) {
        generate(for (n in gen.int(ncol(df))) {
          list(df, df[, -n, drop = FALSE])
        })
      })
    }
    forall(
      gen_df_and_remove_col(4, 6),
      both_terminate_then(expect_equiv_non_removed_attr_deps, 1),
      curry = TRUE
    )
  })
  it("is invariant to changes of accuracy within same required row count", {
    gen_df_and_accuracy_nrow <- function(nrow, ncol, remove_dup_rows = FALSE) {
      generate(for (df in gen_df(nrow, ncol, nonempty = TRUE, remove_dup_rows)) {
        generate(for (n in gen.int(nrow(df))) {
          prop <- n/nrow(df)
          low <- (n - 1)/nrow(df) + 1e-9
          list(df, low, prop)
        })
      })
    }
    both_bounds_terminate_then <- function(fn, ...) {
      function(df, low, high) {
        res1 <- withTimeout(dfd(df, low, ...), timeout = 5, onTimeout = "silent")
        expect_true(!is.null(res1))
        res2 <- withTimeout(dfd(df, high, ...), timeout = 5, onTimeout = "silent")
        expect_true(!is.null(res2))
        fn(res1, res2)
      }
    }
    forall(
      gen_df_and_accuracy_nrow(4, 6),
      both_bounds_terminate_then(expect_equiv_deps),
      curry = TRUE
    )
  })
  it("keeps subsets of all FDs if a row is removed, might have more", {
    gen_df_and_remove_row <- function(nrow, ncol) {
      generate(for (df in gen_df(nrow, ncol, nonempty = TRUE, remove_dup_rows = TRUE)) {
        generate(for (n in gen.element(seq_len(nrow(df)))) {
          list(df, df[-n, , drop = FALSE])
        })
      })
    }
    forall(
      gen_df_and_remove_row(4, 6),
      both_terminate_then(expect_det_subsets_kept, 1),
      curry = TRUE
    )
  })
  it("dfd -> change attribute names is equivalent to change names -> dfd", {
    gen_df_and_name_change <- function(nrow, ncol, remove_dup_rows = FALSE) {
      generate(for (df in gen_df(nrow, ncol, nonempty = TRUE, remove_dup_rows)) {
        generate(for (new_names in gen.sample(LETTERS, ncol(df))) {
          list(df, stats::setNames(df, new_names))
        })
      })
    }
    forall(
      gen_df_and_name_change(4, 6),
      both_terminate_then(expect_equiv_deps_except_names, 1),
      curry = TRUE
    )
  })
  it("doesn't have an excluded attribute in any determinant sets", {
    gen_df_and_exclude <- function(nrow, ncol, remove_dup_rows = FALSE) {
      generate(for (df in gen_df(nrow, ncol, nonempty = TRUE, remove_dup_rows)) {
        list(df, gen.sample(names(df), 1))
      })
    }
    terminates_with_exclusion_then <- function(fn, accuracy, ...) {
      function(df, attr) {
        deps <- withTimeout(
          dfd(df, accuracy, exclude = attr, ...),
          timeout = 5,
          onTimeout = "silent"
        )
        expect_true(!is.null(deps))
        fn(deps, attr)
      }
    }
    exclusion_not_in_determinant_sets <- function(deps, attr) {
      for (det_sets in deps$dependencies) {
        for (det_set in det_sets) {
          expect_false(is.element(attr, det_set))
        }
      }
    }
    forall(
      gen_df_and_exclude(4, 6),
      terminates_with_exclusion_then(
        exclusion_not_in_determinant_sets,
        accuracy = 1
      ),
      curry = TRUE
    )
  })
  it("gives same result from excluding class and exclude attributes with that class", {
    exclude_and_exclude_class_terminate_then <- function(fn, accuracy, class, ...) {
      function(df) {
        attrs_with_class <- names(df)[vapply(df, inherits, logical(1), class)]
        deps1 <- withTimeout(
          dfd(df, accuracy, exclude = attrs_with_class, ...),
          timeout = 5,
          onTimeout = "silent"
        )
        expect_true(!is.null(deps1))
        deps2 <- withTimeout(
          dfd(df, accuracy, exclude_class = class, ...),
          timeout = 5,
          onTimeout = "silent"
        )
        expect_true(!is.null(deps2))
        fn(deps1, deps2)
      }
    }
    forall(
      gen_df_vary_classes(4, 6), # need to generate dfs with differing classes
      exclude_and_exclude_class_terminate_then(
        expect_equiv_deps,
        accuracy = 1,
        "logical"
      )
    )
  })
  it("gives dependencies for unique attributes (in case don't want them as key)", {
    df <- data.frame(A = 1:3, B = c(1, 1, 2), C = c(1, 2, 2))
    deps <- dfd(df, 1)
    expect_identical(deps$dependencies$A, list(c("B", "C")))
  })
  it("finds dependencies for the team data in test-normalise", {
    df <- data.frame(
      team = c(
        'Red', 'Red', 'Red', 'Orange', 'Orange',
        'Yellow', 'Yellow', 'Green', 'Green', 'Blue'
      ),
      jersey_num = c(
        1, 2, 3, 1, 2,
        1, 5, 8, 2, 2
      ),
      player_name = c(
        'A', 'B', 'C', 'D', 'A',
        'E', 'B', 'A', 'G', 'H'
      ),
      city = c(
        'boston', 'boston', 'boston', 'chicago', 'chicago',
        'honolulu', 'honolulu', 'boston', 'boston', 'austin'
      ),
      state = c(
        'MA', 'MA', 'MA', 'IL', 'IL',
        'HI', 'HI', 'MA', 'MA', 'TX'
      )
    )
    deps <- dfd(df, 1)
    expected_deps <- list(
      team = list(c('player_name', 'jersey_num')),
      jersey_num = list(c('player_name', 'team')),
      player_name = list(c('team', 'jersey_num')),
      city = list('team', 'state', c('player_name', 'jersey_num')),
      state = list('team', c('player_name', 'jersey_num'), 'city')
    )
    expect_identical(lengths(deps$dependencies), lengths(expected_deps))
    expect_superset_of_dependency(deps$dependencies, expected_deps)
  })
  it("finds dependencies for the team data in original's edit demo", {
    df <- data.frame(
      team = c("tigers", "elephants", "foxes", "snakes", "dolphins", "eagles"),
      city = c("boston", "chicago", "miami", "austin", "honolulu", "houston"),
      state = c("MA", "IL", "FL", "TX", "HI", "TX"),
      roster_size = c(20L, 21L, 20L, 20L, 19L, 21L)
    )
    deps <- dfd(df, 1)
    expected_deps <- list(
      team = list("city"),
      city = list("team"),
      state = list("team", "city"),
      roster_size = list("team", "city")
    )
    expect_superset_of_dependency(deps$dependencies, expected_deps)
  })
  it("finds dependencies for Wikipedia 1NF->2NF->3NF example", {
    df <- data.frame(
      Title = rep(
        c(
          "Beginning MySQL Database Design and Optimization",
          "The Relational Model for Database Management: Version 2"
        ),
        each = 2
      ),
      Format = c("Hardcover", "E-book", "E-book", "Paperback"),
      Author = rep(c("Chad Russell", "E.F. Codd"), each = 2),
      Author_Nationality = rep(c("American", "British"), each = 2),
      Price = c(4999L, 2234L, 1388L, 3999L),
      Thickness = "Thick",
      Genre_ID = rep(1:2, each = 2),
      Genre_Name = rep(c("Tutorial", "Popular science"), each = 2),
      Publisher_ID = rep(1:2, each = 2)
    )
    deps <- dfd(df, 1)
    expected_deps <- list(
      Title = list(),
      Format = list(),
      Author = list("Title"),
      Author_Nationality = list("Author"),
      Price = list(c("Title", "Format")),
      Thickness = list(character()),
      Genre_ID = list("Title"),
      Genre_Name = list("Genre_ID"),
      Publisher_ID = list("Title")
    )
    expect_superset_of_dependency(deps$dependencies, expected_deps)
  })
  it("correctly handles attributes with non-df-standard names", {
    df <- data.frame(1:3, c(1, 1, 2), c(1, 2, 2)) |>
      stats::setNames(c("A 1", "B 2", "C 3"))
    deps <- dfd(df, 1)
    expect_identical(deps$dependencies$`A 1`, list(c("B 2", "C 3")))
  })
  it("expects attribute names to be unique", {
    df <- data.frame(A = 1:3, B = c(1, 1, 2), A = c(1, 2, 2), check.names = FALSE)
    expect_error(dfd(df, 1), "^duplicate column names: A$")
  })
  it("gets the same results with and without storing partitions", {
    gen_ncol_inc <- gen.int(6)
    gen_len_inc <- gen.int(20)
    gen_df <- generate(
      for (n_col_inc in gen_ncol_inc) {
        generate(
          for (len_inc in gen_len_inc) {
            rep(
              list(gen.sample(c(FALSE, TRUE), len_inc - 1, replace = TRUE)),
              n_col_inc - 1
            ) |>
              setNames(make.unique(rep_len(LETTERS, n_col_inc - 1)))
          }
        )
      }
    )
    forall(
      gen_df,
      function(df) {
        df <- as.data.frame(df)
        res_nocache <- dfd(df, 1, cache = FALSE)
        res_cache <- dfd(df, 1, cache = TRUE)
        expect_identical(
          lapply(res_nocache$dependencies, \(det_sets) if (length(det_sets) == 0) det_sets else det_sets[keys_order(det_sets)]),
          lapply(res_cache$dependencies, \(det_sets) if (length(det_sets) == 0) det_sets else det_sets[keys_order(det_sets)])
        )
        res_partial_nocache <- dfd(df, 3/4, cache = FALSE)
        res_partial_cache <- dfd(df, 3/4, cache = TRUE)
        expect_identical(
          lapply(res_partial_nocache$dependencies, \(det_sets) if (length(det_sets) == 0) det_sets else det_sets[keys_order(det_sets)]),
          lapply(res_partial_cache$dependencies, \(det_sets) if (length(det_sets) == 0) det_sets else det_sets[keys_order(det_sets)])
        )
      },
      shrink.limit = Inf
    )
  })
})
