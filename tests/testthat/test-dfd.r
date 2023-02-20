# Possible additional properties
# Invariants
# + FDs with the same RHS can't be LHSs where one is a subset of the other
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
  terminates_with_and_without_cache_then <- function(fn, accuracy, ...) {
    function(df) {
      res_cache <- withTimeout(
        dfd(df, accuracy, cache = TRUE, ...),
        timeout = 5,
        onTimeout = "silent"
      )
      expect_true(!is.null(res_cache))
      res_nocache <- withTimeout(
        dfd(df, accuracy, cache = FALSE, ...),
        timeout = 5,
        onTimeout = "silent"
      )
      expect_true(!is.null(res_nocache))
      fn(res_cache, res_nocache)
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
      uniq <- sort(unique(vals), na.last = TRUE)
      matches <- match(vals, uniq)
      pool <- union(uniq, NA)
      gen.sample(pool, length(uniq)) |>
        gen.with(\(perm) perm[matches])
    }
    gen_df_and_value_perm <- function(
      nrow,
      ncol,
      remove_dup_rows = FALSE
    ) {
      gen_df(nrow, ncol, minrow = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.int(ncol(df)))) |>
        gen.and_then(\(lst) c(lst, list(gen_perm(lst[[1]][[lst[[2]]]])))) |>
        gen.with(\(lst) {
          df <- lst[[1]]
          attr <- lst[[2]]
          permuted_attr <- lst[[3]]
          permed <- df
          permed[[attr]] <- permuted_attr
          list(df, permed)
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
      gen_df(nrow, ncol, minrow = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.int(ncol(df)))) |>
        gen.and_then(\(lst) {
          c(
            lst,
            list(gen.element(setdiff(classes, class(lst[[1]][, lst[[2]]]))))
          )
        }) |>
        gen.with(
          \(lst) {
            df <- lst[[1]]
            attr <- lst[[2]]
            new_class <- lst[[3]]
            permed <- df
            permed[[attr]] <- as(permed[[attr]], new_class)
            list(df, permed)
          }
        )
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
      gen_df(nrow, ncol, minrow = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, sample.int(ncol(df)))) |>
        gen.with(\(lst) {
          df <- lst[[1]]
          perm <- lst[[2]]
          list(df, df[, perm, drop = FALSE])
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
      gen_df(nrow, ncol, minrow = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.int(ncol(df)))) |>
        gen.with(\(lst) {
          df <- lst[[1]]
          n <- lst[[2]]
          list(df, df[, -n, drop = FALSE])
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
      gen_df(nrow, ncol, minrow = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.int(nrow(df)))) |>
        gen.with(\(lst) {
          df <- lst[[1]]
          n <- lst[[2]]
          prop <- n/nrow(df)
          low <- (n - 1)/nrow(df) + 1e-9
          list(df, low, prop)
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
      gen_df(nrow, ncol, minrow = 1L, remove_dup_rows = TRUE) |>
        gen.and_then(\(df) list(df, gen.element(seq_len(nrow(df))))) |>
        gen.with(\(lst) {
          df <- lst[[1]]
          n <- lst[[2]]
          list(df, df[-n, , drop = FALSE])
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
      gen_df(nrow, ncol, minrow = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.sample(LETTERS, ncol(df)))) |>
        gen.with(\(lst) {
          df <- lst[[1]]
          new_names <- lst[[2]]
          list(df, stats::setNames(df, new_names))
        })
    }
    forall(
      gen_df_and_name_change(4, 6),
      both_terminate_then(expect_equiv_deps_except_names, 1),
      curry = TRUE
    )
  })
  it("correctly simplifies date attributes with varying standard/daylight savings", {
    # example from nycflights13::weather
    df <- data.frame(
      month = c(11L, 11L, 12L),
      day = 3L,
      hour = 1L,
      time = as.POSIXct(
        # 2013-11-03 01:00:00 EDT,
        # 2013-11-03 01:00:00 EST,
        # 2013-11-04 01:00:00 EST
        c(1383454800L, 1383458400L, 1383544800L),
        origin = "1970-01-01 00:00:00 UTC",
        tz = "America/New_York"
      )
    )
    stopifnot(df[1, "time"] != df[2, "time"])
    deps <- dfd(df, 1)
    expect_length(deps$dependencies$time, 0L)
  })
  it("doesn't have an excluded attribute in any determinant sets", {
    gen_df_and_exclude <- function(nrow, ncol, remove_dup_rows = FALSE) {
      gen_df(nrow, ncol, minrow = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.sample(names(df), 1)))
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
    forall(
      gen_df(20, 5),
      terminates_with_and_without_cache_then(expect_equiv_deps, accuracy = 1),
      shrink.limit = Inf
    )
    forall(
      gen_df(20, 5),
      terminates_with_and_without_cache_then(expect_equiv_deps, accuracy = 3/4),
      shrink.limit = Inf
    )
  })
})
