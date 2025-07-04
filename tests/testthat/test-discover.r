# Various tests should have random accuracy draws, probably with lots of
# weight on 1

describe("discover", {
  expect_equiv_deps <- function(deps1, deps2) {
    expect_setequal(attrs_order(deps1), attrs_order(deps2))
    expect_setequal(
      deps1,
      functional_dependency(
        unclass(deps2),
        attrs_order(deps1)
      )
    )
  }
  expect_equiv_deps_except_names <- function(deps1, deps2) {
    nms1 <- attrs_order(deps1)
    nms2 <- attrs_order(deps2)
    renamed_deps1 <- functional_dependency(
      Map(
        list,
        lapply(detset(deps1), \(dets) nms2[match(dets, nms1)]),
        nms2[match(dependant(deps1), nms1)]
      ),
      nms2
    )
    expect_equiv_deps(renamed_deps1, deps2)
  }
  expect_equiv_deps_except_classes <- function(deps1, deps2) {
    nms1 <- attrs_order(deps1)
    nms2 <- attrs_order(deps2)
    reclassed_deps1 <- functional_dependency(
      Map(
        list,
        lapply(detset(deps1), \(dets) nms2[match(dets, nms1)]),
        nms2[match(dependant(deps1), nms1)]
      ),
      nms1
    )
    expect_equiv_deps(reclassed_deps1, deps2)
  }
  expect_equiv_non_removed_attr_deps <- function(deps1, deps2) {
    removed_attr <- setdiff(attrs_order(deps1), attrs_order(deps2))
    expect_length(removed_attr, 1)
    filtered <- deps1
    filtered <- functional_dependency(
      unclass(filtered[vapply(
        filtered,
        \(fd) !is.element(removed_attr, unlist(fd)),
        logical(1)
      )]),
      setdiff(attrs_order(deps1), removed_attr)
    )
    expect_equiv_deps(filtered, deps2)
  }
  expect_det_subsets_kept <- function(deps1, deps2) {
    expect_identical(attrs_order(deps1), attrs_order(deps2))
    expect_true(all(vapply(
      deps1,
      \(ds) any(
        vapply(dependant(deps2), identical, logical(1), dependant(ds)) &
          vapply(
            detset(deps2),
            \(detset) all(is.element(detset, detset(ds)[[1L]])),
            logical(1)
          )
      ),
      logical(1)
    )))
  }
  terminates_then <- function(fn, accuracy, ...) {
    function(df) {
      res <- R.utils::withTimeout(discover(df, accuracy = accuracy, ...), timeout = 5, onTimeout = "silent")
      if (is.null(res)) {
        return(fail("discover() timed out"))
      }
      succeed() # dummy success, otherwise tests complain about no expectations
      fn(res)
    }
  }
  both_terminate_then <- function(fn, accuracy, ...) {
    function(df1, df2) {
      res1 <- R.utils::withTimeout(discover(df1, accuracy = accuracy, ...), timeout = 5, onTimeout = "silent")
      expect_true(!is.null(res1))
      res2 <- R.utils::withTimeout(discover(df2, accuracy = accuracy, ...), timeout = 5, onTimeout = "silent")
      expect_true(!is.null(res2))
      fn(res1, res2)
    }
  }
  terminates_with_and_without_full_cache_then <- function(fn, accuracy, ...) {
    function(df) {
      res_cache <- R.utils::withTimeout(
        discover(df, accuracy = accuracy, full_cache = TRUE, ...),
        timeout = 5,
        onTimeout = "silent"
      )
      expect_true(!is.null(res_cache))
      res_nocache <- R.utils::withTimeout(
        discover(df, accuracy = accuracy, full_cache = FALSE, ...),
        timeout = 5,
        onTimeout = "silent"
      )
      expect_true(!is.null(res_nocache))
      fn(res_cache, res_nocache)
    }
  }
  terminates_with_and_without_store_cache_then <- function(fn, accuracy, ...) {
    function(df) {
      res_store <- R.utils::withTimeout(
        discover(df, accuracy = accuracy, full_cache = TRUE, store_cache = TRUE, ...),
        timeout = 5,
        onTimeout = "silent"
      )
      expect_true(!is.null(res_store))
      res_nostore <- R.utils::withTimeout(
        discover(df, accuracy = accuracy, full_cache = TRUE, store_cache = FALSE, ...),
        timeout = 5,
        onTimeout = "silent"
      )
      expect_true(!is.null(res_nostore))
      fn(res_store, res_nostore)
    }
  }
  terminates_with_and_without_bijection_skip_then <- function(fn, accuracy, ...) {
    function(df) {
      res_skip <- R.utils::withTimeout(
        discover(df, accuracy = accuracy, full_cache = TRUE, store_cache = TRUE, skip_bijections = TRUE, ...),
        timeout = 5,
        onTimeout = "silent"
      )
      if (is.null(res_skip))
        return(fail("discover() with bijection skip timed out"))
      res_noskip <- R.utils::withTimeout(
        discover(df, accuracy = accuracy, full_cache = TRUE, store_cache = TRUE, skip_bijections = FALSE, ...),
        timeout = 5,
        onTimeout = "silent"
      )
      if (is.null(res_noskip))
        return(fail("discover() without bijection skip timed out"))
      fn(res_skip, res_noskip)
    }
  }

  it("gives a deterministic result, except for per-dependant dependency order", {
    two_copies <- function(fn) {
      function(df) {
        fn(df, df)
      }
    }
    forall(
      gen_df(4, 6),
      two_copies(both_terminate_then(expect_equiv_deps, accuracy = 1, method = "DFD"))
    )
    forall(
      gen_df(4, 6),
      two_copies(both_terminate_then(expect_equiv_deps, accuracy = 1, method = "FDHitsSep"))
    )
    forall(
      gen_df(4, 6),
      two_copies(both_terminate_then(expect_equiv_deps, accuracy = 1, method = "FDHitsJoint"))
    )
  })
  it("returns dependencies where shared dependant <=> not sub/supersets for determinants", {
    has_non_nested_determinant_sets <- function(deps) {
      det_groups <- split(detset(deps), dependant(deps))
      for (det_sets in det_groups) {
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
    forall(gen_df(4, 6), terminates_then(has_non_nested_determinant_sets, 1, method = "DFD"))
    forall(gen_df(4, 6), terminates_then(has_non_nested_determinant_sets, 1, method = "FDHitsSep"))
    forall(gen_df(4, 6), terminates_then(has_non_nested_determinant_sets, 1, method = "FDHitsJoint"))
  })
  it("is invariant to an attribute's values being permuted", {
    gen_perm <- function(vals) {
      uniq <- unique(vals)
      matches <- match(vals, uniq)
      gen.sample(uniq, length(uniq)) |>
        gen.with(\(perm) perm[matches])
    }
    gen_df_and_value_perm <- function(
      nrow,
      ncol,
      remove_dup_rows = FALSE
    ) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(gen.pure(df), gen.int(ncol(df)))) |>
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
      both_terminate_then(expect_equiv_deps, 1, method = "DFD"),
      curry = TRUE
    )
    forall(
      gen_df_and_value_perm(4, 6),
      both_terminate_then(expect_equiv_deps, 1, method = "FDHitsSep"),
      curry = TRUE
    )
    forall(
      gen_df_and_value_perm(4, 6),
      both_terminate_then(expect_equiv_deps, 1, method = "FDHitsJoint"),
      curry = TRUE
    )
  })
  it("is invariant to an attribute's class being losslessly changed (except for class info)", {
    gen_df_and_type_change <- function(
      nrow,
      ncol,
      remove_dup_rows = FALSE
    ) {
      classes <- c("logical", "integer", "numeric", "character")
      changes <- list(
        logical = c("integer", "numeric", "character"),
        integer = c("numeric", "character"),
        numeric = c("character"),
        character = c("logical"),
        factor = c("integer", "numeric", "character")
      )
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.sample(ncol(df)))) |>
        gen.and_then(uncurry(\(df, attr) {
          list(
            gen.pure(df),
            gen.pure(attr),
            gen.element(changes[[class(df[[attr]])[[1]]]])
          )
        })) |>
        gen.with(uncurry(\(df, attr, new_class) {
          permed <- df
          permed[[attr]] <- as(permed[[attr]], new_class)
          list(df, permed)
        }))
    }
    forall(
      gen_df_and_type_change(4, 6),
      both_terminate_then(expect_equiv_deps_except_classes, 1, method = "DFD"),
      curry = TRUE
    )
    forall(
      gen_df_and_type_change(4, 6),
      both_terminate_then(expect_equiv_deps_except_classes, 1, method = "FDHitsSep"),
      curry = TRUE
    )
    forall(
      gen_df_and_type_change(4, 6),
      both_terminate_then(expect_equiv_deps_except_classes, 1, method = "FDHitsJoint"),
      curry = TRUE
    )
  })
  it("terminates properly when attributes have parameter names for paste", {
    df1 <- data.frame(
      b = NA,
      j = c(TRUE, NA, NA, NA),
      u = c(FALSE, TRUE, TRUE, NA),
      l = c(FALSE, TRUE, TRUE, TRUE),
      t = c(FALSE, FALSE, TRUE, NA),
      sep = c(TRUE, TRUE, NA, NA)
    )
    df2 <- df1[, c("l", "j", "t", "b", "u", "sep")]
    terminates_with_and_without_cache <- function(...) {
      terminates_with_and_without_full_cache_then(
        \(x, y) {},
        1,
        ...
      )
    }
    terminates_with_and_without_cache(method = "DFD")(df1)
    terminates_with_and_without_cache(method = "FDHitsSep")(df1)
    terminates_with_and_without_cache(method = "FDHitsJoint")(df1)
    terminates_with_and_without_cache(method = "DFD")(df2)
    terminates_with_and_without_cache(method = "FDHitsSep")(df2)
    terminates_with_and_without_cache(method = "FDHitsJoint")(df2)
  })
  it("is invariant, under reordering, to attributes being reordered", {
    gen_df_and_attr_perm <- function(
      nrow,
      ncol,
      remove_dup_rows = FALSE
    ) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, sample.int(ncol(df)))) |>
        gen.with(\(lst) {
          df <- lst[[1]]
          perm <- lst[[2]]
          list(df, df[, perm, drop = FALSE])
        })
    }

    forall(
      gen_df_and_attr_perm(4, 6),
      both_terminate_then(expect_equiv_deps, 1, method = "DFD"),
      curry = TRUE
    )
    forall(
      gen_df_and_attr_perm(4, 6),
      both_terminate_then(expect_equiv_deps, 1, method = "FDHitsSep"),
      curry = TRUE
    )
    forall(
      gen_df_and_attr_perm(4, 6),
      both_terminate_then(expect_equiv_deps, 1, method = "FDHitsJoint"),
      curry = TRUE
    )
  })
  it("loses FDs involving a removed attribute, keeps the rest", {
    gen_df_and_remove_col <- function(nrow, ncol, remove_dup_rows = FALSE) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.int(ncol(df)))) |>
        gen.with(\(lst) {
          df <- lst[[1]]
          n <- lst[[2]]
          list(df, df[, -n, drop = FALSE])
        })
    }
    forall(
      gen_df_and_remove_col(4, 6),
      both_terminate_then(expect_equiv_non_removed_attr_deps, 1, method = "DFD"),
      curry = TRUE
    )
    forall(
      gen_df_and_remove_col(4, 6),
      both_terminate_then(expect_equiv_non_removed_attr_deps, 1, method = "FDHitsSep"),
      curry = TRUE
    )
    forall(
      gen_df_and_remove_col(4, 6),
      both_terminate_then(expect_equiv_non_removed_attr_deps, 1, method = "FDHitsJoint"),
      curry = TRUE
    )
  })
  it("is invariant to changes of accuracy within same required row count when using DFD", {
    gen_df_and_accuracy_nrow <- function(nrow, ncol, remove_dup_rows = FALSE) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
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
        res1 <- R.utils::withTimeout(discover(df, accuracy = low, ...), timeout = 5, onTimeout = "silent")
        expect_true(!is.null(res1))
        res2 <- R.utils::withTimeout(discover(df, accuracy = high, ...), timeout = 5, onTimeout = "silent")
        expect_true(!is.null(res2))
        fn(res1, res2)
      }
    }
    forall(
      gen_df_and_accuracy_nrow(4, 6),
      both_bounds_terminate_then(expect_equiv_deps, method = "DFD"),
      curry = TRUE
    )
  })
  it("keeps subsets of all FDs if a row is removed, might have more", {
    gen_df_and_remove_row <- function(nrow, ncol) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows = TRUE) |>
        gen.and_then(\(df) list(df, gen.element(seq_len(nrow(df))))) |>
        gen.with(\(lst) {
          df <- lst[[1]]
          n <- lst[[2]]
          list(df, df[-n, , drop = FALSE])
        })
    }
    forall(
      gen_df_and_remove_row(4, 6),
      both_terminate_then(expect_det_subsets_kept, 1, method = "DFD"),
      curry = TRUE
    )
    forall(
      gen_df_and_remove_row(4, 6),
      both_terminate_then(expect_det_subsets_kept, 1, method = "FDHitsSep"),
      curry = TRUE
    )
    forall(
      gen_df_and_remove_row(4, 6),
      both_terminate_then(expect_det_subsets_kept, 1, method = "FDHitsJoint"),
      curry = TRUE
    )
  })
  it("discover -> change attribute names is equivalent to change names -> discover", {
    gen_df_and_name_change <- function(nrow, ncol, remove_dup_rows = FALSE) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.sample(LETTERS, ncol(df)))) |>
        gen.with(\(lst) {
          df <- lst[[1]]
          new_names <- lst[[2]]
          list(df, stats::setNames(df, new_names))
        })
    }
    forall(
      gen_df_and_name_change(4, 6),
      both_terminate_then(expect_equiv_deps_except_names, 1, method = "DFD"),
      curry = TRUE
    )
    forall(
      gen_df_and_name_change(4, 6),
      both_terminate_then(expect_equiv_deps_except_names, 1, method = "FDHitsSep"),
      curry = TRUE
    )
    forall(
      gen_df_and_name_change(4, 6),
      both_terminate_then(expect_equiv_deps_except_names, 1, method = "FDHitsJoint"),
      curry = TRUE
    )
  })
  it("correctly simplifies date attributes with varying standard/daylight savings", {
    # example from nycflights13::weather
    df <- data.frame(
      month = c(11L, 11L, 11L),
      day = c(3L, 3L, 4L),
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
    deps <- discover(df, method = "DFD")
    expect_length(deps[dependant(deps) == "time"], 0L)
    deps2 <- discover(df, method = "FDHitsSep")
    expect_length(deps2[dependant(deps2) == "time"], 0L)
    deps3 <- discover(df, method = "FDHitsJoint")
    expect_length(deps3[dependant(deps3) == "time"], 0L)
  })
  it("correctly simplifies floating-point numbers to high accuracy", {
    df <- data.frame(
      x = c(
        47.37661580000000327573,
        47.37661580000000327573
      ),
      y = c(
        8.549177500000007,
        8.549177499999999
      )
    )
    expect_identical(
      discover(df, digits = 8, method = "DFD"),
      functional_dependency(
        list(list(character(), "x"), list(character(), "y")),
        c("x", "y")
      )
    )
    expect_identical(
      discover(df, digits = 8, method = "FDHitsSep"),
      functional_dependency(
        list(list(character(), "x"), list(character(), "y")),
        c("x", "y")
      )
    )
    expect_identical(
      discover(df, digits = 8, method = "FDHitsJoint"),
      functional_dependency(
        list(list(character(), "x"), list(character(), "y")),
        c("x", "y")
      )
    )
    expect_identical(
      discover(df, digits = 15, method = "DFD"),
      functional_dependency(
        list(list(character(), "x")),
        c("x", "y")
      )
    )
    expect_identical(
      discover(df, digits = 15, method = "FDHitsSep"),
      functional_dependency(
        list(list(character(), "x")),
        c("x", "y")
      )
    )
    expect_identical(
      discover(df, digits = 15, method = "FDHitsJoint"),
      functional_dependency(
        list(list(character(), "x")),
        c("x", "y")
      )
    )
  })
  it("doesn't have an excluded attribute in any determinant sets", {
    gen_df_and_exclude <- function(nrow, ncol, remove_dup_rows = FALSE) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.element(names(df))))
    }
    terminates_with_exclusion_then_no_trival <- function(accuracy, ...) {
      function(df, attr) {
        deps <- R.utils::withTimeout(
          discover(df, accuracy = accuracy, exclude = attr, ...),
          timeout = 5,
          onTimeout = "silent"
        )
        if (is.null(deps)) {
          return(fail("discover() with exclude timed out"))
        }
        # test exclusion_not_in_determinant_sets
        expect_false(attr %in% unlist(detset(deps)))
      }
    }
    forall(
      gen_df_and_exclude(4, 6),
      terminates_with_exclusion_then_no_trival(1, method = "DFD"),
      curry = TRUE
    )
    forall(
      gen_df_and_exclude(4, 6),
      terminates_with_exclusion_then_no_trival(1, method = "FDHitsSep"),
      curry = TRUE
    )
    forall(
      gen_df_and_exclude(4, 6),
      terminates_with_exclusion_then_no_trival(1, method = "FDHitsJoint"),
      curry = TRUE
    )
  })
  it("gives same result from excluding class and exclude attributes with that class", {
    exclude_and_exclude_class_terminate_then <- function(fn, accuracy, class, ...) {
      function(df) {
        attrs_with_class <- names(df)[vapply(df, inherits, logical(1), class)]
        deps1 <- R.utils::withTimeout(
          discover(df, accuracy = accuracy, exclude = attrs_with_class, ...),
          timeout = 5,
          onTimeout = "silent"
        )
        if (is.null(deps1))
          return(fail("discover() with exclude timed out"))
        deps2 <- R.utils::withTimeout(
          discover(df, accuracy = accuracy, exclude_class = class, ...),
          timeout = 5,
          onTimeout = "silent"
        )
        if (is.null(deps2))
          return(fail("discover() with exclude_class timed out"))
        fn(deps1, deps2)
      }
    }
    forall(
      gen_df(4, 6),
      exclude_and_exclude_class_terminate_then(
        expect_equiv_deps,
        accuracy = 1,
        "logical",
        method = "DFD"
      )
    )
    forall(
      gen_df(4, 6),
      exclude_and_exclude_class_terminate_then(
        expect_equiv_deps,
        accuracy = 1,
        "logical",
        method = "FDHitsSep"
      )
    )
    forall(
      gen_df(4, 6),
      exclude_and_exclude_class_terminate_then(
        expect_equiv_deps,
        accuracy = 1,
        "logical",
        method = "FDHitsJoint"
      )
    )
  })
  it("gives same result from using filter arguments and from post-filtering", {
    superset <- function(x, y) {
      if (length(x) == 0)
        return(logical(1))
      mat <- outer(x, y, Vectorize(\(u, v) setequal(intersect(u, v), v)))
      apply(mat, 1, any)
    }
    same_under_constraints_and_filtering <- function(
      x,
      dependants,
      detset_limit,
      method,
      ...
    ) {
      by_arguments <- R.utils::withTimeout(
        discover(
          x,
          dependants = dependants,
          detset_limit = detset_limit,
          method = method,
          ...
        ),
        timeout = 5,
        onTimeout = "silent"
      )
      if (is.null(by_arguments))
        return(fail("discover() with filtering arguments timed out"))
      by_filtering <- R.utils::withTimeout(
        discover(x, method = method, ...),
        timeout = 5,
        onTimeout = "silent"
      )
      if (is.null(by_filtering))
        return(fail("discover() without filtering arguments timed out"))
      by_filtering <- by_filtering[
        dependant(by_filtering) %in% dependants &
          lengths(detset(by_filtering)) <= detset_limit
      ]
      expect_setequal(by_arguments, by_filtering)
    }
    forall(
      gen_df(4, 6) |>
        gen.and_then(\(x) {
          list(
            gen.pure(x),
            gen.sample_resampleable(names(x), from = 0, to = ncol(x)),
            gen.element(0:(ncol(x) - 1L)),
            gen.element(c("DFD", "FDHitsSep", "FDHitsJoint"))
          )
        }),
      same_under_constraints_and_filtering,
      curry = TRUE
    )

    # example
    x <- data.frame(
      a = c(0, NA, NA, 1),
      b = c(NA, FALSE, NA, NA),
      c = c("TRUE", NA, "TRUE", "FALSE"),
      d = c(1L, 1L, NA, 1L),
      e = c(NA, 1L, 1L, 0L)
    )
    same_under_constraints_and_filtering(x, c("a", "c"), 2L, method = "DFD")
    same_under_constraints_and_filtering(x, c("a", "c"), 2L, method = "FDHitsSep")
    same_under_constraints_and_filtering(x, c("a", "c"), 2L, method = "FDHitsJoint")
  })
  it("gives dependencies for unique attributes (in case don't want them as key)", {
    df <- data.frame(A = 1:3, B = c(1, 1, 2), C = c(1, 2, 2))

    deps_dfd <- discover(df, method = "DFD")
    A_deps_dfd <- dependant(deps_dfd) == "A"
    A_detsets_dfd <- detset(deps_dfd[A_deps_dfd])
    expect_identical(A_detsets_dfd, list(c("B", "C")))

    deps_sep <- discover(df, method = "FDHitsSep")
    A_deps_sep <- dependant(deps_sep) == "A"
    A_detsets_sep <- detset(deps_sep[A_deps_sep])
    expect_identical(A_detsets_sep, list(c("B", "C")))

    deps_jnt <- discover(df, method = "FDHitsJoint")
    A_deps_jnt <- dependant(deps_jnt) == "A"
    A_detsets_jnt <- detset(deps_jnt[A_deps_jnt])
    expect_identical(A_detsets_jnt, list(c("B", "C")))
  })
  it("finds dependencies for the team data in test-synthesise", {
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
    expected_deps <- functional_dependency(
      list(
        list(c('player_name', 'jersey_num'), "team"),
        list(c('player_name', 'team'), "jersey_num"),
        list(c('team', 'jersey_num'), "player_name"),
        list('team', "city"),
        list('state', "city"),
        list(c('player_name', 'jersey_num'), "city"),
        list('team', "state"),
        list(c('player_name', 'jersey_num'), "state"),
        list('city', "state")
      ),
      c("team", "jersey_num", "player_name", "city", "state")
    )

    deps_dfd <- discover(df, method = "DFD")
    expect_identical(attrs_order(deps_dfd), attrs_order(expected_deps))
    expect_true(all(is.element(expected_deps, deps_dfd)))

    deps_sep <- discover(df, method = "FDHitsSep")
    expect_identical(attrs_order(deps_sep), attrs_order(expected_deps))
    expect_true(all(is.element(expected_deps, deps_sep)))

    deps_jnt <- discover(df, method = "FDHitsJoint")
    expect_identical(attrs_order(deps_jnt), attrs_order(expected_deps))
    expect_true(all(is.element(expected_deps, deps_jnt)))
  })
  it("finds dependencies for the team data in original's edit demo", {
    df <- data.frame(
      team = c("tigers", "elephants", "foxes", "snakes", "dolphins", "eagles"),
      city = c("boston", "chicago", "miami", "austin", "honolulu", "houston"),
      state = c("MA", "IL", "FL", "TX", "HI", "TX"),
      roster_size = c(20L, 21L, 20L, 20L, 19L, 21L)
    )
    expected_deps <- functional_dependency(
      list(
        list("city", "team"),
        list("team", "city"),
        list("team", "state"),
        list("city", "state"),
        list("team", "roster_size"),
        list("city", "roster_size")
      ),
      c("team", "city", "state", "roster_size")
    )

    deps_dfd <- discover(df, method = "DFD")
    expect_identical(attrs_order(deps_dfd), attrs_order(expected_deps))
    expect_true(all(is.element(expected_deps, deps_dfd)))

    deps_sep <- discover(df, method = "FDHitsSep")
    expect_identical(attrs_order(deps_sep), attrs_order(expected_deps))
    expect_true(all(is.element(expected_deps, deps_sep)))

    deps_jnt <- discover(df, method = "FDHitsJoint")
    expect_identical(attrs_order(deps_jnt), attrs_order(expected_deps))
    expect_true(all(is.element(expected_deps, deps_jnt)))
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
    expected_deps <- functional_dependency(
      list(
        list("Title", "Author"),
        list("Author", "Author_Nationality"),
        list(c("Title", "Format"), "Price"),
        list(character(), "Thickness"),
        list("Title", "Genre_ID"),
        list("Genre_ID", "Genre_Name"),
        list("Title", "Publisher_ID")
      ),
      c(
        "Title",
        "Format",
        "Author",
        "Author_Nationality",
        "Price",
        "Thickness",
        "Genre_ID",
        "Genre_Name",
        "Publisher_ID"
      )
    )
    deps_dfd <- discover(df, method = "DFD")
    deps_sep <- discover(df, method = "FDHitsSep")
    deps_jnt <- discover(df, method = "FDHitsJoint")
    expect_identical(attrs_order(deps_dfd), attrs_order(expected_deps))
    expect_identical(attrs_order(deps_sep), attrs_order(expected_deps))
    expect_identical(attrs_order(deps_jnt), attrs_order(expected_deps))
    expect_true(all(is.element(expected_deps, deps_dfd)))
    expect_true(all(is.element(expected_deps, deps_sep)))
    expect_true(all(is.element(expected_deps, deps_jnt)))
  })
  it("correctly handles attributes with non-df-standard names", {
    df <- data.frame(1:3, c(1, 1, 2), c(1, 2, 2)) |>
      stats::setNames(c("A 1", "B 2", "C 3"))

    deps_dfd <- discover(df, method = "DFD")
    A_1_deps_dfd <- dependant(deps_dfd) == "A 1"
    A_1_detsets_dfd <- detset(deps_dfd[A_1_deps_dfd])
    expect_identical(A_1_detsets_dfd, list(c("B 2", "C 3")))

    deps_sep <- discover(df, method = "FDHitsSep")
    A_1_deps_sep <- dependant(deps_sep) == "A 1"
    A_1_detsets_sep <- detset(deps_sep[A_1_deps_sep])
    expect_identical(A_1_detsets_sep, list(c("B 2", "C 3")))

    deps_jnt <- discover(df, method = "FDHitsJoint")
    A_1_deps_jnt <- dependant(deps_jnt) == "A 1"
    A_1_detsets_jnt <- detset(deps_jnt[A_1_deps_jnt])
    expect_identical(A_1_detsets_jnt, list(c("B 2", "C 3")))
  })
  it("expects attribute names to be unique", {
    df <- data.frame(A = 1:3, B = c(1, 1, 2), A = c(1, 2, 2), check.names = FALSE)
    expect_error(discover(df, method = "DFD"), "^duplicate column names: A$")
    expect_error(discover(df, method = "FDHitsSep"), "^duplicate column names: A$")
    expect_error(discover(df, method = "FDHitsJoint"), "^duplicate column names: A$")
  })
  it("gets the same results with and without storing partitions", {
    forall(
      gen_df(20, 5),
      terminates_with_and_without_full_cache_then(
        expect_equiv_deps,
        accuracy = 1,
        method = "DFD"
      ),
      shrink.limit = Inf
    )
    forall(
      gen_df(20, 5),
      terminates_with_and_without_full_cache_then(
        expect_equiv_deps,
        accuracy = 1,
        method = "FDHitsSep"
      ),
      shrink.limit = Inf
    )
    forall(
      gen_df(20, 5),
      terminates_with_and_without_full_cache_then(
        expect_equiv_deps,
        accuracy = 1,
        method = "FDHitsJoint"
      ),
      shrink.limit = Inf
    )
    forall(
      gen_df(20, 5),
      terminates_with_and_without_full_cache_then(
        expect_equiv_deps,
        accuracy = 3/4,
        method = "DFD"
      ),
      shrink.limit = Inf
    )
  })
  it("is invariant to whether partition is transferred between dependants, also for accuracy < 1 for DFD", {
    forall(
      gen_df(20, 5),
      terminates_with_and_without_store_cache_then(
        expect_equiv_deps,
        accuracy = 1,
        method = "DFD"
      ),
      shrink.limit = Inf
    )
    forall(
      gen_df(20, 5),
      terminates_with_and_without_store_cache_then(
        expect_equiv_deps,
        accuracy = 1,
        method = "FDHitsSep"
      ),
      shrink.limit = Inf
    )
    forall(
      gen_df(20, 5),
      terminates_with_and_without_store_cache_then(
        expect_equiv_deps,
        accuracy = 1,
        method = "FDHitsJoint"
      ),
      shrink.limit = Inf
    )
    forall(
      gen_df(20, 5),
      terminates_with_and_without_store_cache_then(
        expect_equiv_deps,
        accuracy = 3/4,
        method = "DFD"
      ),
      shrink.limit = Inf
    )
  })
  it("is invariant to whether bijections are skipped, under full accuracy", {
    df1 <- data.frame(
      a = c(FALSE, FALSE, TRUE),
      b = FALSE,
      c = c(FALSE, TRUE, NA),
      d = c(FALSE, NA, NA),
      e = c(FALSE, TRUE, NA)
    )
    df2 <- data.frame(
      a = c(FALSE, TRUE, NA, TRUE, NA),
      b = c(TRUE, TRUE, TRUE, TRUE, NA),
      c = c(FALSE, TRUE, NA, NA, NA),
      d = c(NA, FALSE, TRUE, FALSE, TRUE),
      e = c(FALSE, FALSE, FALSE, TRUE, TRUE)
    )
    df3 <- data.frame(
      a = c(NA, TRUE, NA),
      b = c(TRUE, FALSE, TRUE),
      c = c(NA, NA, FALSE),
      d = c(NA, NA, TRUE),
      e = c(FALSE, TRUE, NA)
    )
    invariant_to_bijection_skip <- function(...) {
      terminates_with_and_without_bijection_skip_then(
        expect_equiv_deps,
        accuracy = 1,
        ...
      )
    }
    invariant_to_bijection_skip(method = "DFD")(df1)
    invariant_to_bijection_skip(method = "DFD")(df2)
    invariant_to_bijection_skip(method = "DFD")(df3)
    invariant_to_bijection_skip(method = "FDHitsSep")(df1)
    invariant_to_bijection_skip(method = "FDHitsSep")(df2)
    invariant_to_bijection_skip(method = "FDHitsSep")(df3)
    invariant_to_bijection_skip(method = "FDHitsJoint")(df1)
    invariant_to_bijection_skip(method = "FDHitsJoint")(df2)
    invariant_to_bijection_skip(method = "FDHitsJoint")(df3)
    forall(
      gen_df(20, 5),
      invariant_to_bijection_skip(method = "DFD"),
      shrink.limit = Inf
    )
    forall(
      gen_df(20, 5),
      invariant_to_bijection_skip(method = "FDHitsSep"),
      shrink.limit = Inf
    )
    forall(
      gen_df(20, 5),
      invariant_to_bijection_skip(method = "FDHitsJoint"),
      shrink.limit = Inf
    )
  })
  it(
    paste(
      "for accuracy = 1, is invariant to:",
      "- having a non-false keep_rownames vs. adding row names as first column",
      "- method used",
      "- excluding a class vs. excluding attributes in that class",
      "- filtering by arguments (dependants/detset_limit) or by subsetting results",
      "- whether partition is transferred between dependants",
      "- whether bijections are skipped",
      sep = "\n"
    ),
    {
      forall(
        gen_df(4, 6) |>
          gen.and_then(\(x) {
            list(
              gen.pure(x),
              gen.choice(gen.element(c(FALSE, TRUE)), gen_attr_name(9)),
              gen.sample_resampleable(names(x), from = 0, to = ncol(x)),
              gen.element(0:(ncol(x) - 1L))
            )
          }),
        function(df, keep_rownames, dependants, detset_limit) {
          arglists <- expand.grid(
            if (!isFALSE(keep_rownames)) {
              tmp <- if (isTRUE(keep_rownames)) "row" else keep_rownames
              nms <- make.unique(c(names(df), tmp))
              nm <- nms[[length(nms)]]
              list(
                list(df = df, accuracy = 1, keep_rownames = keep_rownames),
                list(
                  df = cbind(setNames(data.frame(rownames(df)), nm), df),
                  accuracy = 1,
                  keep_rownames = FALSE
                )
              )
            }else
              list(list(df = df, accuracy = 1, keep_rownames = FALSE)),
            list(
              list(method = "DFD"),
              list(method = "FDHitsSep"),
              list(method = "FDHitsJoint")
            ),
            list(
              list(exclude_class = "logical"),
              list(exclude = names(df)[vapply(df, is.logical, logical(1))])
            ),
            list(
              list(),
              list(dependants = dependants),
              list(detset_limit = detset_limit),
              list(dependants = dependants, detset_limit = detset_limit)
            ),
            list(
              list(store_cache = FALSE),
              list(store_cache = TRUE)
            ),
            list(
              list(skip_bijections = FALSE),
              list(skip_bijections = TRUE)
            )
          ) |>
            unname() |>
            apply(1, \(x) do.call(c, x), simplify = FALSE)
          results <- lapply(
            arglists,
            \(lst) {
              base <- R.utils::withTimeout(
                do.call(discover, lst),
                timeout = 5,
                onTimeout = "silent"
              )
              if (is.null(base))
                return(base)
              if (is.null(lst$dependants))
                base <- base[dependant(base) %in% dependants]
              if (is.null(lst$detset_limit))
                base <- base[lengths(detset(base)) <= detset_limit]
              base
            }
          )
          if (any(vapply(results, is.null, logical(1))))
            return(fail("some argument lists fail"))
          expect_true(all(vapply(results, setequal, logical(1), results[[1]])))
        },
        curry = TRUE
      )
    }
  )
  it("returns a minimal functional dependency set", {
    forall(
      gen_df(6, 7),
      terminates_then(is_valid_minimal_functional_dependency, 1, method = "DFD")
    )
    forall(
      gen_df(6, 7),
      terminates_then(is_valid_minimal_functional_dependency, 1, method = "FDHitsSep")
    )
    forall(
      gen_df(6, 7),
      terminates_then(is_valid_minimal_functional_dependency, 1, method = "FDHitsJoint")
    )
  })
  it("can include the rownames, equivalent to adding them as first column", {
    x <- data.frame(
      a = c(1, 1, 1, 2, 2, 3, 3, 3, 4),
      b = c(1, 1, 1, 1, 1, 2, 2, 2, 3),
      row.names = letters[1:9]
    )
    default <- discover(x, keep_rownames = TRUE)
    explicit <- discover(x, keep_rownames = "id")
    expected <- discover(x)
    expect_setequal(
      default,
      discover(cbind(data.frame(row = rownames(x)), x))
    )
    expect_setequal(
      explicit,
      discover(cbind(data.frame(id = rownames(x)), x))
    )
  })
})
