# Various tests should have random accuracy draws, probably with lots of
# weight on 1

describe("discover", {
  make.unique_after <- function(x, pre) {
    if (length(pre) == 0)
      return(x)
    stopifnot(!anyDuplicated(pre))
    make.unique(c(pre, x))[-seq_along(pre)]
  }
  with_timeout <- function(expr, timeout = 5) {
    R.utils::withTimeout(
      expr,
      timeout = timeout,
      onTimeout = "silent"
    )
  }
  fds_equivalent <- function(fds1, fds2) {
    setequal(attrs_order(fds1), attrs_order(fds2)) &&
      setequal(fds1, fds2)
  }
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
    expect_equiv_deps(rename_attrs(deps1, attrs_order(deps2)), deps2)
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
    expect_true(all(apply(outer(deps1, deps2, ">="), 1, any)))
  }
  terminates_then <- function(fn, accuracy, ...) {
    function(df) {
      res <- with_timeout(discover(df, accuracy = accuracy, ...))
      if (is.null(res))
        return(fail("discover() timed out"))
      succeed() # dummy success, otherwise tests complain about no expectations
      fn(res)
    }
  }
  both_terminate_then <- function(fn, accuracy, ...) {
    function(df1, df2) {
      res1 <- with_timeout(discover(df1, accuracy = accuracy, ...))
      if (is.null(res1))
        return(fail("first discover() timed out"))
      res2 <- with_timeout(discover(df2, accuracy = accuracy, ...))
      if (is.null(res2))
        return(fail("second discover() timed out"))
      fn(res1, res2)
    }
  }
  terminates_with_and_without_full_cache_then <- function(fn, accuracy, ...) {
    function(df) {
      res_cache <- with_timeout(discover(
        df,
        accuracy = accuracy,
        full_cache = TRUE,
        ...
      ))
      if (is.null(res_cache))
        return(fail("discover() with full_cache = TRUE timed out"))
      res_nocache <- with_timeout(discover(
        df,
        accuracy = accuracy,
        full_cache = FALSE,
        ...
      ))
      if (is.null(res_nocache))
        return(fail("discover() with full_cache = FALSE timed out"))
      fn(res_cache, res_nocache)
    }
  }
  terminates_with_and_without_store_cache_then <- function(fn, accuracy, ...) {
    function(df) {
      res_store <- with_timeout(discover(
        df,
        accuracy = accuracy,
        full_cache = TRUE,
        store_cache = TRUE,
        ...
      ))
      if (is.null(res_store))
        return(fail("discover() with store_cache = TRUE timed out"))
      res_nostore <- with_timeout(discover(
        df,
        accuracy = accuracy,
        full_cache = TRUE,
        store_cache = FALSE,
        ...
      ))
      if (is.null(res_nostore))
        return(fail("discover() with store_cache = FALSE timed out"))
      fn(res_store, res_nostore)
    }
  }
  terminates_with_and_without_bijection_skip_then <- function(fn, accuracy, ...) {
    function(df) {
      res_skip <- with_timeout(discover(
        df,
        accuracy = accuracy,
        full_cache = TRUE,
        store_cache = TRUE,
        skip_bijections = TRUE,
        ...
      ))
      if (is.null(res_skip))
        return(fail("discover() with skip_bijections = TRUE timed out"))
      res_noskip <- with_timeout(discover(
        df,
        accuracy = accuracy,
        full_cache = TRUE,
        store_cache = TRUE,
        skip_bijections = FALSE,
        ...
      ))
      if (is.null(res_noskip))
        return(fail("discover() with skip_bijections = FALSE timed out"))
      fn(res_skip, res_noskip)
    }
  }

  # input requirements
  it("expects attribute names to be unique", {
    df <- data.frame(A = 1:3, B = c(1, 1, 2), A = c(1, 2, 2), check.names = FALSE)
    expect_error(discover(df, method = "DFD"), "^duplicate column names: A$")
    expect_error(discover(df, method = "FDHitsSep"), "^duplicate column names: A$")
    expect_error(discover(df, method = "FDHitsJoint"), "^duplicate column names: A$")
  })

  # input edge cases
  it("can take attributes with same name as arguments for paste(), e.g. sep", {
    df1 <- data.frame(
      b = NA,
      j = c(TRUE, NA, NA, NA),
      u = c(FALSE, TRUE, TRUE, NA),
      recycle0 = c(FALSE, TRUE, TRUE, TRUE),
      collapse = c(FALSE, FALSE, TRUE, NA),
      sep = c(TRUE, TRUE, NA, NA)
    )
    df2 <- df1[, c("recycle0", "j", "collapse", "b", "u", "sep")]
    terminates_with_and_without_cache <- function(...) {
      terminates_with_and_without_full_cache_then(
        \(x, y) succeed(),
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

  # example inputs
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

  it("gives a deterministic result, except for per-dependant dependency order", {
    two_copies <- function(fn) function(df) fn(df, df)
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
  it("doesn't have excluded attributes in any determinant sets", {
    gen_df_and_exclude <- function(nrow, ncol, remove_dup_rows = FALSE) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.element(names(df))))
    }
    terminates_with_exclusion_then_no_trival <- function(accuracy, ...) {
      function(df, attr) {
        deps <- with_timeout(discover(df, accuracy = accuracy, exclude = attr, ...))
        if (is.null(deps))
          return(fail("discover() with exclude timed out"))
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
  it("gives determinants for unique attributes", {
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
  it("gives a minimal functional dependency set", {
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

  # metamorphic tests
  it("loses FDs involving a removed attribute, keeps the rest", {
    gen_df_and_remove_col <- function(nrow, ncol, remove_dup_rows = FALSE) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.int(ncol(df)))) |>
        gen.with(uncurry(\(df, n) {
          list(df, df[, -n, drop = FALSE])
        }))
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
  it("keeps subsets of all FDs if a row is removed, might have more", {
    gen_df_and_remove_row <- function(nrow, ncol) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows = TRUE) |>
        gen.and_then(\(df) list(df, gen.element(seq_len(nrow(df))))) |>
        gen.with(uncurry(\(df, n) {
          list(df, df[-n, , drop = FALSE])
        }))
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
  it("is commutative with changing attribute names", {
    gen_df_and_name_change <- function(nrow, ncol, remove_dup_rows = FALSE) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.sample(LETTERS, ncol(df)))) |>
        gen.with(uncurry(\(df, new_names) {
          list(df, stats::setNames(df, new_names))
        }))
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
        gen.and_then(uncurry(\(df, attr) list(
          gen.pure(df),
          gen.pure(attr),
          gen_perm(df[[attr]])
        ))) |>
        gen.with(uncurry(\(df, attr, permuted_attr) {
          permed <- df
          permed[[attr]] <- permuted_attr
          list(df, permed)
        }))
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
  it("is invariant to an attribute's class being losslessly changed", {
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
      both_terminate_then(expect_equiv_deps, 1, method = "DFD"),
      curry = TRUE
    )
    forall(
      gen_df_and_type_change(4, 6),
      both_terminate_then(expect_equiv_deps, 1, method = "FDHitsSep"),
      curry = TRUE
    )
    forall(
      gen_df_and_type_change(4, 6),
      both_terminate_then(expect_equiv_deps, 1, method = "FDHitsJoint"),
      curry = TRUE
    )
  })
  it("is invariant to attributes being reordered, except for attrs_order", {
    gen_df_and_attr_perm <- function(
      nrow,
      ncol,
      remove_dup_rows = FALSE
    ) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(
          gen.pure(df),
          gen.sample(seq_along(df), size = ncol(df))
        )) |>
        gen.with(uncurry(\(df, perm) {
          list(df, df[, perm, drop = FALSE])
        }))
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
  it("is invariant to changes of accuracy within same required row count when using DFD", {
    gen_df_and_accuracy_nrow <- function(nrow, ncol, remove_dup_rows = FALSE) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.int(nrow(df)))) |>
        gen.with(uncurry(\(df, n) {
          prop <- n/nrow(df)
          low <- (n - 1)/nrow(df) + 1e-9
          list(df, low, prop)
        }))
    }
    both_bounds_terminate_then <- function(fn, ...) {
      function(df, low, high) {
        res1 <- with_timeout(discover(df, accuracy = low, ...))
        if (is.null(res1))
          return(fail("discover() with lower accuracy timed out"))
        res2 <- with_timeout(discover(df, accuracy = high, ...))
        if (is.null(res2))
          return(fail("discover() with higher accuracy timed out"))
        fn(res1, res2)
      }
    }
    forall(
      gen_df_and_accuracy_nrow(4, 6),
      both_bounds_terminate_then(expect_equiv_deps, method = "DFD"),
      curry = TRUE
    )
  })
  it(
    paste(
      "is invariant to:",
      "- having a non-false keep_rownames vs. adding row names as first column",
      "- method used (if accuracy = 1)",
      "- excluding a class vs. excluding attributes in that class",
      "- filtering by arguments (dependants/detset_limit) or by subsetting results",
      "- whether stripped partitions or their sizes are cached",
      "- whether partition is transferred between dependants",
      "- whether bijections are skipped (if accuracy = 1)",
      sep = "\n"
    ),
    {
      expect_invariant_to_input_options <- function(
        df,
        accuracy,
        keep_rownames,
        dependants,
        detset_limit
      ) {
        arglists <- expand.grid(
          if (isFALSE(keep_rownames))
            list(list(df = df, keep_rownames = FALSE))
          else{
            tmp <- if (isTRUE(keep_rownames)) "row" else keep_rownames
            nm <- make.unique_after(tmp, names(df))
            list(
              list(df = df, keep_rownames = nm),
              list(
                df = cbind(setNames(data.frame(rownames(df)), nm), df),
                keep_rownames = FALSE
              )
            )
          },
          list(list(accuracy = accuracy)),
          c(
            list(list(method = "DFD")),
            if (accuracy == 1)
              list(list(method = "FDHitsSep"), list(method = "FDHitsJoint"))
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
            list(full_cache = FALSE),
            list(full_cache = TRUE)
          ),
          list(
            list(store_cache = FALSE),
            list(store_cache = TRUE)
          ),
          c(
            list(list(skip_bijections = FALSE)),
            if (accuracy == 1) list(list(skip_bijections = TRUE))
          )
        ) |>
          unname() |>
          apply(1, \(x) do.call(c, x), simplify = FALSE)
        results <- lapply(
          arglists,
          \(lst) {
            base <- with_timeout(do.call(discover, lst))
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
          return(fail("some argument lists time out"))
        expect_true(all(vapply(results, fds_equivalent, logical(1), results[[1]])))
      }
      forall(
        gen_df(4, 6) |>
          gen.and_then(\(x) {
            list(
              gen.pure(x),
              gen.choice(gen.pure(1), gen.unif(0, 1), prob = c(80, 20)),
              gen.choice(
                gen.element(c(FALSE, TRUE)),
                gen_attr_name(9) |>
                  gen.with(\(nm) make.unique_after(nm, names(x)))
              ),
              gen.sample_resampleable(names(x), from = 0, to = ncol(x)),
              gen.element(0:(ncol(x) - 1L))
            )
          }),
        expect_invariant_to_input_options,
        curry = TRUE
      )
    }
  )
})
