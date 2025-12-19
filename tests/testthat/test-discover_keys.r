# Various tests should have random accuracy draws, probably with lots of
# weight on 1

describe("discover_keys", {
  keys_equivalent <- function(keys1, keys2) {
    setequal(keys1, keys2)
  }
  expect_equiv_keys <- function(keys1, keys2) {
    expect_setequal(lapply(keys1, sort), lapply(keys2, sort))
  }
  expect_equiv_deps_except_names <- function(keys1, keys2, nms1, nms2) {
    renamed <- lapply(keys1, \(x) nms2[match(x, nms1)])
    expect_setequal(renamed, keys2)
  }
  expect_keep_non_removed_attr_keys <- function(keys1, keys2, removed_attr) {
    expect_length(removed_attr, 1)
    filtered <- keys1
    filtered <- filtered[vapply(
      filtered,
      Negate(is.element),
      logical(1),
      el = removed_attr
    )]
    if (length(filtered) == 0)
      return(succeed())
    expect_true(all(is.element(filtered, keys2)))
  }
  expect_key_subsets_kept <- function(keys1, keys2) {
    expect_true(all(vapply(
      keys1,
      \(key1) any(vapply(keys2, \(key2) all(is.element(key2, key1)), logical(1))),
      logical(1)
    )))
  }
  terminates_then <- function(fn, ...) {
    function(df) {
      res <- with_timeout(discover_keys(df, ...))
      if (is.null(res))
        return(fail("discover_keys() timed out"))
      succeed() # dummy success, otherwise tests complain about no expectations
      fn(res)
    }
  }
  both_terminate_then <- function(fn, ...) {
    function(df1, df2) {
      res1 <- with_timeout(discover_keys(df1, ...))
      if (is.null(res1))
        return(fail("first discover_keys() timed out"))
      res2 <- with_timeout(discover_keys(df2, ...))
      if (is.null(res2))
        return(fail("second discover_keys() timed out"))
      fn(res1, res2)
    }
  }

  # input requirements
  it("expects attribute names to be unique", {
    df <- data.frame(A = 1:3, B = c(1, 1, 2), A = c(1, 2, 2), check.names = FALSE)
    expect_error(discover_keys(df), "^duplicate column names: A$")
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
    terminates <- function(...) terminates_then(\(x, y) succeed(), ...)
    terminates()(df1)
    terminates()(df2)
  })
  it("can include the rownames, equivalent to adding them as first column", {
    x <- data.frame(
      a = c(1, 1, 1, 2, 2, 3, 3, 3, 4),
      b = c(1, 1, 1, 1, 1, 2, 2, 2, 3),
      row.names = letters[1:9]
    )
    default <- discover_keys(x, keep_rownames = TRUE)
    explicit <- discover_keys(x, keep_rownames = "id")
    expected <- discover_keys(x)
    expect_setequal(
      default,
      discover_keys(cbind(data.frame(row = rownames(x)), x))
    )
    expect_setequal(
      explicit,
      discover_keys(cbind(data.frame(id = rownames(x)), x))
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
    keys <- discover_keys(df)
    expect_gt(length(keys), 0)
    expect_length(keys[vapply(keys, Negate(is.element), logical(1), el = "time")], 0L)
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
      discover_keys(df, digits = 8),
      list(character())
    )
    expect_identical(
      discover_keys(df, digits = 15),
      list("y")
    )
  })
  it("correctly handles attributes with non-df-standard names", {
    df <- data.frame(1:3, c(1, 1, 2), c(1, 2, 2)) |>
      stats::setNames(c("A 1", "B 2", "C 3"))

    keys_dfd <- discover_keys(df)
    expect_true(all(vapply(
      keys_dfd,
      \(key) all(key %in% c("A 1", "B 2", "C 3")),
      logical(1)
    )))
  })

  # # example inputs
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
    expected_keys <- list(
      c("team", "jersey_num"),
      c("team", "player_name")
      # empirical {jersey_num, player_name} key is not expected
    )
    keys <- discover_keys(df)
    expect_true(all(is.element(expected_keys, keys)))
  })
  it("finds keys for the team data in original's edit demo", {
    df <- data.frame(
      team = c("tigers", "elephants", "foxes", "snakes", "dolphins", "eagles"),
      city = c("boston", "chicago", "miami", "austin", "honolulu", "houston"),
      state = c("MA", "IL", "FL", "TX", "HI", "TX"),
      roster_size = c(20L, 21L, 20L, 20L, 19L, 21L)
    )
    expected_keys <- list("team", "city")
    keys <- discover_keys(df)
    expect_true(all(is.element(expected_keys, keys)))
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
    expected_keys <- list(c("Title", "Format"))
    keys <- discover_keys(df)
    expect_true(all(is.element(expected_keys, keys)))
  })

  it("gives a deterministic result, except for per-dependant dependency order", {
    two_copies <- function(fn) function(df) fn(df, df)
    forall(
      gen_df(4, 6),
      two_copies(both_terminate_then(expect_setequal))
    )
  })
  it("doesn't have excluded attributes in any determinant sets", {
    gen_df_and_exclude <- function(nrow, ncol, remove_dup_rows = FALSE) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.element(names(df))))
    }
    terminates_with_exclusion_then_no_trivial <- function(...) {
      function(df, attr) {
        keys <- with_timeout(discover_keys(df, exclude = attr, ...))
        if (is.null(keys))
          return(fail("discover_keys() with exclude timed out"))
        # test exclusion not in keys
        expect_false(attr %in% unlist(keys))
      }
    }
    forall(
      gen_df_and_exclude(4, 6),
      terminates_with_exclusion_then_no_trivial(),
      curry = TRUE
    )
  })
  it("gives a valid set of keys", {
    forall(
      gen_df(6, 7),
      terminates_then(is_valid_key_set)
    )
  })
  it("gives sets with no duplicate values", {
    forall(
      gen_df(6, 7),
      function(df) {
        terminates_then(\(keys) expect_true(all(vapply(
          keys,
          \(key) !anyDuplicated(unique(df)[key]),
          logical(1)
        ))))(df)
      }
    )
  })
  #
  # metamorphic tests
  it("keeps keys that don't include a removed attribute, can gain others", {
    gen_df_and_remove_col <- function(nrow, ncol, remove_dup_rows = FALSE) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.int(ncol(df)))) |>
        gen.with(uncurry(\(df, n) {
          list(df, df[, -n, drop = FALSE])
        }))
    }
    forall(
      gen_df_and_remove_col(4, 6),
      \(df, df2) both_terminate_then(with_args(
        expect_keep_non_removed_attr_keys,
        setdiff(names(df), names(df2))
      ))(df, df2),
      curry = TRUE
    )
  })
  it("keeps subsets of all keys if a row is removed, might have more", {
    gen_df_and_remove_row <- function(nrow, ncol) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows = TRUE) |>
        gen.and_then(\(df) list(df, gen.element(seq_len(nrow(df))))) |>
        gen.with(uncurry(\(df, n) {
          list(df, df[-n, , drop = FALSE])
        }))
    }
    forall(
       gen_df_and_remove_row(4, 6),
      both_terminate_then(expect_key_subsets_kept),
      curry = TRUE
    )
  })
  it("is commutative with changing attribute names", {
    gen_df_and_name_change <- function(nrow, ncol, remove_dup_rows = FALSE) {
      gen_df(nrow, ncol, minrow = 1L, mincol = 1L, remove_dup_rows) |>
        gen.and_then(\(df) list(df, gen.sample(LETTERS, ncol(df)))) |>
        gen.with(uncurry(\(df, new_names) {
          list(
            df,
            stats::setNames(df, new_names),
            names(df),
            new_names
          )
        }))
    }
    forall(
      gen_df_and_name_change(4, 6),
      function(df1, df2, nms1, nms2) {
        both_terminate_then(with_args(
          expect_equiv_deps_except_names,
          nms1,
          nms2
        ))(df1, df2)
      },
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
      both_terminate_then(expect_equiv_keys),
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
      both_terminate_then(expect_equiv_keys),
      curry = TRUE
    )
  })
  it("is invariant to attributes being reordered, except for order", {
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
      both_terminate_then(expect_equiv_keys),
      curry = TRUE
    )
  })
  it(
    paste(
      "is invariant to:",
      "- having a non-false keep_rownames vs. adding row names as first column",
      "- excluding a class vs. excluding attributes in that class",
      "- filtering by arguments (dependants/detset_limit) or by subsetting results",
      sep = "\n"
    ),
    {
      expect_invariant_to_input_options <- function(
        df,
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
          list(
            list(exclude_class = "logical"),
            list(exclude = names(df)[vapply(df, is.logical, logical(1))])
          ),
          list(
            list(),
            list(dependants = dependants),
            list(detset_limit = detset_limit),
            list(dependants = dependants, detset_limit = detset_limit)
          )
        ) |>
          unname() |>
          apply(1, \(x) do.call(c, x), simplify = FALSE)
        results <- lapply(
          arglists,
          \(lst) {
            base <- with_timeout(do.call(discover_keys, lst))
            if (is.null(base))
              return(base)
            if (is.null(lst$detset_limit))
              base <- base[lengths(base) <= detset_limit]
            base
          }
        )
        if (any(vapply(results, is.null, logical(1))))
          return(fail("some argument lists time out"))
        expect_true(all(vapply(results, keys_equivalent, logical(1), results[[1]])))
      }
      forall(
        gen_df(4, 6) |>
          gen.and_then(\(x) {
            list(
              gen.pure(x),
              gen.choice(
                gen.element(c(FALSE, TRUE)),
                gen_attr_name(9) |>
                  gen.with(\(nm) make.unique_after(nm, names(x)))
              ),
              gen.sample_resampleable(names(x), from = 0, to = ncol(x)),
              gen.element(0:ncol(x))
            )
          }),
        expect_invariant_to_input_options,
        curry = TRUE
      )
    }
  )
})
