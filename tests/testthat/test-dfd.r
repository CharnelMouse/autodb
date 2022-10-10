# Possible additional properties
# Invariants
# + Finishes!
# + Running DFD twice gives equivalent results
# + Treats missing values as normal entries, i.e. replacing a value with NAs
# doesn't change results
# + FDs with the same RHS can't be LHSs where one is a subset of the other
# + invariant to permuting/changing values for an attribute (incl. NA)
# + invariant to changing type for an attribute
# - invariant to changing attribute order, except for order in dep elements and
# determinant sets
# - invariant to changing attribute names (including duplicate names?)
# - invariant to accuracy within same nrow threshold
# - removing a column removes FDs involving that column, the rest stay the same
# - removing a row keeps all previous FDs, might add more
# + doesn't have attribute in determinant sets if excluded
# + excluding type === excluding all attributes of that type
# Metamorphic/model
# - change attribute names then DFD === DFD then change attribute names

# Already implemented:

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
  expect_subset_deps <- function(deps1, deps2) {
    expect_identical(deps1$attrs, deps2$attrs)
    expect_identical(names(deps1$dependencies), names(deps2$dependencies))
    for (n in seq_along(deps1$dependencies))
      expect_true(all(is.element(deps1$dependencies[[n]], deps2$dependencies[[n]])))
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
  list_pair <- function(fn) {
    function(lst) {
      fn(lst[[1]], lst[[2]])
    }
  }

  it("terminates for simple logical relations", {
    is_named <- function(deps) {
      expect_named(deps, c("dependencies", "attrs"))
    }
    forall(gen_df(4, 6), terminates_then(is_named, 1), shrink.limit = Inf)
  })
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
  it("treats missing values as normal entries", {
    with_na_copy <- function(fn) {
      function(df) {
        na_df <- as.data.frame(lapply(
          df,
          \(x) {y <- x; y[!y] <- NA; y}
        ))
        fn(df, na_df)
      }
    }
    forall(gen_df(4, 6), with_na_copy(both_terminate_then(expect_equiv_deps, 1)))
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
      list_pair(both_terminate_then(expect_equiv_deps, 1))
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
      list_pair(both_terminate_then(expect_equiv_deps, 1))
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
      list_pair(terminates_with_exclusion_then(
        exclusion_not_in_determinant_sets,
        accuracy = 1
      ))
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
  it("doesn't consider attributes as determinants if given in exclude", {
    # no determinants to check, returns with calling find_LHSs
    df <- data.frame(A = 1:3, B = c(1, 1, 2), C = c(1, 2, 2))
    deps <- dfd(df, 1, exclude = c("B", "C"))
    expect_identical(deps$dependencies$A, list())

    # does call find_LHSs
    df2 <- data.frame(A = 1:3, B = c(1L, 1L, 2L), C = c(1, 2, 2))
    deps <- dfd(df2, 1, exclude = "C")
    expect_identical(deps$dependencies$A, list())

    # for constant dependents
    df3 <- data.frame(A = 1:3, B = c(1L, 1L, 1L))
    deps <- dfd(df3, 1, exclude = "A")
    expect_identical(deps$dependencies$B, list())
  })
  it("doesn't consider attributes as determinants if type is in exclude_class", {
    # no determinants to check, returns with calling find_LHSs
    df <- data.frame(A = 1:3, B = c(1, 1, 2), C = c(1, 2, 2))
    deps <- dfd(df, 1, exclude_class = "numeric")
    expect_identical(deps$dependencies$A, list())

    # does call find_LHSs
    df2 <- data.frame(A = 1:3, B = c(1L, 1L, 2L), C = c(1, 2, 2))
    deps <- dfd(df2, 1, exclude_class = "numeric")
    expect_identical(deps$dependencies$A, list())
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
      Thickness = list("Title"),
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
})

describe("original tests", {
  describe("compute_partitions", {
    a <- c(
      6, 2, 3, 7, 8, 1, 0, 2, 0, 3,
      6, 0, 4, 6, 8, 7, 6, 8, 1, 5,
      1, 3, 3, 0, 0, 4, 5, 5, 7, 0,
      8, 2, 4, 7, 0, 0, 6, 4, 6, 8
    )
    b <- as.integer(a %% 2 == 0)
    c <- as.integer((a + b < 4)) + 1L
    df <- data.frame(a = a, b = b, c = c)
    it("df", {
      expect_true(compute_partitions(df, "c", c("a", "b"), list(), 1)[[1]])
      expect_true(compute_partitions(df, "c", c("a", "b"), list(), 0.9)[[1]])
    })
    it("is true only for accuracy up to 0.975, given one different row in forty", {
      df2 <- df
      df2$c[1] <- 2L
      expect_true(compute_partitions(df2, "c", c("a", "b"), list(), 0.9)[[1]])
      expect_true(compute_partitions(df2, "c", c("a", "b"), list(), 0.975)[[1]])
      expect_false(compute_partitions(df2, "c", c("a", "b"), list(), 0.98)[[1]])
    })
    it("is true only for accuracy up to 0.95, given two different rows in forty", {
      df3 <- df
      df3$c[1] <- 2L
      df3$c[36] <- 1L
      expect_true(compute_partitions(df3, "c", c("a", "b"), list(), 0.9)[[1]])
      expect_true(compute_partitions(df3, "c", c("a", "b"), list(), 0.95)[[1]])
      expect_false(compute_partitions(df3, "c", c("a", "b"), list(), 0.96)[[1]])
    })
  })
})
