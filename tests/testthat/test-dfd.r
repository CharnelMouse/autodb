library(R.utils)
library(hedgehog)

describe("dfd", {
  it("terminates for simple logical relations", {
    gen_ncol_inc <- gen.int(4)
    gen_len_inc <- gen.int(6)
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
        res <- withTimeout(dfd(df, 1), timeout = 5, onTimeout = "silent")
        expect_true(!is.null(res))
      },
      shrink.limit = Inf
    )
  })
  it("terminates with trivially-dependent columns (not stuck on no MNFDs)", {
    df <- data.frame(
      A = 1:3,
      B = 1:3,
      C = 1:3
    )
    res <- withTimeout(dfd(df, 1), timeout = 5, onTimeout = "silent")
    expect_true(!is.null(res))
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
