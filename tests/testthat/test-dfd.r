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

  # it("terminates for long tables with dependent columns", {
  #   gen_len2 <- gen.int(10)
  #   gen_df2 <- generate(
  #     for (n in gen_len2) {
  #       gen.with(
  #         list(
  #           A = 1:n,
  #           B = gen.sample(0:25, n, replace = TRUE),
  #           C = gen.sample(0:3, n, replace = TRUE),
  #           D = gen.sample((-10):20, n, replace = TRUE)
  #         ),
  #         function(lst) {
  #           df <- as.data.frame(lst)
  #           df$E <- df$C != 1
  #           df$F <- df$B < 10
  #           df$G <- df$C + df$D
  #           df
  #         }
  #       )
  #     }
  #   )
  #   forall(
  #     gen_df2,
  #     function(df) {
  #       df <- as.data.frame(df)
  #       res <- withTimeout(dfd(df, 1), timeout = 5, onTimeout = "silent")
  #       expect_true(!is.null(res))
  #     },
  #     shrink.limit = Inf
  #   )
  # })
  it("gives dependencies for unique attributes (in case don't want them as key)", {
    df <- data.frame(A = 1:3, B = c(1, 1, 2), C = c(1, 2, 2))
    deps <- dfd(df, 1)
    expect_identical(deps$dependencies$A, list(c("B", "C")))
  })
  it("finds dependencies for the team data in test-normalize", {
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
  describe("dfd", {
    assert_equal_dependency_dics <- function(dep1, dep2) {
      stopifnot(sort(names(dep1)) == sort(names(dep2)))
      for (rhs in names(dep1)) {
        one <- dep1[[rhs]]
        two <- dep2[[rhs]]
        stopifnot(length(one) == length(two))
        for (one_el in one) {
          stopifnot(any(vapply(two, identical, logical(1), one_el)))
        }
      }
    }

    it("hard-coded example", {
      df_1 <- data.frame(
        id = c(100, 101, 102, 103, 104, 105, 106, 107, 109),
        age = c(1, 2, 3, 4, 5, 6, 7, 5, 6),
        height = c(4, 5, 6, 7, 8, 9, 10, 8, 9),
        less_than_5 = c(1, 1, 1, 1, 0, 0, 0, 0, 0)
      )
      dep <- list(
        id = list(),
        age = list("height", "id"),
        height = list("age", "id"),
        less_than_5 = list("age", "height", "id")
      )
      solved <- dfd(df_1, 0.98)
      assert_equal_dependency_dics(solved$dependencies, dep)
    })

    it("randomly-generated example", {
      # A = index,   B = random,   C = random,   D = random,
      # E = c != 1,   F = b < 10,   G = c + d
      df_2 <- data.frame(
        A = 1:1000,
        B = sample(0:25, 1000, replace = TRUE),
        C = sample(0:3, 1000, replace = TRUE),
        D = sample((-10):20, 1000, replace = TRUE)
      )
      df_2$E <- df_2$C != 1
      df_2$`F` <- df_2$B < 10
      df_2$G <- df_2$C + df_2$D

      dep <- list(
        A = list(),
        B = list("A"),
        C = list(c("D", "G"), "A"),
        D = list(c("C", "G"), "A"),
        E = list("C", c("D", "G"), "A"),
        F = list("B", "A"),
        G = list(c("C", "D"), "A")
      )
      res <- dfd(df_2, 0.98)
      expect_superset_of_dependency(res$dependencies, dep)
    })
  })

  describe("compute_partitions", {
    a <- c(
      6, 2, 3, 7, 8, 1, 0, 2, 0, 3,
      6, 0, 4, 6, 8, 7, 6, 8, 1, 5,
      1, 3, 3, 0, 0, 4, 5, 5, 7, 0,
      8, 2, 4, 7, 0, 0, 6, 4, 6, 8
    )
    b <- as.integer(a %% 2 == 0)
    c <- a + b < 4
    df <- data.frame(
      a = a,
      b = b,
      c = c
    )

    it("df", {
      expect_true(
        compute_partitions(df, 'c', c('a', 'b'), list(), 1.00)[[1]]
      )
      expect_true(
        compute_partitions(df, 'c', c('a', 'b'), list(), 0.90)[[1]]
      )
      expect_false(compute_partitions(df, 'a', 'c', list(), 1.00)[[1]])
      expect_false(compute_partitions(df, 'a', 'c', list(), 0.90)[[1]])
    })

    it("df2", {
      df2 <- df
      df2$c[1] <- TRUE
      expect_true(
        compute_partitions(df2, 'c', c('a', 'b'), list(), 0.97)[[1]]
      )
      expect_false(
        compute_partitions(df2, 'c', c('a', 'b'), list(), 0.98)[[1]]
      )
    })

    it("df3", {
      df3 <- df
      df3$c[1] <- TRUE
      df3$c[36] <- FALSE
      expect_true(
        compute_partitions(df3, 'c', c('a', 'b'), list(), 0.95)[[1]]
      )
      expect_false(
        compute_partitions(df3, 'c', c('a', 'b'), list(), 0.96)[[1]]
      )
    })
  })

  describe("approximate_dependencies", {
    a <- c(
      6, 2, 3, 7, 8, 1, 0, 2, 0, 3,
      6, 0, 4, 6, 8, 7, 6, 8, 1, 5,
      1, 3, 3, 0, 0, 4, 5, 5, 7, 0,
      8, 2, 4, 7, 0, 0, 6, 4, 6, 8
    )
    b <- as.integer(a %% 2 == 0)
    c <- (a + b < 4)
    df <- data.frame(a = a, b = b, c = c)
    it("df", {
      expect_true(approximate_dependencies(c("a", "b"), "c", df, 1.00))
      expect_true(approximate_dependencies(c("a", "b"), "c", df, 0.90))
    })
    it("is true only for accuracy up to 0.975, given one different row in forty", {
      df2 <- df
      df2$c[1] <- TRUE
      expect_true(approximate_dependencies(c("a", "b"), "c", df2, .9))
      expect_true(approximate_dependencies(c("a", "b"), "c", df2, .975))
      expect_false(approximate_dependencies(c("a", "b"), "c", df2, .98))
    })
    it("is true only for accuracy up to 0.95, given two different rows in forty", {
      df3 <- df
      df3$c[1] <- TRUE
      df3$c[36] <- FALSE
      expect_true(approximate_dependencies(c("a", "b"), "c", df3, .9))
      expect_true(approximate_dependencies(c("a", "b"), "c", df3, .95))
      expect_false(approximate_dependencies(c("a", "b"), "c", df3, .96))
    })
  })
})
