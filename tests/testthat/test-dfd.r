library(R.utils)
library(hedgehog)

describe("dfd", {
  it("terminates for simple logical relations", {
    gen_ncol <- gen.int(3)
    gen_len <- gen.int(5)
    gen_df <- generate(
      for (n_col in gen_ncol) {
        generate(
          for (n in gen_len) {
            rep(
              list(gen.sample(c(FALSE, TRUE), n, replace = TRUE)),
              n_col
            ) |>
              setNames(LETTERS[1:n_col])
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

  it("terminates for long tables with dependent columns", {
    gen_len2 <- gen.int(10)
    gen_df2 <- generate(
      for (n in gen_len2) {
        gen.with(
          list(
            A = 1:n,
            B = gen.sample(0:25, n, replace = TRUE),
            C = gen.sample(0:3, n, replace = TRUE),
            D = gen.sample((-10):20, n, replace = TRUE)
          ),
          function(lst) {
            df <- as.data.frame(lst)
            df$E <- df$C != 1
            df$F <- df$B < 10
            df$G <- df$C + df$D
            df
          }
        )
      }
    )
    forall(
      gen_df2,
      function(df) {
        df <- as.data.frame(df)
        res <- withTimeout(dfd(df, 1), timeout = 5, onTimeout = "silent")
        expect_true(!is.null(res))
      },
      shrink.limit = Inf
    )
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
    expect_identical(lengths(deps), lengths(expected_deps))
    expect_superset_of_dependency(deps, expected_deps)
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
      assert_equal_dependency_dics(solved, dep)
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
      expect_superset_of_dependency(res, dep)
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
