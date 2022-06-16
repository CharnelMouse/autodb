describe("normalize_dependencies", {
  it("removes extraneous attributes", {
    dependencies <- list(
      list("a", "b"),
      list(c("a", "b"), "c")
    )
    norm.dependencies <- normalize_dependencies(dependencies)
    expect_identical(
      norm.dependencies,
      list(list(attrs = c("a", "b", "c"), keys = list("a")))
    )
  })
  it("removes extraneous dependencies", {
    dependencies <- list(
      list("a", "b"),
      list("a", "c"),
      list("b", "c")
    )
    norm.dependencies <- normalize_dependencies(dependencies)
    expect_identical(
      norm.dependencies,
      list(
        list(attrs = c("a", "b"), keys = list("a")),
        list(attrs = c("b", "c"), keys = list("b"))
      )
    )
  })
  it("merges equivalent keys", {
    dependencies <- list(
      list("d", "a"),
      list("d", "b"),
      list("a", "d"),
      list(c("b", "c"), "d")
    )
    norm.dependencies <- normalize_dependencies(dependencies)
    expect_identical(
      norm.dependencies,
      list(
        list(attrs = c("d", "a", "b"), keys = list("d", "a")),
        list(attrs = c("b", "c", "d"), keys = list(c("b", "c")))
      )
    )
  })
  it("can handle basic bijections", {
    # A -> B, B -> A, A -> C, B -> C, A -> D, B -> F, D -> E, F -> E
    # => A <-> B, A -> CDF, D -> E, F -> E
    dependencies <- list(
      list("a", "b"),
      list("b", "a"),
      list("a", "c"),
      list("b", "c"),
      list("a", "d"),
      list("b", "f"),
      list("d", "e"),
      list("f", "e")
    )
    norm.dependencies <- normalize_dependencies(dependencies)
    expect_identical(
      norm.dependencies,
      list(
        list(attrs = c("a", "b", "d", "c", "f"), keys = list("a", "b")),
        list(attrs = c("d", "e"), keys = list("d")),
        list(attrs = c("f", "e"), keys = list("f"))
      )
    )
  })
  describe("previous normalize tests", {
    it("resolves a simple bijection with no splits, if given an index", {
      dependencies <- list(
        list("a", "b"),
        list("b", "a")
      )
      norm <- normalize_dependencies(dependencies)
      expect_identical(length(norm), 1L)
      norm1 <- norm[[1]]
      expect_setequal(norm1$attrs, c("a", "b"))
      expect_setequal(norm1$keys, list("a", "b"))
    })
    it("resolves a simple bijection with no splits, if given no index", {
      dependencies <- list(
        list("a", "b"),
        list("b", "a")
      )
      norm <- normalize_dependencies(dependencies)
      expect_identical(length(norm), 1L)
      norm1 <- norm[[1]]
      expect_setequal(norm1$attrs, c("a", "b"))
      expect_setequal(norm1$keys, list("a", "b"))
    })
    describe("Dependencies", {
      it("original test", {
        # F->D, ABCD->E, AB->F
        # => F->D, ABC->E, AB->F
        dep <- list(
          list("F", "D"),
          list(c("A", "B", "C", "D"), "E"),
          list(c("A", "B"), "F")
        )
        new <- normalize_dependencies(dep)
        expected <- list(
          list(attrs = c("A", "B", "C", "E"), keys = list(c("A", "B", "C"))),
          list(attrs = c("A", "B", "F"), keys = list(c("A", "B"))),
          list(attrs = c("F", "D"), keys = list("F"))
        )
        expect_setequal(new, expected)
      })
    })
  })
})

test_that("find_most_comm", {
  deps <- Dependencies(
    dependencies = list(),
    primary_key = "d"
  )
  rels <- list(
    list('a', 'b'),
    list('b', 'c'),
    list('b', 'a'),
    list('d', 'a')
  )
  expect_identical(find_most_comm(rels, deps), 'b')
  rels <- list(
    list(c('a', 'c'), 'b'),
    list('b', 'c'),
    list('b', 'a'),
    list('d', 'a'),
    list(c('a', 'c'), 'b')
  )
  expect_identical(find_most_comm(rels, deps), 'b')
})

test_that("split_on_dep", {
  dep_dic <- list(
    A = list(),
    B = list(),
    C = list("A", "B"),
    D = list("B")
  )
  new <- split_on_dep('B', Dependencies(dependencies = dep_dic))
  expect_identical(new[[1]]$dependencies, list(A = list(), B = list()))
  expect_identical(
    new[[2]]$dependencies,
    list(B = list(), C = list("B"), D = list("B"))
  )
})

test_that("drop_primary_dups", {
  df <- data.frame(
    city = c(
      'honolulu', 'boston', 'honolulu', 'dallas', 'seattle',
      'honolulu', 'boston', 'honolulu', 'seattle', 'boston'
    ),
    state = c(
      'HI', 'MA', 'HI', 'TX', 'WA',
      'AL', 'MA', 'HI', 'WA', 'NA'
    ),
    is_liberal = c(
      TRUE, TRUE, TRUE, FALSE, TRUE,
      TRUE, TRUE, TRUE, TRUE, FALSE
    )
  )
  new_df <- drop_primary_dups(df, "city")
  df_new_dic <- data.frame(
    city = c("boston", "dallas", "honolulu", "seattle"),
    state = c("MA", "TX", "HI", "WA"),
    is_liberal = c(TRUE, FALSE, TRUE, TRUE)
  )
  expect_identical(new_df, df_new_dic)

  df <- data.frame(
    requires_light = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
    is_dark = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
    light_on = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  new_df <- drop_primary_dups(df, c('requires_light', 'is_dark'))
  expect_false(new_df$light_on[new_df$requires_light & !new_df$is_dark])
  expect_false(new_df$light_on[!new_df$requires_light & new_df$is_dark])
  expect_true(new_df$light_on[new_df$requires_light & new_df$is_dark])
})

test_that("filter", {
  keys <- list(
    list(c('A'), 'E'),
    list(c('A', 'B'), 'E'),
    list(c('C', 'D'), 'E')
  )
  df <- data.frame(
    A = numeric(),
    B = integer(),
    C = factor(),
    D = logical()
  )

  keys <- filter(keys, df)
  expect_identical(keys, list(list(c('C', 'D'), 'E')))
})

describe("choose_index", {
  df <- data.frame(A = logical(), B = logical(), C = logical(), D = logical())
  it("priorities columns with id prefix/suffix in the name", {
    keys <- list('A', 'A_id', 'B')
    expect_identical(choose_index(keys, df), 'A_id')
  })

  it("prioritises columns earlier in the data.frame", {
    keys <- list('B', 'C', 'A')
    expect_identical(choose_index(keys, df), 'A')
  })

  it("priorities key members earlier in the data.frame", {
    keys <- list(c('A', 'C'), c('A', 'B'))
    expect_identical(choose_index(keys, df), c('A', 'B'))
  })
})

describe("make_indexes", {
  new_dfs <- list(
    id = list(
      deps = list(
          id = list(),
          month = list("id", c("hemisphere", "is_winter")),
          hemisphere = list(c("month", "is_winter"), "id")
      ),
      df = data.frame(
        id = 0:9,
        month = c(
          "dec", "dec", "jul", "jul", "dec",
          "jul", "jul", "jul", "dec", "jul"
        ),
        hemisphere = c(
          "N", "N", "N", "N", "S",
          "S", "S", "S", "S", "N"
        )
      )
    ),
    month_hemisphere = list(
      deps = list(
        month = list(c("hemisphere", "is_winter")),
        hemisphere = list(c("month", "is_winter")),
        is_winter = list(c("month", "hemisphere"))
      ),
      df = data.frame(
        month = c("dec", "jul", "dec", "jul"),
        hemisphere = c("N", "N", "S", "S"),
        is_winter = c(TRUE, FALSE, FALSE, TRUE)
      )
    )
  )

  new_dfs <- make_indexes(new_dfs)

  apply_mask <- function(depdf, mask) {
    depdf$df[mask, 1]
  }
  foreign_vals <- new_dfs[[1]]$df[, colnames(new_dfs[[2]]$df)[1]]

  mask <- (new_dfs[[2]]$df[['month']] == 'dec') &
    (new_dfs[[2]]$df[['hemisphere']] == 'N')
  val <- apply_mask(new_dfs[[2]], mask)
  expect_identical(foreign_vals[1:2], rep(val, 2))

  mask <- (new_dfs[[2]]$df[['month']] == 'jul') &
    (new_dfs[[2]]$df[['hemisphere']] == 'N')
  val <- apply_mask(new_dfs[[2]], mask)
  expect_identical(foreign_vals[c(3, 4, 10)], rep(val, 3))

  mask <- (new_dfs[[2]]$df[['month']] == 'dec') &
    (new_dfs[[2]]$df[['hemisphere']] == 'S')
  val <- apply_mask(new_dfs[[2]], mask)
  expect_identical(foreign_vals[c(5, 9)], rep(val, 2))

  mask <- (new_dfs[[2]]$df[['month']] == 'jul') &
    (new_dfs[[2]]$df[['hemisphere']] == 'S')
  val <- apply_mask(new_dfs[[2]], mask)
  expect_identical(foreign_vals[6:8], rep(val, 3))

  # Make sure new column names are sorted
  skip("wait until make_indexes implemented")
  expect_true("hemisphere_month" %in% colnames(new_dfs[[1]]$df))
  expect_true("hemisphere_month" %in% colnames(new_dfs[[2]]$df))
})
