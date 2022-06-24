describe("normalize_dependencies", {
  it("removes extraneous attributes", {
    dependencies <- list(
      dependencies = list(
        list("a", "b"),
        list(c("a", "b"), "c")
      ),
      attrs = c("a", "b", "c")
    )
    norm.dependencies <- normalize_dependencies(dependencies)
    expect_identical(
      norm.dependencies,
      list(list(attrs = c("a", "b", "c"), keys = list("a")))
    )
  })
  it("removes extraneous dependencies", {
    dependencies <- list(
      dependencies = list(
        list("a", "b"),
        list("a", "c"),
        list("b", "c")
      ),
      attrs = c("a", "b", "c")
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
      dependencies = list(
        list("d", "a"),
        list("d", "b"),
        list("a", "d"),
        list(c("b", "c"), "d")
      ),
      attrs = c("a", "b", "c", "d")
    )
    norm.dependencies <- normalize_dependencies(dependencies)
    expect_identical(
      norm.dependencies,
      list(
        list(attrs = c("a", "d", "b"), keys = list("a", "d")),
        list(attrs = c("b", "c", "a"), keys = list(c("b", "c")))
      )
    )
  })
  it("can handle basic bijections", {
    # A -> B, B -> A, A -> C, B -> C, A -> D, B -> F, D -> E, F -> E
    # => A <-> B, A -> CDF, D -> E, F -> E
    dependencies <- list(
      dependencies = list(
        list("a", "b"),
        list("b", "a"),
        list("a", "c"),
        list("b", "c"),
        list("a", "d"),
        list("b", "f"),
        list("d", "e"),
        list("f", "e")
      ),
      attrs = c("a", "b", "c", "d", "e", "f")
    )
    norm.dependencies <- normalize_dependencies(dependencies)
    expect_identical(
      norm.dependencies,
      list(
        list(attrs = c("a", "b", "c", "d", "f"), keys = list("a", "b")),
        list(attrs = c("d", "e"), keys = list("d")),
        list(attrs = c("f", "e"), keys = list("f"))
      )
    )
  })
  it("removes transient dependencies after-merging keys (DFD fig. 3)", {
    dependencies <- list(
      dependencies = list(
        list(c("x1", "x2"), "a"),
        list(c("x1", "x2"), "d"),
        list(c("c", "d"), "x1"),
        list(c("c", "d"), "x2"),
        list(c("a", "x1"), "b"),
        list(c("b", "x2"), "c"),
        list("c", "a")
      ),
      attrs = c("x1", "x2", "a", "b", "c", "d")
    )
    norm.dep <- normalize_dependencies(dependencies)
    expect_setequal(
      norm.dep,
      list(
        list(
          # contains a if trans_deps not removed
          attrs = c("x1", "x2", "c", "d"),
          keys = list(c("x1", "x2"), c("c", "d"))
        ),
        list(
          attrs = c("a", "x1", "b"),
          keys = list(c("a", "x1"))
        ),
        list(
          attrs = c("b", "x2", "c"),
          keys = list(c("b", "x2"))
        ),
        list(
          attrs = c("c", "a"),
          keys = list("c")
        )
      )
    )
  })
  it("replaces keys / non-key attributes with their bijection set's chosen index", {
    dependencies <- list(
      dependencies = list(
        list(c("A", "B"), "C"),
        list("C", "A"),
        list("C", "B"),
        list("C", "D"),
        list(c("A", "B", "E"), "F")
      ),
      attrs = c("A", "B", "C", "D", "E", "F")
    )
    norm.dep <- normalize_dependencies(dependencies)
    expect_identical(
      norm.dep,
      list(
        list(
          attrs = c("C", "A", "B", "D"),
          keys = list("C", c("A", "B"))
        ),
        list(
          attrs = c("C", "E", "F"),
          keys = list(c("C", "E"))
        )
      )
    )
  })
  describe("previous normalize tests", {
    it("resolves a simple bijection with no splits, if given an index", {
      dependencies <- list(
        dependencies = list(
          list("a", "b"),
          list("b", "a")
        ),
        attrs = c("a", "b")
      )
      norm <- normalize_dependencies(dependencies)
      expect_identical(length(norm), 1L)
      norm1 <- norm[[1]]
      expect_setequal(norm1$attrs, c("a", "b"))
      expect_setequal(norm1$keys, list("a", "b"))
    })
    it("resolves a simple bijection with no splits, if given no index", {
      dependencies <- list(
        dependencies = list(
          list("a", "b"),
          list("b", "a")
        ),
        attrs = c("a", "b")
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
          dependencies = list(
            list("F", "D"),
            list(c("A", "B", "C", "D"), "E"),
            list(c("A", "B"), "F")
          ),
          attrs = c("A", "B", "C", "D", "E", "F")
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
  it("prioritises columns earlier in the data.frame", {
    keys <- list(2L, 3L, 1L)
    expect_identical(choose_index(keys), 1L)
  })

  it("priorities key members earlier in the data.frame", {
    keys <- list(c(1L, 3L), c(1L, 2L))
    expect_identical(choose_index(keys), c(1L, 2L))
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
