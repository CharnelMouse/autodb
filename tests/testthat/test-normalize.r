describe("normalize", {
  it("resolves a simple bijection with no splits, if given an index", {
    dependencies <- Dependencies(
      list(a = "b", b = "a"),
      primary_key = "a"
    )
    df <- data.frame(a = integer(), b = integer())

    norm.Dependencies <- normalize.Dependencies(dependencies, df)
    expect_identical(length(norm.Dependencies), 1L)
  })
  it("resolves a simple bijection with no splits, if given no index", {
    dependencies <- Dependencies(
      list(a = "b", b = "a")
    )
    df <- data.frame(a = integer(), b = integer())

    norm.Dependencies <- normalize.Dependencies(dependencies, df)
    skip("normalize fix needed")
    expect_identical(length(norm.Dependencies), 1L)
  })
  describe("Dependencies", {
    it("original test", {
      # how to test that relations remain the same???
      # check that there are no new relations?
      # there can be less however?
      dep_dic <- list(
        A = list(),
        B = list(),
        C = list(),
        D = list("F"),
        E = list(c("A", "B", "C", "D")),
        F = list(c("A", "B"))
      )
      dep <- Dependencies(
        dependencies = dep_dic,
        primary_key = c("A", "B", "C")
      )
      df <- data.frame(
        A = integer(),
        B = integer(),
        C = integer(),
        D = integer(),
        E = integer(),
        F = integer()
      )
      new <- normalize(dep, df)
      dep_dic <- dep$dependencies
      for (x in new) {
        trans_deps <- find_trans_deps(x)
        trans_deps <- filter(trans_deps, df)
        expect_identical(trans_deps, list())
        part_deps <- find_partial_deps(x)
        part_deps <- filter(part_deps, df)
        expect_identical(part_deps, list())
        dic <- x$dependencies
        expect_superset_of_dependency(dep_dic, dic)
      }
    })
  })
  describe("entityset", {
    it("original test", {
      df1 <- data.frame(test = 0:2)
      df2 <- data.frame(test = 0:2)
      accuracy <- 0.98

      es_empty <- EntitySet.data.frame(NULL, df_name = NA)
      es_empty$dataframes <- list()
      expect_error(
        normalize(es_empty, accuracy),
        "^This EntitySet is empty$"
      )

      es_one <- EntitySet(df1, df_name = NA_character_)
      es_norm <- normalize(es_one, accuracy)
      expect_identical(length(es_norm$dataframes), 1L)

      es_two <- EntitySet.data.frame(NULL, df_name = NA)
      es_two$dataframes <- list(df1 = df1, df2 = df2)
      expect_error(
        normalize(es_two, accuracy),
        "^There is more than one dataframe in this EntitySet$"
      )
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
    id = DepDF(
      deps = Dependencies(
        list(
          id = list(),
          month = list("id", c("hemisphere", "is_winter")),
          hemisphere = list(c("month", "is_winter"), "id")
        ),
        primary_key = "id"
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
      ),
      index = "id",
      children = "month_hemisphere",
      parent = NA_character_
    ),
    month_hemisphere = DepDF(
      deps = Dependencies(
        list(
          month = list(c("hemisphere", "is_winter")),
          hemisphere = list(c("month", "is_winter")),
          is_winter = list(c("month", "hemisphere"))
        ),
        primary_key = c("month", "hemisphere")
      ),
      df = data.frame(
        month = c("dec", "jul", "dec", "jul"),
        hemisphere = c("N", "N", "S", "S"),
        is_winter = c(TRUE, FALSE, FALSE, TRUE)
      ),
      index = c("month", "hemisphere"),
      children = character(),
      parent = "id"
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
