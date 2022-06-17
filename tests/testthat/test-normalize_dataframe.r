describe("normalize_dataframe", {
  it("removes extraneous dependencies", {
    dependencies <- list(
      list("a", "b"),
      list(c("a", "b"), "c")
    )
    df <- data.frame(a = integer(), b = integer(), c = integer())
    norm.df <- normalize_dataframe(df, dependencies)
    expect_identical(
      norm.df,
      list(a = list(
        df = df,
        keys = list("a"),
        index = "a",
        children = character()
      ))
    )
  })
  it("resolves a simple bijection with no splits", {
    dependencies <- list(
      list("a", "b"),
      list("b", "a")
    )
    df <- data.frame(a = integer(), b = integer())
    norm.df <- normalize_dataframe(df, dependencies)
    expect_identical(
      norm.df,
      list(a = list(
        df = df,
        keys = list("a", "b"),
        index = "a",
        children = character()
      ))
    )
  })
  it("correctly splits example data.frame for original make_indexes() test", {
    df <- data.frame(
      id = c(
        0, 1, 2, 3, 4,
        5, 6, 7, 8, 9
      ),
      month = c(
        'dec', 'dec', 'jul', 'jul', 'dec',
        'jul', 'jul', 'jul', 'dec', 'jul'
      ),
      hemisphere = c(
        'N', 'N', 'N', 'N', 'S',
        'S', 'S', 'S', 'S', 'N'
      ),
      is_winter = c(
        TRUE, TRUE, FALSE, FALSE, FALSE,
        TRUE, TRUE, TRUE, FALSE, FALSE
      )
    )
    deps <- list(
      list("id", "month"),
      list("id", "hemisphere"),
      list("id", "is_winter"),
      list(c("month", "hemisphere"), "is_winter"),
      list(c("month", "is_winter"), "hemisphere"),
      list(c("hemisphere", "is_winter"), "month")
    )
    new_dfs <- normalize_dataframe(df, deps)
    skip("do this later")
    expected_dfs <- list(
      id = list(
        deps = list(
          id = list(),
          month = list("id", c("hemisphere", "is_winter")),
          hemisphere = list(c("month", "is_winter"), "id")
        ),
        df = df[, c("id", "month", "hemisphere")]
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

    expect_identical(new_dfs, expected_dfs)
  })
  describe("Dependencies", {
    it("DepDF", {
      deps <- list(
        list(c("player_name", "jersey_num"), "team"),
        list(c("player_name", "team"), "jersey_num"),
        list(c("team", "jersey_num"), "player_name"),
        list("team", "city"),
        list("state", "city"),
        list(c("player_name", "jersey_num"), "city"),
        list("team", "state"),
        list(c("player_name", "jersey_num"), "state"),
        list("city", "state")
      )
      df <- data.frame(
        player_name = integer(),
        jersey_num = integer(),
        team = integer(),
        city = integer(),
        state = integer()
      )
      depdfs <- normalize_dataframe(df, deps)
      expect_identical(length(depdfs), 3L)
      expected_depdfs <- list(
        player_name_jersey_num = list(
          df = data.frame(
            player_name = integer(),
            jersey_num = integer(),
            team = integer()
          ),
          keys = list(
            c("player_name", "jersey_num"),
            c("player_name", "team"),
            c("team", "jersey_num")
          ),
          index = c("player_name", "jersey_num"),
          children = "team"
        ),
        city = list(
          df = data.frame(
            city = integer(),
            state = integer()
          ),
          keys = list("city", "state"),
          index = "city",
          children = character()
        ),
        team = list(
          df = data.frame(
            team = integer(),
            state = integer()
          ),
          keys = list("team"),
          index = "team",
          children = "city"
        )
      )
      expect_setequal(depdfs, expected_depdfs)
    })
  })
  it("correctly handles attributes with non-df-standard names", {
    df <- data.frame(1:3, c(1, 1, 2), c(1, 2, 2)) |>
      stats::setNames(c("A 1", "B 2", "C 3"))
    deps <- dfd(df, 1) |>
      tuple_relations()
    norm.df <- normalize_dataframe(df, deps)
    expect_setequal(names(norm.df[[1]]$df), c("A 1", "B 2", "C 3"))
  })
})

describe("normalize_dependencies() replacing normalize_step()", {
  it("removes extraneous dependencies", {
    dependencies <- list(
      list("a", "b"),
      list(c("a", "b"), "c")
    )
    norm.df <- normalize_dependencies(dependencies)
    expect_identical(
      norm.df,
      list(list(attrs = c("a", "b", "c"), keys = list("a")))
    )
  })
  it("resolves a simple bijection with no splits", {
    dependencies <- list(
      list("a", "b"),
      list("b", "a")
    )
    norm.df <- normalize_dependencies(dependencies)
    expect_identical(
      norm.df,
      list(list(attrs = c("a", "b"), keys = list("a", "b")))
    )
  })
  it("correctly splits example data.frame for original make_indexes() test", {
    # has multi-keys getting combined, so tests construct_relations
    # handles multiple values in RHS of input relations
    df <- data.frame(
      id = c(
        0, 1, 2, 3, 4,
        5, 6, 7, 8, 9
      ),
      month = c(
        'dec', 'dec', 'jul', 'jul', 'dec',
        'jul', 'jul', 'jul', 'dec', 'jul'
      ),
      hemisphere = c(
        'N', 'N', 'N', 'N', 'S',
        'S', 'S', 'S', 'S', 'N'
      ),
      is_winter = c(
        TRUE, TRUE, FALSE, FALSE, FALSE,
        TRUE, TRUE, TRUE, FALSE, FALSE
      )
    )
    deps <- list(
      list("id", "month"),
      list("id", "hemisphere"),
      list("id", "is_winter"),
      list(c("month", "hemisphere"), "is_winter"),
      list(c("month", "is_winter"), "hemisphere"),
      list(c("hemisphere", "is_winter"), "month")
    )
    new_deps <- normalize_dependencies(deps)
    expected_parent <- list(
      attrs = c("id", "month", "hemisphere"),
      keys = list("id")
    )
    expect_identical(length(new_deps[[1]]$attrs), 3L)
    expect_true("id" %in% new_deps[[1]]$attrs)
    expect_identical(
      length(intersect(
        new_deps[[1]]$attrs,
        c("month", "hemisphere", "is_winter")
      )),
      2L
    )
    expect_identical(new_deps[[1]]$keys, list("id"))
    expected_child <- list(
      attrs = c("month", "hemisphere", "is_winter"),
      keys = list(
        c("month", "hemisphere"),
        c("month", "is_winter"),
        c("hemisphere", "is_winter")
      )
    )
    expect_identical(new_deps[[2]], expected_child)
  })
  it("DepDF", {
    deps <- list(
      list(c("player_name", "jersey_num"), "team"),
      list(c("player_name", "team"), "jersey_num"),
      list(c("team", "jersey_num"), "player_name"),
      list("team", "city"),
      list("state", "city"),
      list(c("player_name", "jersey_num"), "city"),
      list("team", "state"),
      list(c("player_name", "jersey_num"), "state"),
      list("city", "state")
    )
    new_deps <- normalize_dependencies(deps)
    expected_deps <- list(
      list(
        attrs = c("player_name", "jersey_num", "team"),
        keys = list(
          c("player_name", "jersey_num"),
          c("player_name", "team"),
          c("team", "jersey_num")
        )
      ),
      list(
        attrs = c("team", "state"),
        keys = list("team")
      ),
      list(
        attrs = c("state", "city"),
        keys = list("state", "city")
      )
    )
    expect_setequal(new_deps, expected_deps)
  })
})
