describe("normalize_step", {
  it("removes extraneous dependencies", {
    dependencies <- Dependencies(
      list(a = character(), b = list("a"), c = list(c("a", "b"))),
      primary_key = "a"
    )
    df <- data.frame(a = integer(), b = integer(), c = integer())
    norm.df <- normalize_step.data.frame(df, dependencies)
    expect_identical(length(norm.df), 1L)
    expect_identical(
      norm.df[[1]]$deps,
      Dependencies(
        dependencies = list(a = character(), b = list("a"), c = list("a")),
        primary_key = "a"
      )
    )
  })
  it("resolves a simple bijection with no splits, if given an index", {
    dependencies <- Dependencies(
      list(a = "b", b = "a"),
      primary_key = "a"
    )
    df <- data.frame(a = integer(), b = integer())

    norm.df <- normalize_step.data.frame(df, dependencies)
    norm.DepDF <- normalize_step.DepDF(DepDF(dependencies, df))
    expect_identical(norm.df, norm.DepDF)
    expect_identical(length(norm.df), 1L)
  })
  it("resolves a simple bijection with no splits, if given no index", {
    dependencies <- Dependencies(
      list(a = "b", b = "a")
    )
    df <- data.frame(a = integer(), b = integer())

    norm.df <- normalize_step.data.frame(df, dependencies)
    norm.DepDF <- normalize_step.DepDF(DepDF(dependencies, df))
    expect_identical(norm.df, norm.DepDF)
    skip("normalize fix needed")
    expect_identical(length(norm.df), 1L)
  })
  it("correctly splits example data.frame for original make_indexes() test", {
    dic <- list(
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

    df <- as.data.frame(dic)
    deps = Dependencies(
      dependencies = list(
        id = list(),
        month = list('id', c('hemisphere', 'is_winter')),
        hemisphere = list(c('month', 'is_winter'), 'id'),
        is_winter = list(c('month', 'hemisphere'), 'id')
      ),
      primary_key = 'id'
    )

    depdf <- DepDF(deps = deps, df = df, index = get_prim_key(deps))
    new_dfs <- normalize_step(depdf)

    expected_dfs <- list(
      id = DepDF(
        deps = Dependencies(
          list(
            id = list(),
            month = list("id", c("hemisphere", "is_winter")),
            hemisphere = list(c("month", "is_winter"), "id")
          ),
          primary_key = "id"
        ),
        df = df[, c("id", "month", "hemisphere")],
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

    expect_identical(new_dfs, expected_dfs)
  })
  describe("Dependencies", {
    it("DepDF", {
      dic = list(
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
      df <- as.data.frame(dic)
      deps <- Dependencies(
        dependencies = list(
          team = list(c('player_name', 'jersey_num')),
          jersey_num = list(c('player_name', 'team')),
          player_name = list(c('team', 'jersey_num')),
          city = list('team', 'state', c('player_name', 'jersey_num')),
          state = list('team', c('player_name', 'jersey_num'), 'city')
        ),
        primary_key = c('team', 'jersey_num')
      )

      depdf <- DepDF(
        deps = deps,
        df = df,
        index = get_prim_key(deps)
      )
      new_dfs <- normalize_step(depdf)
      depdf <- new_dfs[[1]]

      expect_identical(length(new_dfs), 3L)

      dic_one <- list(
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
        )
      )

      dic_two <- list(
        team = c('Red', 'Orange', 'Yellow', 'Green', 'Blue', 'Blue'),
        city = c('boston', 'chicago', 'honolulu', 'boston', 'austin', 'austin')
      )

      dic_three <- list(
        city = c('boston', 'chicago', 'honolulu', 'austin', 'austin'),
        state = c('MA', 'IL', 'HI', 'TX', 'TX')
      )

      expect_identical(new_dfs[[1]]$df, drop_primary_dups(as.data.frame(dic_one), c('team', 'jersey_num')))
      expect_identical(new_dfs[[2]]$df, drop_primary_dups(as.data.frame(dic_two), 'team'))
      expect_identical(new_dfs[[3]]$df, drop_primary_dups(as.data.frame(dic_three), 'city'))
    })
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
