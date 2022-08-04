describe("decompose", {
  it("removes extraneous dependencies", {
    dependencies <- list(
      dependencies = list(
        list("a", "b"),
        list(c("a", "b"), "c")
      ),
      attrs = c("a", "b", "c")
    )
    df <- data.frame(a = integer(), b = integer(), c = integer())
    norm_deps <- normalize_dependencies(dependencies)
    norm.df <- decompose(df, norm_deps)
    expect_identical(
      norm.df,
      list(a = list(
        df = df,
        keys = list("a"),
        index = "a",
        parents = character()
      ))
    )
  })
  it("resolves a simple bijection with no splits", {
    dependencies <- list(
      dependencies = list(
        list("a", "b"),
        list("b", "a")
      ),
      attrs = c("a", "b")
    )
    df <- data.frame(a = integer(), b = integer())
    norm_deps <- normalize_dependencies(dependencies)
    norm.df <- decompose(df, norm_deps)
    expect_identical(
      norm.df,
      list(a = list(
        df = df,
        keys = list("a", "b"),
        index = "a",
        parents = character()
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
      dependencies = list(
        list("id", "month"),
        list("id", "hemisphere"),
        list("id", "is_winter"),
        list(c("month", "hemisphere"), "is_winter"),
        list(c("month", "is_winter"), "hemisphere"),
        list(c("hemisphere", "is_winter"), "month")
      ),
      attrs = c("id", "month", "hemisphere", "is_winter")
    )
    norm_deps <- normalize_dependencies(deps)
    new_dfs <- decompose(df, norm_deps)
    expected_dfs <- list(
      id = list(
        df = df[, c("id", "month", "hemisphere")],
        keys = list("id"),
        index = "id",
        parents = "month_hemisphere"
      ),
      month_hemisphere = list(
        df = data.frame(
          month = c("dec", "jul", "dec", "jul"),
          hemisphere = c("N", "N", "S", "S"),
          is_winter = c(TRUE, FALSE, FALSE, TRUE),
          row.names = c(1L, 3L, 5:6)
        ),
        keys = list(
          c("month", "hemisphere"),
          c("month", "is_winter"),
          c("hemisphere", "is_winter")
        ),
        index = c("month", "hemisphere"),
        parents = character()
      )
    )
    expect_identical(new_dfs, expected_dfs)
  })
  it("removes transitive relationships", {
    df <- data.frame(
      a = 1L,
      b = 1L,
      c = 1L,
      d = 1L,
      e = 1L
    )
    deps <- list(
      dependencies = list(
        list("a", "b"),
        list("a", "c"),
        list(c("b", "c"), "d"),
        list("b", "e")
      ),
      attrs = c("a", "b", "c", "d", "e")
    )
    norm_deps <- normalize_dependencies(deps)
    new_dfs <- decompose(df, norm_deps)
    expect_identical(new_dfs$a$parents, "b_c")
  })

  describe("Dependencies", {
    it("DepDF", {
      deps <- list(
        dependencies = list(
          list(c("player_name", "jersey_num"), "team"),
          list(c("player_name", "team"), "jersey_num"),
          list(c("team", "jersey_num"), "player_name"),
          list("team", "city"),
          list("state", "city"),
          list(c("player_name", "jersey_num"), "city"),
          list("team", "state"),
          list(c("player_name", "jersey_num"), "state"),
          list("city", "state")
        ),
        attrs = c("player_name", "jersey_num", "team", "city", "state")
      )
      df <- data.frame(
        player_name = integer(),
        jersey_num = integer(),
        team = integer(),
        city = integer(),
        state = integer()
      )
      norm_deps <- normalize_dependencies(deps)
      depdfs <- decompose(df, norm_deps)
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
          parents = "team"
        ),
        city = list(
          df = data.frame(
            city = integer(),
            state = integer()
          ),
          keys = list("city", "state"),
          index = "city",
          parents = character()
        ),
        team = list(
          df = data.frame(
            team = integer(),
            city = integer()
          ),
          keys = list("team"),
          index = "team",
          parents = "city"
        )
      )
      expect_identical(depdfs, expected_depdfs)
    })
  })
  it("correctly handles attributes with non-df-standard names", {
    df <- data.frame(1:3, c(1, 1, 2), c(1, 2, 2)) |>
      stats::setNames(c("A 1", "B 2", "C 3"))
    deps <- list(
      dependencies = dfd(df, 1)$dependencies |>
        flatten(),
      attrs = c("A 1", "B 2", "C 3")
    )
    norm_deps <- normalize_dependencies(deps)
    norm.df <- decompose(df, norm_deps)
    expect_setequal(names(norm.df[[1]]$df), c("A 1", "B 2", "C 3"))
  })
})

describe("normalize_dependencies() replacing normalize_step()", {
  it("removes extraneous dependencies", {
    dependencies <- list(
      dependencies = list(
        list("a", "b"),
        list(c("a", "b"), "c")
      ),
      attrs = c("a", "b", "c")
    )
    norm.df <- normalize_dependencies(dependencies)
    expect_identical(
      norm.df,
      list(attrs = list(c("a", "b", "c")), keys = list(list("a")))
    )
  })
  it("resolves a simple bijection with no splits", {
    dependencies <- list(
      dependencies = list(
        list("a", "b"),
        list("b", "a")
      ),
      attrs = c("a", "b")
    )
    norm.df <- normalize_dependencies(dependencies)
    expect_identical(
      norm.df,
      list(attrs = list(c("a", "b")), keys = list(list("a", "b")))
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
      dependencies = list(
        list("id", "month"),
        list("id", "hemisphere"),
        list("id", "is_winter"),
        list(c("month", "hemisphere"), "is_winter"),
        list(c("month", "is_winter"), "hemisphere"),
        list(c("hemisphere", "is_winter"), "month")
      ),
      attrs = c("id", "month", "hemisphere", "is_winter")
    )
    new_deps <- normalize_dependencies(deps)
    expected_parent <- list(
      attrs = c("id", "month", "hemisphere"),
      keys = list("id")
    )
    expect_identical(length(new_deps$attrs[[1]]), 3L)
    expect_true("id" %in% new_deps$attrs[[1]])
    expect_identical(
      length(intersect(
        new_deps$attrs[[1]],
        c("month", "hemisphere", "is_winter")
      )),
      2L
    )
    expect_identical(new_deps$keys[[1]], list("id"))
    expected_child_attrs <- c("month", "hemisphere", "is_winter")
    expected_child_keys <- list(
      c("month", "hemisphere"),
      c("month", "is_winter"),
      c("hemisphere", "is_winter")
    )
    expect_identical(new_deps$attrs[[2]], expected_child_attrs)
    expect_identical(new_deps$keys[[2]], expected_child_keys)
  })
  it("DepDF", {
    deps <- list(
      dependencies = list(
        list(c("player_name", "jersey_num"), "team"),
        list(c("player_name", "team"), "jersey_num"),
        list(c("team", "jersey_num"), "player_name"),
        list("team", "city"),
        list("state", "city"),
        list(c("player_name", "jersey_num"), "city"),
        list("team", "state"),
        list(c("player_name", "jersey_num"), "state"),
        list("city", "state")
      ),
      attrs = c("player_name", "jersey_num", "team", "state", "city")
    )
    new_deps <- normalize_dependencies(deps)
    expected_deps <- list(
      attrs = list(
        c("player_name", "jersey_num", "team"),
        c("team", "state"),
        c("state", "city")
      ),
      keys = list(
        list(
          c("player_name", "jersey_num"),
          c("player_name", "team"),
          c("team", "jersey_num")
        ),
        list("team"),
        list("state", "city")
      )
    )
    expect_setequal(new_deps$attrs, expected_deps$attrs)
    expect_setequal(new_deps$keys, expected_deps$keys)
    expect_identical(
      match(new_deps$attrs, expected_deps$attrs),
      match(new_deps$keys, expected_deps$keys)
    )
  })
})
