test_that("normalize", {
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
  dep <- list(
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
    for (rhs in names(dic)) {
      for (lhs in dic[[rhs]]) {
        expect_true(list(lhs) %in% dep_dic[[rhs]])
      }
    }
  }
})

test_that("find_most_comm", {
  deps <- list(
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
  new <- split_on_dep('B', list(dependencies = dep_dic))
  expect_identical(new[[1]]$dependencies, list(A = list(), B = list()))
  expect_identical(new[[2]]$dependencies, list(B = list(), C = list("B"), D = list("B")))
})

test_that("drop_primary_dups", {
  df_dic = list(
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
  df <- as.data.frame(df_dic)
  new_df <- drop_primary_dups(df, "city")

  df_new_dic <- list(
    city = c("boston", "dallas", "honolulu", "seattle"),
    state = c("MA", "TX", "HI", "WA"),
    is_liberal = c(TRUE, FALSE, TRUE, TRUE)
  )
  expect_equal(as.data.frame(df_new_dic), new_df)

  df <- data.frame(
    requires_light = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
    is_dark = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
    light_on = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )

  new_df <- drop_primary_dups(df, c('requires_light', 'is_dark'))
  # compare_df = pd.DataFrame([[TRUE, FALSE, FALSE], [FALSE, TRUE, FALSE], [TRUE, TRUE, TRUE]],
  #                           columns=["requires_light", "is_dark", "light_on"])
  # compare_df = compare_df.sort_values(by=["requires_light", "is_dark"]).reset_index(drop=TRUE)

  apply(
    new_df,
    1,
    \(row) {
      if (row['requires_light'] && !row['is_dark'])
        expect_false(row['light_on'])
      if (!row['requires_light'] && row['is_dark'])
        expect_false(row['light_on'])
      if (row['requires_light'] && row['is_dark'])
        expect_true(row['light_on'])
    }
  )
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

test_that("choose_index", {
  keys <- list('A', 'A_id', 'B')
  df <- data.frame(A = logical(), B = logical(), C = logical(), D = logical())
  expect_identical(choose_index(keys, df), 'A_id')

  keys <- list('B', 'C', 'A')
  expect_identical(choose_index(keys, df), 'A')

  keys <- list(c('A', 'C'), c('A', 'B'))
  expect_identical(choose_index(keys, df), c('A', 'B'))
})

test_that("normalize_dataframe", {
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
  deps <- list(
    dependencies = list(
      team = list(c('player_name', 'jersey_num')),
      jersey_num = list(c('player_name', 'team')),
      player_name = list(c('team', 'jersey_num')),
      city = list('team', 'state', c('player_name', 'jersey_num')),
      state = list('team', c('player_name', 'jersey_num'), 'city')
    ),
    primary_key = c('team', 'jersey_num')
  )

  depdf <- depDF(
    deps = deps,
    df = df,
    index = get_prim_key(deps)
  )
  new_dfs <- normalize_dataframe(depdf)
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

test_that("make_indexes", {
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
  deps = list(
    dependencies = list(
      id = list(),
      month = list('id', c('hemisphere', 'is_winter')),
      hemisphere = list(c('month', 'is_winter'), 'id'),
      is_winter = list(c('month', 'hemisphere'), 'id')
    ),
    primary_key = 'id'
  )

  depdf <- depDF(deps = deps, df = df, index = get_prim_key(deps))
  new_dfs <- normalize_dataframe(depdf)
  depdf <- new_dfs[[1]]
  new_dfs <- make_indexes(new_dfs)

  mask <- (new_dfs[[2]]$df[['month']] == 'dec') &
    (new_dfs[[2]]$df[['hemisphere']] == 'N')
  val <- new_dfs[[2]]$df[mask, , drop = FALSE][[colnames(new_dfs[[2]]$df)[1]]][1]
  expect_identical(new_dfs[[1]]$df[[colnames(new_dfs[[2]]$df)[1]]][1], val)
  expect_identical(new_dfs[[1]]$df[[colnames(new_dfs[[2]]$df)[1]]][2], val)

  mask <- (new_dfs[[2]]$df[['month']] == 'jul') &
    (new_dfs[[2]]$df[['hemisphere']] == 'N')
  val <- new_dfs[[2]]$df[mask, , drop = FALSE][[colnames(new_dfs[[2]]$df)[1]]][1]
  expect_identical(new_dfs[[1]]$df[[colnames(new_dfs[[2]]$df)[1]]][3], val)
  expect_identical(new_dfs[[1]]$df[[colnames(new_dfs[[2]]$df)[1]]][4], val)
  expect_identical(new_dfs[[1]]$df[[colnames(new_dfs[[2]]$df)[1]]][10], val)

  mask <- (new_dfs[[2]]$df[['month']] == 'dec') &
    (new_dfs[[2]]$df[['hemisphere']] == 'S')
  val <- new_dfs[[2]]$df[mask, , drop = FALSE][[colnames(new_dfs[[2]]$df)[1]]][1]
  expect_identical(new_dfs[[1]]$df[[colnames(new_dfs[[2]]$df)[1]]][5], val)
  expect_identical(new_dfs[[1]]$df[[colnames(new_dfs[[2]]$df)[1]]][9], val)

  mask <- (new_dfs[[2]]$df[['month']] == 'jul') &
    (new_dfs[[2]]$df[['hemisphere']] == 'S')
  val <- new_dfs[[2]]$df[mask, , drop = FALSE][[colnames(new_dfs[[2]]$df)[1]]][1]
  expect_identical(new_dfs[[1]]$df[[colnames(new_dfs[[2]]$df)[1]]][6], val)
  expect_identical(new_dfs[[1]]$df[[colnames(new_dfs[[2]]$df)[1]]][7], val)
  expect_identical(new_dfs[[1]]$df[[colnames(new_dfs[[2]]$df)[1]]][8], val)

  # Make sure new column names are sorted
  skip("wait until make_indexes working")
  expect_true(grepl(
    "hemisphere_month",
    paste(colnames(new_dfs[[1]]$df), collapse = "_"),
    fixed = TRUE
  ))
  expect_true(grepl(
    "hemisphere_month",
    paste(colnames(new_dfs[[2]]$df), collapse = "_"),
    fixed = TRUE
  ))
})
