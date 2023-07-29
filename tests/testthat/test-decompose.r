library(hedgehog)

describe("decompose", {
  expect_database <- function(current, target) {
    expect_identical(current, structure(target, class = c("database", "list")))
  }

  it("returns valid databases", {
    forall(
      gen_df(6, 7),
      dup %>>%
        (onRight(
          with_args(discover, accuracy = 1) %>>%
            normalise
        )) %>>%
        uncurry(decompose) %>>%
        is_valid_database
    )
  })
  it("removes extraneous dependencies", {
    df <- data.frame(a = integer(), b = integer(), c = integer())
    schema <- relation_schema(
      list(a = list(c("a", "b", "c"), list("a"))),
      attrs_order = c("a", "b", "c")
    ) |>
      database_schema(relationships = list())
    norm.df <- decompose(df, schema)
    expect_database(
      norm.df,
      list(
        name = NA_character_,
        relations = list(a = list(
          df = df,
          keys = list("a")
        )),
        relationships = list(),
        attributes = c("a", "b", "c")
      )
    )
  })
  it("resolves a simple bijection with no splits", {
    df <- data.frame(a = integer(), b = integer())
    schema <- relation_schema(
      list(a = list(c("a", "b"), list("a", "b"))),
      attrs_order = c("a", "b")
    ) |>
      database_schema(relationships = list())
    norm.df <- decompose(df, schema)
    expect_identical(
      norm.df$relations,
      list(a = list(
        df = df,
        keys = list("a", "b")
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
    schema <- relation_schema(
      list(
        id = list(c("id", "month", "hemisphere"), list("id")),
        month_hemisphere = list(
          c("month", "hemisphere", "is_winter"),
          list(
            c("month", "hemisphere"),
            c("month", "is_winter"),
            c("hemisphere", "is_winter")
          )
        )
      ),
      attrs_order = c("id", "month", "hemisphere", "is_winter")
    ) |>
      database_schema(
        relationships = list(
          list(1:2, "month"),
          list(1:2, "hemisphere")
        )
      )
    new_dfs <- decompose(df, schema)
    expected_dfs <- list(
      name = NA_character_,
      relations = list(
        id = list(
          df = df[, c("id", "month", "hemisphere")],
          keys = list("id")
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
          )
        )
      ),
      relationships = list(
        c("id", "month", "month_hemisphere", "month"),
        c("id", "hemisphere", "month_hemisphere", "hemisphere")
      ),
      attributes = c("id", "month", "hemisphere", "is_winter")
    )
    expect_database(new_dfs, expected_dfs)
  })
  it("removes transitive relationships", {
    df <- data.frame(
      a = 1L,
      b = 1L,
      c = 1L,
      d = 1L,
      e = 1L
    )
    schema <- relation_schema(
      list(
        a = list(c("a", "b", "c"), list("a")),
        b_c = list(c("b", "c", "d"), list(c("b", "c"))),
        b = list(c("b", "e"), list("b"))
      ),
      attrs_order = c("a", "b", "c", "d", "e")
    ) |>
      database_schema(
        relationships = list(
          list(1:2, "b"),
          list(1:2, "c"),
          list(2:3, "b")
        )
      )
    new_dfs <- decompose(df, schema)
  })
  it("returns a error if data.frame doesn't satisfy FDs in the schema", {
    add_id_attribute <- function(df) {
      df2 <- cbind(df, a = seq.int(nrow(df)))
      names(df2) <- make.names(names(df2), unique = TRUE)
      df2
    }
    gen_fd_reduction_for_df <- function(df) {
      true_fds <- discover(df, 1)
      nonempty_detsets <- which(lengths(detset(true_fds)) > 0L)
      if (length(nonempty_detsets) == 0)
        return(gen.pure(list(df, NULL, NULL)))
      gen.element(nonempty_detsets) |>
        gen.with(\(index) list(detset(true_fds)[[index]], dependent(true_fds)[[index]])) |>
        gen.and_then(\(fd) list(fd, gen.int(length(fd[[1]])))) |>
        gen.with(\(lst) c(list(df), lst))
    }
    gen_df_and_fd_reduction <- function(nrow, ncol) {
      gen_df(nrow, ncol, minrow = 2L, remove_dup_rows = TRUE) |>
        gen.with(add_id_attribute) |>
        gen.and_then(gen_fd_reduction_for_df)
    }
    expect_decompose_error <- function(df, reduced_fd, removed_det) {
      if (nrow(df) <= 1)
        discard()
      flat_deps <- discover(df, 1)
      reduced_index <- match(list(reduced_fd), flat_deps)
      reduced_deps <- unclass(flat_deps)
      reduced_deps[[reduced_index]][[1]] <-
        reduced_deps[[reduced_index]][[1]][-removed_det]
      reduced_deps <- functional_dependency(reduced_deps, attrs(flat_deps))
      schema <- normalise(reduced_deps)
      expect_error(
        decompose(df, schema),
        paste0(
          "\\A",
          "df doesn't satisfy functional dependencies in schema:",
          "(\\n\\{.*\\} -> .*)+",
          "\\Z"
        ),
        perl = TRUE
      )
    }
    forall(
      gen_df_and_fd_reduction(6, 7),
      expect_decompose_error,
      discard.limit = 10L,
      curry = TRUE
    )
  })

  describe("Dependencies", {
    it("DepDF", {
      df <- data.frame(
        player_name = integer(),
        jersey_num = integer(),
        team = integer(),
        city = integer(),
        state = integer()
      )
      schema <- relation_schema(
        list(
          player_name_jersey_num = list(
            c("player_name", "jersey_num", "team"),
            list(
              c("player_name", "jersey_num"),
              c("player_name", "team"),
              c("team", "jersey_num")
            )
          ),
          city = list(
            c("city", "state"),
            list("city", "state")
          ),
          team = list(
            c("team", "city"),
            list("team")
          )
        ),
        attrs_order = c("player_name", "jersey_num", "team", "city", "state")
      ) |>
        database_schema(
          relationships = list(
            list(c(1L, 3L), "team"),
            list(3:2, "city")
          )
        )
      depdfs <- decompose(df, schema)
      expect_identical(length(depdfs$relations), 3L)
      expected_depdfs <- list(
        name = NA_character_,
        relations = list(
          player_name_jersey_num = list(
            df = data.frame(
              player_name = integer(),
              jersey_num = integer(),
              team = integer()
            ),
            keys = list(
              c("player_name", "jersey_num"),
              c("player_name", "team"),
              c("jersey_num", "team")
            )
          ),
          city = list(
            df = data.frame(
              city = integer(),
              state = integer()
            ),
            keys = list("city", "state")
          ),
          team = list(
            df = data.frame(
              team = integer(),
              city = integer()
            ),
            keys = list("team")
          )
        ),
        relationships = list(
          c("player_name_jersey_num", "team", "team", "team"),
          c("team", "city", "city", "city")
        ),
        attributes = c("player_name", "jersey_num", "team", "city", "state")
      )
      expect_database(depdfs, expected_depdfs)
    })
  })
  it("correctly handles attributes with non-df-standard names", {
    df <- data.frame(1:3, c(1, 1, 2), c(1, 2, 2)) |>
      stats::setNames(c("A 1", "B 2", "C 3"))
    schema <- relation_schema(
      list(
        `A 1` = list(
          c("A 1", "B 2", "C 3"),
          list("A 1", c("B 2", "C 3"))
        )
      ),
      attrs_order = c("A 1", "B 2", "C 3")
    ) |>
      database_schema(relationships = list())
    norm.df <- decompose(df, schema)
    expect_setequal(names(norm.df$relations[[1]]$df), c("A 1", "B 2", "C 3"))
  })
  it("links added key relations", {
    df <- data.frame(
      a = c(1L, 2L, 1L, 2L),
      b = c(1L, 2L, 1L, 2L),
      c = c(1L, 1L, 2L, 2L)
    )
    schema <- relation_schema(
      list(
        a = list(c("a", "b"), list("a", "b")),
        a_c = list(c("a", "c"), list(c("a", "c")))
      ),
      attrs_order = c("a", "b", "c")
    ) |>
      database_schema(relationships = list(list(2:1, "a")))
    norm.df <- decompose(df, schema)
    expect_identical(
      norm.df$relations$a_c,
      list(
        df = data.frame(a = 1:2, c = rep(1:2, each = 2), row.names = 1:4),
        keys = list(c("a", "c"))
      )
    )
  })
})
