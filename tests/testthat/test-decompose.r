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
          with_args(dfd, accuracy = 1) %>>%
            flatten %>>%
            normalise %>>%
            cross_reference
        )) %>>%
        uncurry(decompose) %>>%
        is_valid_database
    )
  })
  it("removes extraneous dependencies", {
    df <- data.frame(a = integer(), b = integer(), c = integer())
    schema <- list(
      attrs = list(c("a", "b", "c")),
      keys = list(list("a")),
      parents = list(integer()),
      relationships = list(),
      relation_names = "a",
      all_attrs = c("a", "b", "c")
    )
    norm.df <- decompose(df, schema)
    expect_database(
      norm.df,
      list(
        name = NA_character_,
        relations = list(a = list(
          df = df,
          keys = list("a"),
          parents = character()
        )),
        relationships = list(),
        attributes = c("a", "b", "c")
      )
    )
  })
  it("resolves a simple bijection with no splits", {
    df <- data.frame(a = integer(), b = integer())
    schema <- list(
      attrs = list(c("a", "b")),
      keys = list(list("a", "b")),
      parents = list(integer()),
      relationships = list(),
      relation_names = "a",
      all_attrs = c("a", "b")
    )
    norm.df <- decompose(df, schema)
    expect_identical(
      norm.df$relations,
      list(a = list(
        df = df,
        keys = list("a", "b"),
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
    schema <- list(
      attrs = list(
        c("id", "month", "hemisphere"),
        c("month", "hemisphere", "is_winter")
      ),
      keys = list(
        list("id"),
        list(
          c("month", "hemisphere"),
          c("month", "is_winter"),
          c("hemisphere", "is_winter")
        )
      ),
      parents = list(2L, integer()),
      relationships = list(
        list(1:2, "month"),
        list(1:2, "hemisphere")
      ),
      relation_names = c("id", "month_hemisphere"),
      all_attrs = c("id", "month", "hemisphere", "is_winter")
    )
    new_dfs <- decompose(df, schema)
    expected_dfs <- list(
      name = NA_character_,
      relations = list(
        id = list(
          df = df[, c("id", "month", "hemisphere")],
          keys = list("id"),
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
          parents = character()
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
    schema <- list(
      attrs = list(
        c("a", "b", "c"),
        c("b", "c", "d"),
        c("b", "e")
      ),
      keys = list(
        list("a"),
        list(c("b", "c")),
        list("b")
      ),
      parents = list(2L, 3L, integer()),
      relationships = list(
        list(1:2, "b"),
        list(1:2, "c"),
        list(2:3, "b")
      ),
      relation_names = c("a", "b_c", "b"),
      all_attrs = c("a", "b", "c", "d", "e")
    )
    new_dfs <- decompose(df, schema)
    expect_identical(new_dfs$relations$a$parents, "b_c")
  })
  it("returns a error if data.frame doesn't satisfy FDs in the schema", {
    add_id_attribute <- function(df) {
      df2 <- cbind(df, a = seq.int(nrow(df)))
      names(df2) <- make.names(names(df2), unique = TRUE)
      df2
    }
    gen_fd_reduction_for_df <- function(df) {
      true_fds <- flatten(dfd(df, 1))
      nonempty_detsets <- which(vapply(
        true_fds,
        \(fd) length(fd[[1]]) > 0,
        logical(1)
      ))
      if (length(nonempty_detsets) == 0)
        return(gen.pure(list(df, NULL, NULL)))
      gen.element(nonempty_detsets) |>
        gen.with(\(index) true_fds[[index]]) |>
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
      flat_deps <- flatten(dfd(df, 1))
      reduced_index <- match(list(reduced_fd), flat_deps)
      reduced_deps <- flat_deps
      reduced_deps[[reduced_index]][[1]] <-
        flat_deps[[reduced_index]][[1]][-removed_det]
      schema <- cross_reference(normalise(reduced_deps))
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
      schema <- list(
        attrs = list(
          c("player_name", "jersey_num",  "team"),
          c("city",  "state"),
          c("team", "city")
        ),
        keys = list(
          list(
            c("player_name", "jersey_num"),
            c("player_name", "team"),
            c("team", "jersey_num")
          ),
          list("city", "state"),
          list("team")
        ),
        parents = list(3L, integer(), 2L),
        relationships = list(
          list(c(1L, 3L), "team"),
          list(3:2, "city")
        ),
        relation_names = c("player_name_jersey_num", "city", "team"),
        all_attrs = c("player_name", "jersey_num", "team", "city", "state")
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
              c("team", "jersey_num")
            ),
            parents = "team"
          ),
          city = list(
            df = data.frame(
              city = integer(),
              state = integer()
            ),
            keys = list("city", "state"),
            parents = character()
          ),
          team = list(
            df = data.frame(
              team = integer(),
              city = integer()
            ),
            keys = list("team"),
            parents = "city"
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
    schema <- structure(
      list(
        attrs = list(c("A 1", "B 2", "C 3")),
        keys = list(list("A 1", c("B 2", "C 3"))),
        parents = list(integer()),
        relationships = list(),
        all_attrs = c("A 1", "B 2", "C 3")
      ),
      class = c("database_schema", "list")
    )
    norm.df <- decompose(df, schema)
    expect_setequal(names(norm.df$relations[[1]]$df), c("A 1", "B 2", "C 3"))
  })
  it("links added key relations", {
    df <- data.frame(
      a = c(1L, 2L, 1L, 2L),
      b = c(1L, 2L, 1L, 2L),
      c = c(1L, 1L, 2L, 2L)
    )
    schema <- list(
      attrs = list(c("a", "b"), c("a", "c")),
      keys = list(list("a", "b"), list(c("a", "c"))),
      parents = list(integer(), 1L),
      relationships = list(list(2:1, "a")),
      relation_names = c("a", "a_c"),
      all_attrs = c("a", "b", "c")
    )
    norm.df <- decompose(df, schema)
    expect_identical(
      norm.df$relations$a_c,
      list(
        df = data.frame(a = 1:2, c = rep(1:2, each = 2), row.names = 1:4),
        keys = list(c("a", "c")),
        parents = "a"
      )
    )
  })
})
