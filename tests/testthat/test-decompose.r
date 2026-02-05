describe("decompose", {
  it("returns valid databases", {
    forall(
      list(
        gen_df(6, 7, variant = "tibble"),
        gen.choice(gen.element(7:1), gen.pure(NA_integer_)),
        gen.element(c(FALSE, TRUE))
      ) |>
        gen.with(\(x) c(x[1], list(as.list(x[[1]])), x[2:3])),
      \(x, x2, digits, check) {
        fds <- discover(x, digits = digits)
        schema <- normalise(fds)
        db <- decompose(x, schema, digits = digits, check = check)
        is_valid_database(db)
      },
      curry = TRUE
    )
  })
  it("removes extraneous dependencies", {
    df <- data.frame(a = integer(), b = integer(), c = integer())
    schema <- relation_schema(
      list(a = list(c("a", "b", "c"), list("a"))),
      attrs_order = c("a", "b", "c")
    ) |>
      database_schema(references = list())
    db <- decompose(df, schema)
    expect_identical(
      db,
      database(
        relation(
          list(a = list(
            df = df,
            keys = list("a")
          )),
          attrs_order = c("a", "b", "c")
        ),
        references = list()
      )
    )
  })
  it("resolves a simple bijection with no splits", {
    df <- data.frame(a = integer(), b = integer())
    schema <- relation_schema(
      list(a = list(c("a", "b"), list("a", "b"))),
      attrs_order = c("a", "b")
    ) |>
      database_schema(references = list())
    db <- decompose(df, schema)
    expect_identical(
      db,
      database(
        relation(list(a = list(df = df, keys = list("a", "b"))), c("a", "b")),
        list()
      )
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
        references = list(list(
          "id",
          c("month", "hemisphere"),
          "month_hemisphere",
          c("month", "hemisphere")
        ))
      )
    new_db <- decompose(df, schema)
    expected_db <- database(
      relation(
        list(
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
        attrs_order = c("id", "month", "hemisphere", "is_winter")
      ),
      references = list(list(
        "id",
        c("month", "hemisphere"),
        "month_hemisphere",
        c("month", "hemisphere")
      ))
    )
    expect_identical(new_db, expected_db)
  })
  it("returns a error if data.frame doesn't satisfy FDs in the schema", {
    add_id_attribute <- function(df) {
      df2 <- cbind(df, a = seq.int(nrow(df)))
      names(df2) <- make.names(names(df2), unique = TRUE)
      df2
    }
    gen_fd_reduction_for_df <- function(df) {
      true_fds <- discover(df)
      nonempty_detsets <- which(lengths(detset(true_fds)) > 0L)
      if (length(nonempty_detsets) == 0)
        return(gen.pure(list(df, NULL, NULL)))
      gen.element(nonempty_detsets) |>
        gen.with(\(index) true_fds[[index]]) |>
        gen.and_then(\(fd) list(gen.pure(fd), gen.int(length(detset(fd))))) |>
        gen.with(\(lst) c(list(df), lst))
    }
    gen_df_and_fd_reduction <- function(nrow, ncol) {
      gen_df(nrow, ncol, minrow = 2L, mincol = 2L, remove_dup_rows = TRUE) |>
        gen.with(add_id_attribute) |>
        gen.and_then(gen_fd_reduction_for_df)
    }
    expect_decompose_error <- function(df, reduced_fd, removed_det) {
      if (nrow(df) <= 1)
        discard()
      flat_deps <- discover(df)
      reduced_index <- match(reduced_fd, flat_deps)
      if (is.na(reduced_index))
        stop("reduced_fd doesn't exist")
      reduced_deps <- flat_deps
      detset(reduced_deps)[[reduced_index]] <-
        detset(reduced_deps)[[reduced_index]][-removed_det]
      schema <- normalise(reduced_deps)
      expect_error(
        decompose(df, schema, check = TRUE),
        paste0(
          "\\A",
          "df doesn't satisfy functional dependencies in schema:",
          "(\\n\\{.*\\} -> .*)+",
          "\\Z"
        ),
        perl = TRUE
      )
      element_regex <- ".+\\.\\{[^}]*\\}"
      expect_error(
        decompose(df, schema, check = FALSE),
        paste0(
          "\\A",
          "relations must satisfy their keys:",
          " element",
          paste0(
            "(",
            " ", element_regex,
            "|",
            "s ", element_regex, "(, ", element_regex, ")*",
            ")"
          ),
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
  it("returns a error if data.frame doesn't satisfy FKs in the schema", {
    gen_df_and_fk_reduction <- function(nrow, ncol) {
      gen_df(nrow, ncol, minrow = 4L, mincol = 4L, remove_dup_rows = TRUE) |>
        # change factors to characters, since they cause merge problems when
        # merged with non-factor non-character vectors that aren't in their
        # level set
        gen.with(fac2char) |>
        gen.and_then(gen_fk_reduction_for_df)
    }
    expect_fk_error <- function(df, dbs) {
      if (nrow(df) <= 1 || is.null(dbs))
        discard()
      name_regexp <- "[\\w\\. ]+"
      fk_half_regexp <- paste0(
        name_regexp,
        "\\.\\{", name_regexp, "(, ", name_regexp, ")*\\}"
      )
      expect_error(
        decompose(df, dbs, check = TRUE),
        paste0(
          "\\A",
          "relations must satisfy references in schema:",
          "(\\n", fk_half_regexp, " -> ", fk_half_regexp, ")+",
          "\\Z"
        ),
        perl = TRUE
      )
    }
    forall(
      gen_df_and_fk_reduction(6, 7),
      expect_fk_error,
      discard.limit = 200,
      curry = TRUE
    )
  })
  it("is equivalent to create >> insert for valid data", {
    forall(
      list(
        gen_df(6, 7, remove_dup_rows = TRUE),
        gen.element(c(7:1, NA)),
        gen.element(c(FALSE, TRUE))
      ) |>
        gen.with(uncurry(\(df, digits, check) {
          list(
            df = df,
            schema = normalise(
              discover(df, digits = digits),
              remove_avoidable = TRUE
            ),
            digits = digits,
            check = check
          )
        })),
      \(df, schema, digits, check) {
        expect_identical(
          decompose(df, schema, digits = digits,  check = check),
          create(schema) |> insert(df, digits = digits)
        )
      },
      curry = TRUE
    )
    forall(
      list(
        gen_df(6, 7, remove_dup_rows = TRUE),
        gen.element(c(7:1, NA)),
        gen.element(c(FALSE, TRUE))
      ) |>
        gen.with(uncurry(\(df, digits, check) {
          list(
            df = df,
            schema = normalise(
              discover(df, keep_rownames = TRUE, digits = digits),
              remove_avoidable = TRUE
            ),
            digits = digits,
            check = check
          )
        })),
      \(df, schema, digits, check) {
        expect_identical(
          decompose(df, schema, keep_rownames = TRUE, digits = digits,  check = check),
          create(schema) |> insert(df, keep_rownames = TRUE, digits = digits)
        )
      },
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
          references = list(
            list("player_name_jersey_num", "team", "team", "team"),
            list("team", "city", "city", "city")
          )
        )
      db <- decompose(df, schema)
      expect_identical(length(db), 3L)
      expected_db <- database(
        relation(
          list(
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
          attrs_order = c("player_name", "jersey_num", "team", "city", "state")
        ),
        references = list(
          list("player_name_jersey_num", "team", "team", "team"),
          list("team", "city", "city", "city")
        )
      )
      expect_identical(db, expected_db)
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
      database_schema(references = list())
    db <- decompose(df, schema)
    expect_setequal(attrs(db)[[1]], c("A 1", "B 2", "C 3"))
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
      database_schema(references = list(list("a_c", "a", "a", "a")))
    db <- decompose(df, schema)
    expect_identical(
      records(db)$a_c,
      data.frame(a = 1:2, c = rep(1:2, each = 2), row.names = 1:4)
    )
    expect_identical(
      keys(db)$a_c,
      list(c("a", "c"))
    )
  })
})

describe("create_insert", {
  it("is equivalent to create >> insert", {
    forall(
      list(
        gen_df(6, 7, remove_dup_rows = TRUE),
        gen.element(c(7:1, NA_integer_))
      ) |>
        gen.with(uncurry(\(df, digits) list(
          df = df,
          schema = synthesise(discover(df, digits = digits)),
          digits = digits
        ))),
      \(df, schema, digits) {
        expect_identical(
          create_insert(df, schema, digits = digits),
          insert(create(schema), df, digits = digits)
        )
      },
      curry = TRUE
    )
  })
})
