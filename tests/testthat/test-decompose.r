library(hedgehog)

describe("decompose", {
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
        name = NA_character_,
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
      name = NA_character_,
      references = list(list(
        "id",
        c("month", "hemisphere"),
        "month_hemisphere",
        c("month", "hemisphere")
      ))
    )
    expect_identical(new_db, expected_db)
  })
  it("removes transitive references", {
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
        references = list(
          list("a", c("b", "c"), "b_c", c("b", "c")),
          list("b_c", "b", "b", "b")
        )
      )
    new_db <- decompose(df, schema)
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
        gen.with(\(index) list(detset(true_fds)[[index]], dependant(true_fds)[[index]])) |>
        gen.and_then(\(fd) list(gen.pure(fd), gen.int(length(fd[[1]])))) |>
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
      flat_deps <- discover(df, 1)
      reduced_index <- match(list(reduced_fd), flat_deps)
      reduced_deps <- unclass(flat_deps)
      reduced_deps[[reduced_index]][[1]] <-
        reduced_deps[[reduced_index]][[1]][-removed_det]
      reduced_deps <- functional_dependency(reduced_deps, attrs_order(flat_deps))
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
  it("returns a error if data.frame doesn't satisfy FKs in the schema", {
    fac2char <- function(df) {
      facs <- vapply(df, is.factor, logical(1))
      df[, facs] <- lapply(df[, facs, drop = FALSE], as.character)
      df
    }
    gen_fk_reduction_for_df <- function(df) {
      true_dbs <- normalise(discover(df, 1))
      true_fks <- references(true_dbs)
      true_fk_key_switch <- lapply(
        true_fks,
        \(fk) {
          len <- length(fk[[2]])
          new_tbs <- vapply(
            keys(true_dbs),
            \(ks) match(len, lengths(ks)),
            integer(1)
          )
          valid_new_tbs <- new_tbs[
            !is.na(new_tbs) &
              names(new_tbs) != fk[[1]] &
              names(new_tbs) != fk[[3]]
          ]
          new_fks <- Map(
            \(new_key_index, new_parent) {
              new_fk <- c(
                fk[1:2],
                list(new_parent, keys(true_dbs)[[new_parent]][[new_key_index]])
              )
              stopifnot(length(new_fk) == 4)
              if (!is.element(list(new_fk[[4]]), keys(true_dbs)[[new_fk[[3]]]]))
                stop("argh")
              new_fk
            },
            valid_new_tbs,
            names(valid_new_tbs)
          ) |>
            Filter(f = \(fk) !is.element(list(fk), true_fks)) |>
            Filter(f = \(fk) {
              length(remove_violated_references(
                list(fk),
                decompose(df, true_dbs)
              )) == 0
            })
          new_fks
        }
      )
      if (all(lengths(true_fk_key_switch) == 0))
        return(gen.pure(list(df, NULL)))
      gen.element(which(lengths(true_fk_key_switch) > 0)) |>
        gen.and_then(\(index) list(
          gen.pure(df),
          gen.element(true_fk_key_switch[[index]]) |>
            gen.with(\(new_fk) {
              dbs <- true_dbs
              references(dbs)[[index]] <- new_fk
              dbs
            })
        ))
    }
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
        decompose(df, dbs),
        paste0(
          "\\A",
          "relations must satisfy references in schema:",
          "(\\n", fk_half_regexp, " -> ", fk_half_regexp, ")+",
          "\\Z"
        ),
        perl = TRUE
      )
    }
    # forall(
    #   gen_df_and_fk_reduction(6, 7),
    #   expect_fk_error,
    #   tests = 1000,
    #   discard.limit = 910,
    #   curry = TRUE
    # )
  })
  it("is equivalent to create >> insert for valid data", {
    forall(
      gen_df(6, 7, remove_dup_rows = TRUE) |>
        gen.with(\(df) {
          list(
            df = df,
            schema = normalise(discover(df, 1), remove_avoidable = TRUE)
          )
        }),
      \(df, schema) {
        expect_identical(
          decompose(df, schema),
          create(schema) |> insert(df)
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
        name = NA_character_,
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
      gen_df(6, 7, remove_dup_rows = TRUE) |>
        gen.with(\(df) list(
          df = df,
          schema = synthesise(discover(df, 1))
        )),
      expect_biidentical(
        uncurry(create_insert),
        onRight(create) %>>% rev %>>% uncurry(insert)
      )
    )
  })
})
