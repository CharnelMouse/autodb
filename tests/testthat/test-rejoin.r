library(hedgehog)

describe("rejoin", {
  it("is left-inverse for lossless full-dep database creation, outside of row permutations, for tables with unique rows", {
    autodb_inverted_by_rejoin <- expect_bi(
      df_equiv,
      with_args(autodb, ensure_lossless = TRUE) %>>%
        rejoin,
      identity
    )
    table_dum <- data.frame()
    table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
    autodb_inverted_by_rejoin(table_dum)
    autodb_inverted_by_rejoin(table_dee)
    # 6 columns allows for interesting cases, such as a table containing two
    # independent ones, or a reference involving several attributes
    forall(
      gen_df(6, 7, remove_dup_rows = TRUE),
      autodb_inverted_by_rejoin,
      tests = 1000
    )
    forall(
      gen_df(6, 7, remove_dup_rows = FALSE),
      expect_bi(
        df_equiv,
        with_args(autodb, ensure_lossless = TRUE) %>>%
          rejoin,
        df_unique
      ),
      tests = 1000
    )
  })
  it("is possible for any database constructed from a data frame", {
    forall(
      gen.choice(0, 10) |>
        gen.list(of = 2) |>
        gen.and_then(uncurry(\(rows, cols) {
          gen_df(rows, cols, remove_dup_rows = TRUE)
        })) |>
        gen.with(with_args(autodb, ensure_lossless = TRUE)),
      rejoin %>>%
        with_args(try, silent = TRUE) %>>%
        with_args(expect_s3_class, class = "data.frame")
    )
  })
  it("handles databases with zero-column relations properly", {
    db <- database(
      relation(
        list(
          constants = list(
            df = data.frame(a = 1L)[, FALSE, drop = FALSE],
            keys = list(character())
          ),
          a = list(df = data.frame(a = 1L), keys = list("a"))
        ),
        "a"
      ),
      list()
    )
    expect_identical(rejoin(db), data.frame(a = 1L))
  })
  it("returns an error if any attributes aren't present in relations", {
    expect_error(
      rejoin(database(
        relation(setNames(list(), character()), "a"),
        list()
      )),
      "^database is not lossless: attributes in attrs_order not present in relations\\na$"
    )
    expect_error(
      rejoin(database(
        relation(setNames(list(), character()), c("a", "b")),
        list()
      )),
      "^database is not lossless: attributes in attrs_order not present in relations\\na, b$"
    )
  })
  it("returns an error if rejoining fails, with best joined attribute sets", {
    expect_error(
      rejoin(database(
        relation(
          list(
            a = list(df = data.frame(a = c(FALSE, TRUE)), keys = list("a")),
            b = list(df = data.frame(b = c(FALSE, TRUE)), keys = list("b"))
          ),
          c("a", "b")
        ),
        list()
      )),
      "^database can not be fully rejoined\\nbest joined sets:\\na\\nb$"
    )
    # below doesn't return {c} as a set, since we have {b, c}.
    expect_error(
      rejoin(database(
        relation(
          list(
            a = list(
              df = data.frame(a = c(FALSE, TRUE), b = c(FALSE, TRUE)),
              keys = list("a")
            ),
            c = list(
              df = data.frame(c = 1:4),
              keys = list("c")
            ),
            b_c = list(
              df = data.frame(b = c(FALSE, TRUE), c = 1:4),
              keys = list(c("b", "c"))
            ),
            b_c.1 = list(
              df = data.frame(b = c(FALSE, TRUE), c = 1:4),
              keys = list(c("b", "c"))
            )
          ),
          c("a", "b", "c")
        ),
        list()
      )),
      "^database can not be fully rejoined\\nbest joined sets:\\na, b\\nb, c$"
    )
    # sorts set contents, removes repeat sets
    expect_error(
      rejoin(database(
        relation(
          list(
            a = list(
              df = data.frame(a = logical()),
              keys = list("a")
            ),
            d = list(
              df = data.frame(d = logical(), c = logical()),
              keys = list("d")
            ),
            b_d = list(
              df = data.frame(b = logical(), d = logical()),
              keys = list(c("d"))
            )
          ),
          c("a", "b", "c", "d")
        ),
        list()
      )),
      "^database can not be fully rejoined\\nbest joined sets:\\na\\nb, c, d$"
    )
  })
})
