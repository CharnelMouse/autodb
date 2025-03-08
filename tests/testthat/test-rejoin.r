describe("rejoin", {
  it("is left-inverse for lossless full-dep database creation, outside of row permutations, for tables with unique rows", {
    autodb_inverted_by_rejoin <- expect_bi(
      with_args(df_equiv, digits = NA),
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
        with_args(df_equiv, digits = NA),
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
  it("properly merges tables with close floating-point values (no duplicates)", {
    # The motivating cause of spurious duplicates on re-merges is that
    # merge.data.frame works differently if there is more than one "by" column:
    # in this case, it creates a grouping vector by pasting all the values in a
    # row together, with "\r" as a separator. This implicitly turns floats into
    # characters, i.e. represents them using only 15 sig. fig.s. Since going
    # above 15 sig. figs. results in behaviour that varies across machines (see
    # print.default), we have to test this behaviour without
    # rounding, so digits = NA.
    x <- data.frame(
      a = c(F, F, F, F, F, F, T, T),
      b = c(
        2.69353840461766669279,
        2.69353840461766669279,
        2.69353840461766713688,
        2.69353840461766713688,
        3.74416921885076714460,
        5.50801230419353693435,
        3.72990161259524111159,
        5.50801230419353693435
      ),
      c = c(
        -3.19131542233864262670,
        -3.19131542233864262670,
        -2.87397721325655908231,
        -1.98786466514381765514,
        -3.33190719804050772268,
        -3.33190719804050772268,
        -3.33190719804050772268,
        -3.33190719804050772268
      ),
      d = c(
        2.56310969849146363941,
        2.56310969849146363941,
        2.56310969849146363941,
        2.56310969849146363941,
        2.94505568569789533129,
        4.88640238864414033770,
        2.94505568569789533129,
        4.88640238864414211406
      ),
      e = 1:8
    )
    # rows are unique, even when using only 1 sig. fig., so no rows can be
    # considered duplicate on any machine.
    # abce[e].{a,b} -> abd[ab,bd].{a,b},
    # abcd[acd].{a,b} -> abd[ab,bd].{a,b}
    # abcd[acd].{b,d} -> abd[ab,bd].{b,d}
    # the merge duplicates occur when joining e and ab along the former
    # reference.
    db <- autodb(x, digits = NA)
    expect_false(!anyDuplicated(merge(records(db)$e, records(db)$a_b)))
    expect_silent(y <- rejoin(db))
    expect_true(df_equiv(y, x, digits = NA))
  })
})
