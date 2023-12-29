library(hedgehog)

describe("relation", {
  it("expects valid input: relations is a list", {
    expect_error(relation(1L, character()))
  })
  it("expects valid input: list elements contain df and keys elements, and no others", {
    expect_error(relation(list(list(df = data.frame())), character()))
    expect_error(relation(
      list(list(df = data.frame(), keys = list(), extra = 1L)),
      character()
    ))
    expect_silent(relation(
      list(list(keys = list(), df = data.frame())),
      character()
    ))
  })
  it("expects valid input: attrs_order is a character", {
    expect_error(relation(list(), 1L))
  })
  it("expects valid input: df columns are from attrs_order, in order of key mentions first", {
    expect_error(relation(
      list(list(df = data.frame(a = integer()), keys = list("a"))),
      "b"
    ))
    expect_error(relation(
      list(list(df = data.frame(b = integer(), a = integer()), keys = list("a"))),
      c("a", "b")
    ))
    expect_silent(relation(
      list(list(df = data.frame(b = integer(), a = integer()), keys = list("b"))),
      c("a", "b")
    ))
  })
  it("expects valid input: keys are in columns", {
    expect_error(relation(
      list(list(df = data.frame(a = rep(1L, 2L)), keys = list("b"))),
      "a"
    ))
    expect_error(relation(
      list(list(
        df = data.frame(a = rep(1L, 2L), b = 1L, c = 1L),
        keys = list("b", c("c", "d"))
      )),
      c("a", "b", "c", "d")
    ))
  })
  it("expects valid input: keys are satisfied", {
    expect_error(relation(
      list(list(df = data.frame(a = rep(1L, 2L)), keys = list("a"))),
      "a"
    ))
  })
  it("is composed of its records(), keys(), names(), and attrs_order()", {
    forall(
      gen.relation(letters[1:6], 0, 8),
      \(r) expect_identical(
        r,
        relation(
          setNames(
            Map(
              \(recs, ks) list(df = recs, keys = ks),
              records(r),
              keys(r)
            ),
            names(r)
          ),
          attrs_order = attrs_order(r)
        )
      )
    )
  })
  it("has record attributes given by attrs()", {
    forall(
      gen.relation(letters[1:6], 0, 8),
      \(r) expect_identical(
        attrs(r),
        lapply(records(r), names)
      )
    )
  })
  it("expects record assignments to have same attributes and attribute order", {
    x <- relation(
      list(a = list(df = data.frame(a = 1:4, b = 1:2), keys = list("a"))),
      attrs_order = c("a", "b")
    )
    expect_error(
      records(x) <- list(a = data.frame(b = 1:2, a = 1:6)),
      "^record reassignments must have the same attributes, in the same order$"
    )
  })

  it("is subsetted to a valid relation schema", {
    forall(
      gen.relation(letters[1:6], 0, 8) |>
        gen.and_then(\(rs) list(
          gen.pure(rs),
          gen.sample_resampleable(c(FALSE, TRUE), of = length(rs))
        )),
      \(rs, i) {
        is_valid_relation(rs[i])
        is_valid_relation(rs[which(i)])
        is_valid_relation(rs[names(rs)[i]])
        expect_identical(rs[i], rs[which(i)])
        expect_identical(rs[i], rs[names(rs)[i]])
        expect_length(rs[i], sum(i))
      },
      curry = TRUE
    )
    forall(
      gen.relation(letters[1:6], 1, 8) |>
        gen.and_then(\(rel) list(
          gen.pure(rel),
          gen.element(seq_along(rel))
        )),
      \(rel, i) {
        is_valid_relation(rel[[i]])
        is_valid_relation(rel[[names(rel)[[i]]]])
        is_valid_relation(eval(rlang::expr(`$`(rel, !!names(rel)[[i]]))))
        expect_identical(rel[i], rel[[i]])
        expect_identical(rel[i], rel[[names(rel)[[i]]]])
        expect_identical(rel[i], eval(rlang::expr(`$`(rel, !!names(rel)[[i]]))))
      },
      curry = TRUE
    )
  })
  it("can be subsetted while preserving attributes", {
    x <- relation(
      list(a = list(
        df = data.frame(a = logical(), b = logical()),
        keys = list("a")
      )),
      letters[1:5]
    )
    expect_identical(x[TRUE], x)
    expect_identical(
      x[FALSE],
      relation(setNames(list(), character()), letters[1:5])
    )
    expect_identical(x[[1]], x)
    expect_error(x[[integer()]])
    expect_error(x[[c(1, 1)]])
  })

  it("prints", {
    expect_output(
      print(relation(setNames(list(), character()), character())),
      "\\A0 relations\\n0 attributes\\Z",
      perl = TRUE
    )
    expect_output(
      print(relation(
        list(a = list(df = data.frame(a = logical(), b = logical()), keys = list("a"))),
        c("a", "b")
      )),
      paste0(
        "\\A",
        "1 relation",
        "\\n",
        "2 attributes: a, b",
        "\\n",
        "relation a: a, b; 0 records\\n  key 1: a",
        "\\Z"
      ),
      perl = TRUE
    )
    expect_output(
      print(relation(
        list(a = list(df = data.frame(a = FALSE, b = TRUE), keys = list("a"))),
        c("a", "b")
      )),
      paste0(
        "\\A",
        "1 relation",
        "\\n",
        "2 attributes: a, b",
        "\\n",
        "relation a: a, b; 1 record\\n  key 1: a",
        "\\Z"
      ),
      perl = TRUE
    )
  })
})
