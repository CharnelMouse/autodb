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