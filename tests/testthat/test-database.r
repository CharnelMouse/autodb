library(hedgehog)

describe("database", {
  empty_rs <- relation_schema(setNames(list(), character()), character())
  it("expects valid input: relations is a list", {
    expect_error(
      database(1L, list(), character()),
      "^relations must be a list$"
    )
  })
  it("expects valid input: relationships is a list", {
    expect_error(
      database(list(), 1L, character()),
      "^relationships must be a list$"
    )
  })
  it("expects valid input: attrs_order is a character", {
    expect_error(
      database(list(), list(), 1L),
      "^attrs_order must be a character$"
    )
  })
  it("expects valid input: name is a scalar character", {
    expect_error(
      database(list(), list(), character(), 1L),
      "^name must be a scalar character$"
    )
    expect_error(
      database(list(), list(), character(), c("a", "b")),
      "^name must be a scalar character$"
    )
  })
  it("expects valid input: relations satisfy database schema", {
    succeed()
  })
  it("prints", {
    expect_output(
      print(database(
        list(),
        list(),
        character()
      )),
      paste0(
        "\\A",
        "database NA with 0 relations",
        "\\n",
        "no relationships",
        "\\Z"
      ),
      perl = TRUE
    )
    expect_output(
      print(database(
        list(
          a = list(df = data.frame(a = logical(), b = logical()), keys = list("a")),
          b = list(df = data.frame(b = logical(), c = logical()), keys = list("b", "c"))
        ),
        list(c("a", "b", "b", "b")),
        c("a", "b", "c"),
        "nm"
      )),
      paste0(
        "\\A",
        "database nm with 2 relations",
        "\\n",
        "relation a: a, b; 0 records\\n  key 1: a",
        "\\n",
        "relation b: b, c; 0 records\\n  key 1: b\\n  key 2: c",
        "\\n",
        "relationships:\\na\\.b -> b\\.b",
        "\\Z"
      ),
      perl = TRUE
    )
  })
})
