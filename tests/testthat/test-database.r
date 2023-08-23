library(hedgehog)

describe("database", {
  empty_rs <- relation_schema(setNames(list(), character()), character())
  it("expects valid input: relations is a relation", {
    expect_error(
      database(1L, list()),
      "^relations must be a relation$"
    )
    expect_error(
      database(list(), list()),
      "^relations must be a relation$"
    )
  })
  it("expects valid input: relationships is a list", {
    expect_error(
      database(relation(list(), character()), 1L),
      "^relationships must be a list$"
    )
  })
  it("expects valid input: name is a scalar character", {
    expect_error(
      database(relation(list(), character()), list(), 1L),
      "^name must be a scalar character$"
    )
    expect_error(
      database(relation(list(), character()), list(), c("a", "b")),
      "^name must be a scalar character$"
    )
  })
  it("expects valid input: relations satisfy database schema", {
    succeed()
  })
  it("prints", {
    expect_output(
      print(database(
        relation(list(), character()),
        list()
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
        relation(
          list(
            a = list(df = data.frame(a = logical(), b = logical()), keys = list("a")),
            b = list(df = data.frame(b = logical(), c = logical()), keys = list("b", "c"))
          ),
          c("a", "b", "c")
        ),
        list(c("a", "b", "b", "b")),
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
