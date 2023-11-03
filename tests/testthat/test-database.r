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
  it("expects valid input: relations satisfy database schema (incl. relationships)", {
    expect_error(
      database(
        relation(
          list(
            a = list(df = data.frame(a = 1:3, b = 3:1), keys = list("a")),
            b = list(df = data.frame(b = 1:2, c = 3:4), keys = list("b"))
          ),
          letters[1:3]
        ),
        list(list("a", "b", "b", "b"))
      ),
      "^relations must satisfy relationships$"
    )
    expect_error(
      database(
        relation(
          list(
            a = list(df = data.frame(a = 1:3, b = 3:1), keys = list("a")),
            c = list(df = data.frame(c = 1:2, d = 3:4), keys = list("c"))
          ),
          letters[1:4]
        ),
        list(list("a", "b", "c", "c"))
      ),
      "^relations must satisfy relationships$"
    )
    # accounts for duplicate references before checking
    expect_silent(
      database(
        relation(
          list(
            a = list(df = data.frame(a = 1:3, b = c(1L, 1L, 2L)), keys = list("a")),
            b = list(df = data.frame(b = 1:2), keys = list("b"))
          ),
          letters[1:3]
        ),
        list(list("a", "b", "b", "b"))
      )
    )
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
        "relationships:\\na\\.\\{b\\} -> b\\.\\{b\\}",
        "\\Z"
      ),
      perl = TRUE
    )
    expect_output(
      print(database(
        relation(
          list(
            a = list(df = data.frame(a = logical(), b = logical(), c = logical()), keys = list("a")),
            b = list(df = data.frame(b = logical(), c = logical()), keys = list(c("b", "c")))
          ),
          c("a", "b", "c")
        ),
        list(list("a", c("b", "c"), "b", c("b", "c"))),
        "nm"
      )),
      paste0(
        "\\A",
        "database nm with 2 relations",
        "\\n",
        "relation a: a, b, c; 0 records\\n  key 1: a",
        "\\n",
        "relation b: b, c; 0 records\\n  key 1: b, c",
        "\\n",
        "relationships:\\na\\.\\{b, c\\} -> b\\.\\{b, c\\}",
        "\\Z"
      ),
      perl = TRUE
    )
  })
})
