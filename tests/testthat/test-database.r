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
      "^relations must satisfy relationships in schema:\na\\.\\{b\\} -> b\\.\\{b\\}$"
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
      "^relations must satisfy relationships in schema:\na\\.\\{b\\} -> c\\.\\{c\\}$"
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

  it("is subsetted to a valid database schema", {
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.list(of = 2) |>
        gen.and_then(uncurry(\(san, skp) {
          list(
            gen.pure(san),
            gen.pure(skp),
            gen.database(
              letters[1:6],
              0,
              8,
              same_attr_name = san,
              single_key_pairs = skp
            )
          )
        })) |>
        gen.and_then(\(lst) list(
          gen.pure(lst[[1]]),
          gen.pure(lst[[2]]),
          gen.pure(lst[[3]]),
          gen.sample_resampleable(c(FALSE, TRUE), of = length(lst[[3]]))
        )),
      \(san, skp, db, i) {
        is_valid_database(db[i], same_attr_name = san, single_key_pairs = skp)
        is_valid_database(db[which(i)], same_attr_name = san, single_key_pairs = skp)
        is_valid_database(db[names(db)[i]], same_attr_name = san, single_key_pairs = skp)
        expect_identical(db[i], db[which(i)])
        expect_identical(db[i], db[names(db)[i]])
        expect_length(db[i], sum(i))
      },
      curry = TRUE
    )
  })
  it("can be subsetted while preserving attributes order", {
    preserves_attributes_when_subsetting <- function(db, indices, op) {
      expect_identical(attrs_order(op(db, indices)), attrs_order(db))
    }
    forall(
      gen.database(letters[1:6], 0, 8, same_attr_name = FALSE) |>
        gen.and_then(\(db) list(
          db = gen.pure(db),
          indices = gen.sample_resampleable(seq_along(db), from = 0, to = length(db))
        )) |>
        gen.with(\(lst) c(lst, list(op = `[`))),
      preserves_attributes_when_subsetting,
      curry = TRUE
    )
    forall(
      gen.database_schema(letters[1:6], 1, 8, same_attr_name = FALSE) |>
        gen.and_then(\(db) list(
          db = gen.pure(db),
          indices = gen.int(length(db))
        )) |>
        gen.with(\(lst) c(lst, list(op = `[[`))),
      preserves_attributes_when_subsetting,
      curry = TRUE
    )
  })
  it("keeps relevant relationships when subsetted", {
    keeps_relevant_relationships <- function(db, indices, op) {
      expect_identical(
        relationships(op(db, indices)),
        # this is too close to replicating the code for my liking
        Filter(
          \(r) all(c(r[[1]], r[[3]]) %in% names(db)[indices]),
          relationships(db)
        )
      )
    }
    forall(
      gen.database(letters[1:6], 0, 8, same_attr_name = FALSE) |>
        gen.and_then(\(db) list(
          db = gen.pure(db),
          indices = gen.sample_resampleable(seq_along(db), from = 0, to = length(db))
        )) |>
        gen.with(\(lst) c(lst, list(op = `[`))),
      keeps_relevant_relationships,
      curry = TRUE
    )
    forall(
      gen.database(letters[1:6], 1, 8, same_attr_name = FALSE) |>
        gen.and_then(\(db) list(
          db = gen.pure(db),
          indices = gen.int(length(db))
        )) |>
        gen.with(\(lst) c(lst, list(op = `[[`))),
      keeps_relevant_relationships,
      curry = TRUE
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
