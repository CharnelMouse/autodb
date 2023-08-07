library(hedgehog)

describe("database_schema", {
  empty_rs <- relation_schema(setNames(list(), character()), character())
  it("expects valid input: relation_schema is a relation_schema", {
    expect_error(
      database_schema(1L, list()),
      "^relations must be a relation_schema$"
    )
  })
  it("expects valid input: relationships is a list", {
    expect_error(
      database_schema(empty_rs, logical()),
      "^relationships must be a list$"
    )
  })
  it("expects valid input: relationship elements have length two", {
    expect_error(
      database_schema(empty_rs, list("a")),
      "^relationship elements must have length two$"
    )
  })
  it("expects valid input: relationship relation names are a length-two character", {
    rs <- relation_schema(
      list(
        r1 = list(c("a", "c"), list("a")),
        r2 = list(c("b", "c"), list("c")),
        r3 = list("b", list("b"))
      ),
      c("a", "b", "c")
    )
    expect_error(
      database_schema(rs, list(list(1:2, c("c", "c")))),
      "^relationship elements must have length-two character first elements"
    )
    expect_error(
      database_schema(rs, list(list(paste0("r", 1:3), c("a", "a")))),
      "^relationship elements must have length-two character first elements"
    )
  })
  it("expects valid input: relationship relation names are within relation names", {
    rs <- relation_schema(
      list(
        r1 = list(c("a", "c"), list("a")),
        r2 = list(c("b", "c"), list("c")),
        r3 = list("b", list("b"))
      ),
      c("a", "b", "c")
    )
    expect_error(
      database_schema(rs, list(list(paste0("r", 3:4), c("b", "b")))),
      "^relationship relation names must be within relation schema names$"
    )
    expect_error(
      database_schema(rs, list(list(paste0("r", 4:3), c("b", "b")))),
      "^relationship relation names must be within relation schema names$"
    )
  })
  it("expects valid input: relationship attributes are a length-two character", {
    rs <- relation_schema(
      list(
        r1 = list(c("a", "c"), list("a")),
        r2 = list(c("b", "c"), list("c")),
        r3 = list("b", list("b"))
      ),
      c("a", "b", "c")
    )
    expect_error(
      database_schema(rs, list(list(paste0("r", 1:2), 1L))),
      "^relationship attributes must be length-two characters$"
    )
    expect_error(
      database_schema(rs, list(list(paste0("r", 1:2), character()))),
      "^relationship attributes must be length-two characters$"
    )
    rs2 <- relation_schema(
      list(
        r1 = list(c("a", "b", "c", "d"), list(c("a", "b", "c"))),
        r2 = list(c("a", "b", "d", "e"), list(c("a", "b", "d")))
      ),
      c("a", "b", "c", "d", "e")
    )
    expect_error(
      database_schema(rs2, list(list(paste0("r", 1:2), c("a", "b", "d")))),
      "^relationship attributes must be length-two characters$"
    )
  })

  it("expects valid input: relationship attribute names are within referer's attributes and referee's keys", {
    expect_error(
      database_schema(
        relation_schema(
          list(
            a = list(c("a", "b", "c"), list("a")),
            X = list(c("a", "b"), list("a"))
          ),
          c("a", "b", "c")
        ),
        list(list(c("a", "X"), c("b", "b")))
      ),
      "^relationship attributes must be within referer's attributes and referee's keys$"
    )
    # need more examples here

    # should have something about collected FK being a key of the citee,
    # waiting on FK grouping first
  })

  it("returns the relation_schema, with the additional attributes unmodified", {
    forall(
      gen.relation_schema(letters[1:6], 0, 8) |>
        gen.and_then(\(rs) list(gen.pure(rs), gen.relationships(rs))),
      dup %>>%
        onLeft(with_args(`[[`, 1)) %>>%
        onRight(with_args(do.call, what = database_schema) %>>% subschemas) %>>%
        uncurry(expect_identical)
    )
  })
  it("prints", {
    expect_output(
      print(database_schema(
        relation_schema(setNames(list(), character()), character()),
        list()
      )),
      paste0(
        "\\A",
        "database schema with 0 relation schemas",
        "\\n",
        "0 attributes",
        "\\n",
        "no relationships",
        "\\Z"
      ),
      perl = TRUE
    )
    expect_output(
      print(database_schema(
        relation_schema(
          list(
            a = list(c("a", "b"), list("a")),
            b = list(c("b", "c"), list("b", "c"))
          ),
          c("a", "b", "c")
        ),
        list(list(c("a", "b"), c("b", "b")))
      )),
      paste0(
        "\\A",
        "database schema with 2 relation schemas",
        "\\n",
        "3 attributes: a, b, c",
        "\\n",
        "schema a: a, b\\n  key 1: a",
        "\\n",
        "schema b: b, c\\n  key 1: b\\n  key 2: c",
        "\\n",
        "relationships:\\na\\.b -> b\\.b",
        "\\Z"
      ),
      perl = TRUE
    )
  })

  it("is subsetted to a valid database schema", {
    forall(
      gen.database_schema(letters[1:6], 0, 8) |>
        gen.and_then(\(ds) list(
          gen.pure(ds),
          gen.sample(c(FALSE, TRUE), length(ds), replace = TRUE)
        )),
      \(ds, i) {
        is_valid_database_schema(ds[i])
        is_valid_database_schema(ds[which(i)])
        expect_identical(ds[i], ds[which(i)])
        expect_length(ds[i], sum(i))
      },
      curry = TRUE
    )
  })
  it("can be subsetted while preserving attributes order", {
    preserves_attributes_when_subsetting <- function(ds, indices, op) {
      expect_identical(attrs_order(op(ds, indices)), attrs_order(ds))
    }
    forall(
      gen.database_schema(letters[1:6], 0, 8) |>
        gen.and_then(\(ds) list(
          ds = gen.pure(ds),
          indices = gen.sample(seq_along(ds), gen.sample(0:length(ds)), replace = TRUE)
        )) |>
        gen.with(\(lst) c(lst, list(op = `[`))),
      preserves_attributes_when_subsetting,
      curry = TRUE
    )
    forall(
      gen.database_schema(letters[1:6], 1, 8) |>
        gen.and_then(\(ds) list(
          ds = gen.pure(ds),
          indices = gen.int(length(ds))
        )) |>
        gen.with(\(lst) c(lst, list(op = `[[`))),
      preserves_attributes_when_subsetting,
      curry = TRUE
    )
  })
  it("keeps relevant relationships when subsetted", {
    keeps_relevant_relationships <- function(ds, indices, op) {
      expect_identical(
        relationships(op(ds, indices)),
        # this is too close to replicating the code for my liking
        Filter(\(r) all(r[[1]] %in% indices), relationships(ds)) |>
          lapply(\(r) list(match(r[[1]], indices), r[[2]]))
      )
    }
    forall(
      gen.database_schema(letters[1:6], 0, 8) |>
        gen.and_then(\(ds) list(
          ds = gen.pure(ds),
          indices = gen.sample(seq_along(ds), gen.sample(0:length(ds)), replace = TRUE)
        )) |>
        gen.with(\(lst) c(lst, list(op = `[`))),
      keeps_relevant_relationships,
      curry = TRUE
    )
    forall(
      gen.database_schema(letters[1:6], 1, 8) |>
        gen.and_then(\(ds) list(
          ds = gen.pure(ds),
          indices = gen.int(length(ds))
        )) |>
        gen.with(\(lst) c(lst, list(op = `[[`))),
      keeps_relevant_relationships,
      curry = TRUE
    )
  })
  it("can be made unique within class", {
    forall(
      gen.database_schema(letters[1:6], 0, 8),
      expect_biidentical(class, unique %>>% class)
    )
  })
  it("is made unique to a valid database schema", {
    forall(
      gen.database_schema(letters[1:6], 0, 8),
      unique %>>% with_args(is_valid_database_schema, unique = TRUE)
    )
  })
  it("is made unique with relationships preserved", {
    forall(
      gen.database_schema(letters[1:3], 0, 8) |> gen.with(unique),
      expect_biidentical(
        dup %>>% uncurry(c) %>>% unique %>>% relationships,
        relationships
      )
    )
  })

  it("concatenates to a valid database schema", {
    forall(
      gen.database_schema(letters[1:6], 0, 8) |> gen.list(from = 1, to = 3),
      c %>>% is_valid_database_schema,
      curry = TRUE
    )
  })
  it("concatenates without losing an attribute order", {
    concatenate_lossless_for_attrs_order <- function(lst) {
      res <- do.call(c, lst)
      for (l in lst) {
        expect_true(all(is.element(attrs_order(l), attrs_order(res))))
      }
    }
    forall(
      gen.database_schema(letters[1:6], 0, 8) |> gen.list(from = 1, to = 10),
      concatenate_lossless_for_attrs_order
    )
  })
  it("concatenates without losing attribute orderings, if consistent", {
    empty_schema_from_attrs <- with_args(
      relation_schema,
      schemas = setNames(list(), character())
    ) %>>%
      with_args(database_schema, relationships = list())
    concatenate_keeps_attribute_order <- function(attrs_lst) {
      lst <- lapply(attrs_lst, empty_schema_from_attrs)
      expect_silent(res <- do.call(c, lst))
      for (index in seq_along(lst)) {
        expect_identical(
          attrs_order(lst[[!!index]]),
          intersect(attrs_order(res), attrs_order(lst[[!!index]]))
        )
      }
    }

    forall(
      gen.sample(letters[1:8], gen.int(3)) |>
        gen.with(sort) |>
        gen.list(from = 2, to = 5),
      concatenate_keeps_attribute_order
    )

    # example where attributes aren't consistent, but are pairwise
    expect_failure(concatenate_keeps_attribute_order(
      list(c("a", "b"), c("b", "c"), c("c", "a"))
    ))

    forall(
      gen.subsequence(letters[1:6]) |>
        gen.list(from = 2, to = 10),
      concatenate_keeps_attribute_order
    )
  })
  it("concatenates without losing relationships", {
    concatenate_lossless_for_relationships <- function(lst) {
      res <- do.call(c, lst)
      for (l in lst) {
        equiv_relations <- lapply(
          l,
          \(r) {
            as <- sort(attrs(r)[[1]])
            ks <- lapply(keys(r)[[1]], sort)
            schema_matches <- which(vapply(
              res,
              \(r2) {
                as2 <- sort(attrs(r2)[[1]])
                ks2 <- lapply(keys(r2)[[1]], sort)
                identical(ks, ks2) &&
                  (
                    (identical(lengths(ks), 0L) && all(as %in% as2)) ||
                    identical(as, as2)
                  )
              },
              logical(1)
            ))
            schema_matches
          }
        )
        possible_equiv_relationship_present <- vapply(
          relationships(l),
          \(rl) {
            index_replacements <- lapply(
              rl[[1]],
              \(n) equiv_relations[[n]]
            )
            rl_replacements <- apply(
              do.call(expand.grid, index_replacements),
              1,
              \(x) list(names(res)[c(x[[1]], x[[2]])], rl[[2]])
            )
            any(is.element(rl_replacements, relationships(res)))
          },
          logical(1)
        )
        expect_true(all(possible_equiv_relationship_present))
      }
    }
    forall(
      gen.database_schema(letters[1:6], 0, 8) |> gen.list(from = 1, to = 10),
      concatenate_lossless_for_relationships
    )
  })
  it("concatenates without losing schemas", {
    concatenate_lossless_for_schemas <- function(lst) {
      res <- do.call(c, lst)
      # sort attrs to keep test independent from that for attribute orderings
      sorted_joined <- Map(
        \(as, ks) list(sort(as), lapply(ks, sort)),
        attrs(res),
        keys(res)
      )
      for (l in lst) {
        sorted <- Map(
          \(as, ks) list(sort(as), lapply(ks, sort)),
          attrs(l),
          keys(l)
        )
        expect_true(all(
          vapply(
            sorted,
            \(s) {
              any(vapply(
                sorted_joined,
                \(sj) {
                  all(is.element(s[[1]], sj[[1]])) &&
                    identical(s[[2]], sj[[2]])
                },
                logical(1)
              ))
            },
            logical(1)
          )
        ))
      }
    }
    forall(
      gen.database_schema(letters[1:6], 0, 8) |> gen.list(from = 1, to = 10),
      concatenate_lossless_for_schemas
    )
  })
  it("is composed of its attrs(), keys(), names(), attrs_order(), and relationships()", {
    forall(
      gen.database_schema(letters[1:6], 0, 8),
      \(ds) expect_identical(
        database_schema(
          relation_schema(
            setNames(Map(list, attrs(ds), keys(ds)), names(ds)),
            attrs_order(ds)
          ),
          relationships = relationships(ds)
        ),
        ds
      )
    )
  })
  it("is composed of its subschemas() and relationships()", {
    forall(
      gen.database_schema(letters[1:6], 0, 8),
      \(ds) expect_identical(
        database_schema(subschemas(ds), relationships(ds)),
        ds
      )
    )
  })
})
