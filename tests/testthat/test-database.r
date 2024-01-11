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
      database(relation(setNames(list(), character()), character()), 1L),
      "^relationships must be a list$"
    )
  })
  it("expects valid input: name is a scalar character", {
    expect_error(
      database(relation(setNames(list(), character()), character()), list(), 1L),
      "^name must be a scalar character$"
    )
    expect_error(
      database(relation(setNames(list(), character()), character()), list(), c("a", "b")),
      "^name must be a scalar character$"
    )
  })
  it("expects valid input: relationship elements are length-four lists", {
    expect_error(
      database(create(empty_rs), list("a")),
      "^relationship elements must be length-four lists$"
    )
    rs <- create(relation_schema(
      list(
        r1 = list(c("a", "c"), list("a")),
        r2 = list(c("b", "c"), list("c")),
        r3 = list("b", list("b"))
      ),
      c("a", "b", "c")
    ))
    expect_error(
      database(rs, list(1:4)),
      "^relationship elements must be length-four lists$"
    )
    expect_error(
      database(rs, list(as.list(paste0("r", 1:3)))),
      "^relationship elements must be length-four lists$"
    )
  })
  it("expects valid input: relationship relation names are within relation names", {
    rs <- create(relation_schema(
      list(
        r1 = list(c("a", "c"), list("a")),
        r2 = list(c("b", "c"), list("c")),
        r3 = list("b", list("b"))
      ),
      c("a", "b", "c")
    ))
    expect_error(
      database(rs, list(list("r3", "b", "r4", "b"))),
      "^relationship relation names must be within relation names$"
    )
    expect_error(
      database(rs, list(list("r4", "b", "r3", "b"))),
      "^relationship relation names must be within relation names$"
    )
  })
  it("expects valid input: relationship attribute names are within referer's attributes and referee's keys", {
    expect_error(
      database(
        create(relation_schema(
          list(
            a = list(c("a", "b", "c"), list("a")),
            X = list(c("a", "b"), list("a"))
          ),
          c("a", "b", "c")
        )),
        list(list("a", "b", "X", "b"))
      ),
      "^relationship attributes must be within referer's attributes and referee's keys$"
    )
    # need more examples here

    # should have something about collected FK being a key of the citee,
    # waiting on FK grouping first
  })
  it("expects valid input: relationships aren't self-references", {
    expect_error(
      database(
        create(relation_schema(list(a = list(c("a", "b"), list("a"))), c("a", "b"))),
        list(list("a", "b", "a", "a"))
      ),
      "^relationship cannot be from a relation's attribute to itself$"
    )
  })
  it("expects valid input: records satisfy database schema", {
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
    forall(
      gen.database(letters[1:6], 1, 8) |>
        gen.and_then(\(db) list(
          gen.pure(db),
          gen.element(seq_along(db))
        )),
      \(db, i) {
        is_valid_relation(db[[i]])
        is_valid_relation(db[[names(db)[[i]]]])
        is_valid_relation(eval(rlang::expr(`$`(db, !!names(db)[[i]]))))
        expect_identical(db[i], db[[i]])
        expect_identical(db[i], db[[names(db)[[i]]]])
        expect_identical(db[i], eval(rlang::expr(`$`(db, !!names(db)[[i]]))))
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
      gen.database(letters[1:6], 1, 8, same_attr_name = FALSE) |>
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

  it("is made unique to a valid database", {
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.and_then(\(san) {
          list(
            gen.pure(san),
            gen.database(letters[1:6], 0, 8, same_attr_name = san)
          )
        }),
      \(san, ds) {
        unique(ds) |> is_valid_database(unique = TRUE, same_attr_name = san)
      },
      curry = TRUE
    )
  })
  it("is made unique with relationships preserved", {
    forall(
      gen.database(letters[1:3], 0, 8, same_attr_name = FALSE) |>
        gen.with(unique),
      expect_biidentical(
        dup %>>% uncurry(c) %>>% unique %>>% relationships,
        relationships
      )
    )

    forall(
      gen.database(letters[1:3], 0, 8, same_attr_name = FALSE) |>
        gen.with(unique),
      expect_biidentical(
        dup %>>%
          onLeft(\(db) {
            len <- length(relationships(db))
            relationships(db) <- relationships(db)[seq_len(floor(len))]
            db
          }) %>>%
          onRight(\(db) {
            len <- length(relationships(db))
            relationships(db) <- relationships(db)[setdiff(
              seq_len(len),
              seq_len(floor(len))
            )]
            db
          }) %>>%
          uncurry(c) %>>% unique %>>% relationships,
        relationships
      )
    )

    # special case: unique must merge two tables to keep both relationships
    ds <- database_schema(
      relation_schema(
        list(
          a.1 = list(c("a", "b"), list("a")),
          a.2 = list(c("a", "b"), list("a")),
          b.1 = list(c("b", "c"), list("b")),
          b.2 = list(c("b", "d"), list("b"))
        ),
        letters[1:4]
      ),
      list(
        list("a.1", "b", "b.1", "b"),
        list("a.2", "b", "b.2", "b")
      )
    )
    db <- create(ds)
    expect_identical(
      relationships(unique(db)),
      list(
        list("a.1", "b", "b.1", "b"),
        list("a.1", "b", "b.2", "b")
      )
    )
  })
  it("is made unique where tables with permuted rows count as duplicates", {
    db <- database(
      relation(
        list(
          a = list(df = data.frame(a = c(T, F)), keys = list("a")),
          a.1 = list(df = data.frame(a = c(F, T)), keys = list("a"))
        ),
        "a"
      ),
      list()
    )
    expect_length(unique(db), 1L)
  })

  it("concatenates to a valid database", {
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.and_then(\(san) list(
          gen.pure(san),
          gen.database(letters[1:6], 0, 8, same_attr_name = san) |>
            gen.list(from = 1, to = 3)
        )),
      \(san, dss) do.call(c, dss) |> is_valid_database(same_attr_name = san),
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
      gen.element(c(FALSE, TRUE)) |>
        gen.and_then(\(san) {
          gen.database(letters[1:6], 0, 8, same_attr_name = san) |>
            gen.list(from = 1, to = 10)
        }),
      concatenate_lossless_for_attrs_order
    )
  })
  it("concatenates without losing attribute orderings, if consistent", {
    empty_schema_from_attrs <- with_args(
      relation,
      relations = setNames(list(), character())
    ) %>>%
      with_args(database, relationships = list())
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
      gen.subsequence(letters[1:8]) |>
        gen.with(\(x) if (length(x) > 3) x[1:3] else x) |>
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
        equiv_relations <- setNames(
          Map(
            \(as, ks) {
              schema_matches <- which(mapply(
                \(as2, ks2) {
                  identical(ks, ks2) &&
                    (
                      (identical(lengths(ks), 0L) && all(as %in% as2)) ||
                        identical(as, as2)
                    )
                },
                attrs(res),
                keys(res)
              ))
              unname(schema_matches)
            },
            unname(attrs(l)),
            unname(keys(l))
          ),
          names(l)
        )
        possible_equiv_relationship_present <- vapply(
          relationships(l),
          \(rl) {
            index_replacements <- list(
              equiv_relations[[rl[[1]]]],
              equiv_relations[[rl[[3]]]]
            )
            rl_replacements <- apply(
              do.call(expand.grid, index_replacements),
              1,
              \(x) list(
                names(res)[[x[[1]]]],
                rl[[2]],
                names(res)[[x[[2]]]],
                rl[[4]]
              ),
              simplify = FALSE
            )
            any(is.element(rl_replacements, relationships(res)))
          },
          logical(1)
        )
        expect_true(all(possible_equiv_relationship_present))
      }
    }
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.and_then(\(san) {
          gen.database(letters[1:6], 0, 8, same_attr_name = san) |>
            gen.list(from = 1, to = 10)
        }),
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
      gen.element(c(FALSE, TRUE)) |>
        gen.and_then(\(san) {
          gen.database(
            letters[1:6],
            0,
            8,
            same_attr_name = san
          ) |>
            gen.list(from = 1, to = 10)
        }),
      concatenate_lossless_for_schemas
    )
  })

  it("is composed of its records(), keys(), names(), attrs_order(), and relationships()", {
    forall(
      gen.database(letters[1:6], 0, 8, same_attr_name = FALSE),
      \(db) expect_identical(
        database(
          relation(
            setNames(
              Map(
                list %>>% with_args(setNames, c("df", "keys")),
                records(db),
                keys(db)
              ),
              names(db)
            ),
            attrs_order(db)
          ),
          relationships = relationships(db)
        ),
        db
      )
    )
  })
  it("is composed of its subrelations() and relationships()", {
    forall(
      gen.database(letters[1:6], 0, 8, same_attr_name = FALSE),
      \(db) expect_identical(
        database(subrelations(db), relationships(db)),
        db
      )
    )
  })

  it("prints", {
    expect_output(
      print(database(
        relation(setNames(list(), character()), character()),
        list()
      )),
      paste0(
        "\\A",
        "database NA with 0 relations",
        "\\n",
        "0 attributes",
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
        list(list("a", "b", "b", "b")),
        "nm"
      )),
      paste0(
        "\\A",
        "database nm with 2 relations",
        "\\n",
        "3 attributes: a, b, c",
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
        "3 attributes: a, b, c",
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
