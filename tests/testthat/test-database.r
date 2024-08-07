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
  it("expects valid input: references is a list", {
    expect_error(
      database(relation(setNames(list(), character()), character()), 1L),
      "^references must be a list$"
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
  it("expects valid input: reference elements are length-four lists", {
    expect_error(
      database(create(empty_rs), list("a")),
      "^reference elements must be length-four lists$"
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
      "^reference elements must be length-four lists$"
    )
    expect_error(
      database(rs, list(as.list(paste0("r", 1:3)))),
      "^reference elements must be length-four lists$"
    )
  })
  it("expects valid input: unique relation names", {
    expect_error(
      database(
        relation(
          list(
            a = list(df = data.frame(), keys = list(character())),
            a = list(df = data.frame(), keys = list(character()))
          ),
          character()
        ),
        list()
      ),
      "^relation names must be unique$"
    )
  })
  it("expects valid input: non-empty relation names", {
    expect_error(
      database(
        relation(
          setNames(
            list(
              a = list(character(), list(character())),
              b = list(character(), list(character()))
            ),
            c("", "b")
          ),
          character()
        ),
        list()
      ),
      "^relation names must be non-empty"
    )
  })
  it("expects valid input: reference relation names are within relation names", {
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
      "^reference relation names must be within relation names$"
    )
    expect_error(
      database(rs, list(list("r4", "b", "r3", "b"))),
      "^reference relation names must be within relation names$"
    )
  })
  it("expects valid input: reference attribute names are within referrer's attributes and referee's keys", {
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
      "^reference attributes must be within referrer's attributes and referee's keys$"
    )
    # need more examples here

    # should have something about collected FK being a key of the citee,
    # waiting on FK grouping first
  })
  it("expects valid input: references aren't self-references", {
    expect_error(
      database(
        create(relation_schema(list(a = list(c("a", "b"), list("a"))), c("a", "b"))),
        list(list("a", "b", "a", "a"))
      ),
      "^reference cannot be from a relation's attribute to itself$"
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
      "^relations must satisfy references in schema:\na\\.\\{b\\} -> b\\.\\{b\\}$"
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
      "^relations must satisfy references in schema:\na\\.\\{b\\} -> c\\.\\{c\\}$"
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

  it("is subsetted to a valid database schema, obeys usual subsetting rules", {
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

        inum <- which(i)
        is_valid_database(db[inum], same_attr_name = san, single_key_pairs = skp)
        expect_identical(db[i], db[inum])

        ineg <- -setdiff(seq_along(db), inum)
        if (!all(i)) {
          is_valid_database(db[ineg], same_attr_name = san, single_key_pairs = skp)
          expect_identical(db[i], db[ineg])
        }

        is_valid_database(db[names(db)[i]], same_attr_name = san, single_key_pairs = skp)
        expect_identical(db[i], db[names(db)[i]])

        expect_length(db[i], sum(i))

        ints <- stats::setNames(seq_along(db), names(db))
        expect_identical(db[i], db[ints[i]])
        expect_identical(db[ineg], db[ints[ineg]])
        expect_identical(db[names(db)[i]], db[names(db)[ints[i]]])
      },
      curry = TRUE
    )
    forall(
      gen.database(letters[1:6], 1, 8) |>
        gen.and_then(\(db) list(
          gen.pure(db),
          gen.element(seq_along(db))
        )),
      \(db, inum) {
        is_valid_database(db[[inum]])
        expect_identical(db[inum], db[[inum]])

        ineg <- -setdiff(seq_along(db), inum)
        if (length(ineg) == 1) {
          is_valid_database(db[[ineg]])
          expect_identical(db[inum], db[[ineg]])
        }

        is_valid_database(db[[names(db)[[inum]]]])
        expect_identical(db[inum], db[[names(db)[[inum]]]])

        is_valid_database(eval(rlang::expr(`$`(db, !!names(db)[[inum]]))))
        expect_identical(db[inum], eval(rlang::expr(`$`(db, !!names(db)[[inum]]))))

        ints <- stats::setNames(seq_along(db), names(db))
        expect_identical(db[[inum]], db[[ints[[inum]]]])
        expect_identical(
          tryCatch(db[[ineg]], error = function(e) e$message),
          tryCatch(db[[ints[[ineg]]]], error = function(e) e$message)
        )
        expect_identical(db[[names(db)[[inum]]]], db[[names(db)[[ints[[inum]]]]]])
      },
      curry = TRUE
    )
    forall(
      gen.database(letters[1:6], 1, 8),
      \(db) {
        expect_identical(db[[TRUE]], db[[1]])
      }
    )
    forall(
      gen.database(letters[1:6], 1, 8) |>
        gen.and_then(\(db) list(
          db = gen.pure(db),
          indices = gen.sample_resampleable(
            seq_along(db),
            from = 2,
            to = 2*length(db)
          )
        )),
      \(db, indices) {
        is_valid_database(db[indices])
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
  it("keeps relevant references when subsetted", {
    keeps_relevant_references <- function(db, indices, op) {
      expect_identical(
        references(op(db, indices)),
        # this is too close to replicating the code for my liking
        Filter(
          \(r) all(c(r[[1]], r[[3]]) %in% names(db)[indices]),
          references(db)
        )
      )
    }
    forall(
      gen.database(letters[1:6], 0, 8, same_attr_name = FALSE) |>
        gen.and_then(\(db) list(
          db = gen.pure(db),
          indices = gen.sample(seq_along(db), replace = FALSE)
        )) |>
        gen.with(\(lst) c(lst, list(op = `[`))),
      keeps_relevant_references,
      curry = TRUE
    )
    forall(
      gen.database(letters[1:6], 1, 8, same_attr_name = FALSE) |>
        gen.and_then(\(db) list(
          db = gen.pure(db),
          indices = gen.int(length(db))
        )) |>
        gen.with(\(lst) c(lst, list(op = `[[`))),
      keeps_relevant_references,
      curry = TRUE
    )
  })
  it("duplicates references when taking duplicate relation schemas", {
    forall(
      gen.database(letters[1:6], 1, 8, same_attr_name = FALSE) |>
        gen.and_then(\(db) list(
          db = gen.pure(db),
          indices = gen.sample_resampleable(seq_along(db), from = 2, to = 2*length(db))
        )),
      \(db, indices) {
        if (!anyDuplicated(indices) || length(references(db)) == 0)
          discard()
        orig <- references(db)
        db_new <- db[indices]
        expected <- subset_refs(orig, indices, names(db), names(db_new))
        expect_setequal(references(db_new), expected)
      },
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
      \(san, db) {
        unique(db) |> is_valid_database(unique = TRUE, same_attr_name = san)
      },
      curry = TRUE
    )
  })
  it("is made unique with references preserved", {
    forall(
      gen.database(letters[1:3], 0, 8, same_attr_name = FALSE) |>
        gen.with(unique),
      expect_biidentical(
        dup %>>% uncurry(c) %>>% unique %>>% references,
        references
      )
    )

    forall(
      gen.database(letters[1:3], 0, 8, same_attr_name = FALSE) |>
        gen.with(unique),
      expect_biidentical(
        dup %>>%
          onLeft(\(db) {
            len <- length(references(db))
            references(db) <- references(db)[seq_len(floor(len))]
            db
          }) %>>%
          onRight(\(db) {
            len <- length(references(db))
            references(db) <- references(db)[setdiff(
              seq_len(len),
              seq_len(floor(len))
            )]
            db
          }) %>>%
          uncurry(c) %>>% unique %>>% references,
        references
      )
    )

    # special case: unique must merge two tables to keep both references
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
      references(unique(db)),
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
      with_args(database, references = list())
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
  it("concatenates without losing references", {
    concatenate_lossless_for_references <- function(lst) {
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
        possible_equiv_reference_present <- vapply(
          references(l),
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
            any(is.element(rl_replacements, references(res)))
          },
          logical(1)
        )
        expect_true(all(possible_equiv_reference_present))
      }
    }
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.and_then(\(san) {
          gen.database(letters[1:6], 0, 8, same_attr_name = san) |>
            gen.list(from = 1, to = 10)
        }),
      concatenate_lossless_for_references
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

  it("is composed of its records(), keys(), names(), attrs_order(), and references()", {
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
          references = references(db)
        ),
        db
      )
    )
  })
  it("is composed of its subrelations() and references()", {
    forall(
      gen.database(letters[1:6], 0, 8, same_attr_name = FALSE),
      \(db) expect_identical(
        database(subrelations(db), references(db)),
        db
      )
    )
  })

  it("renames relations in its references when they're renamed", {
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.list(of = 3) |>
        gen.and_then(uncurry(\(sek, san, skp) {
          gen.database(letters[1:6], 0, 8, sek, san, skp)
        })),
      \(db) {
        nms <- names(db)
        new_nms <- letters[seq_along(db)]
        new_db <- db
        names(new_db) <- new_nms
        ref_nms <- vapply(
          references(db),
          \(ref) c(ref[[1]], ref[[3]]),
          character(2)
        )
        new_ref_nms <- vapply(
          references(new_db),
          \(ref) c(ref[[1]], ref[[3]]),
          character(2)
        )
        expected_new_ref_nms <- ref_nms
        expected_new_ref_nms[] <- new_nms[match(ref_nms, nms)]
        expect_identical(new_ref_nms, expected_new_ref_nms)
      }
    )
  })

  it("can have its attributes renamed", {
    forall(
      gen.database(letters[1:6], 1, 8),
      function(db) {
        names <- toupper(attrs_order(db))
        db2 <- rename_attrs(db, names)
        expect_identical(
          db2,
          database(
            rename_attrs(subrelations(db), names),
            lapply(
              references(db),
              \(ref) list(
                ref[[1]],
                toupper(ref[[2]]),
                ref[[3]],
                toupper(ref[[4]])
              )
            )
          )
        )
      }
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
        "no references",
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
        "references:\\na\\.\\{b\\} -> b\\.\\{b\\}",
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
        "references:\\na\\.\\{b, c\\} -> b\\.\\{b, c\\}",
        "\\Z"
      ),
      perl = TRUE
    )
  })
})
