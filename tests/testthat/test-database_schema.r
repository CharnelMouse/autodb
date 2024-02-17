library(hedgehog)

describe("database_schema", {
  empty_rs <- relation_schema(setNames(list(), character()), character())
  it("expects valid input: relation_schemas is a relation_schema", {
    expect_error(
      database_schema(1L, list()),
      "^relations must be a relation_schema$"
    )
  })
  it("expects valid input: references is a list", {
    expect_error(
      database_schema(empty_rs, logical()),
      "^references must be a list$"
    )
  })
  it("expects valid input: reference elements are length-four lists", {
    expect_error(
      database_schema(empty_rs, list("a")),
      "^reference elements must be length-four lists$"
    )
    rs <- relation_schema(
      list(
        r1 = list(c("a", "c"), list("a")),
        r2 = list(c("b", "c"), list("c")),
        r3 = list("b", list("b"))
      ),
      c("a", "b", "c")
    )
    expect_error(
      database_schema(rs, list(1:4)),
      "^reference elements must be length-four lists$"
    )
    expect_error(
      database_schema(rs, list(as.list(paste0("r", 1:3)))),
      "^reference elements must be length-four lists$"
    )
  })
  it("expects valid input: reference relation names are within relation names", {
    rs <- relation_schema(
      list(
        r1 = list(c("a", "c"), list("a")),
        r2 = list(c("b", "c"), list("c")),
        r3 = list("b", list("b"))
      ),
      c("a", "b", "c")
    )
    expect_error(
      database_schema(rs, list(list("r3", "b", "r4", "b"))),
      "^reference relation names must be within relation schema names$"
    )
    expect_error(
      database_schema(rs, list(list("r4", "b", "r3", "b"))),
      "^reference relation names must be within relation schema names$"
    )
  })
  it("expects valid input: reference attribute names are within referer's attributes and referee's keys", {
    expect_error(
      database_schema(
        relation_schema(
          list(
            a = list(c("a", "b", "c"), list("a")),
            X = list(c("a", "b"), list("a"))
          ),
          c("a", "b", "c")
        ),
        list(list("a", "b", "X", "b"))
      ),
      "^reference attributes must be within referer's attributes and referee's keys$"
    )
    # need more examples here

    # should have something about collected FK being a key of the citee,
    # waiting on FK grouping first
  })
  it("expects valid input: references aren't self-references", {
    expect_error(
      database_schema(
        relation_schema(list(a = list(c("a", "b"), list("a"))), c("a", "b")),
        list(list("a", "b", "a", "a"))
      ),
      "^reference cannot be from a relation's attribute to itself$"
    )
  })

  it("returns the relation_schema, with the additional attributes unmodified", {
    forall(
      list(
        gen.relation_schema(letters[1:6], 0, 8),
        gen.element(c(FALSE, TRUE))
      ) |>
        gen.and_then(uncurry(\(rs, skp) {
          list(gen.pure(rs), gen.references(rs, skp))
        })),
      expect_biidentical(
        with_args(`[[`, 1),
        with_args(do.call, what = database_schema) %>>% subschemas
      )
    )
  })

  it("is subsetted to a valid database schema, obeys usual subsetting rules", {
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.and_then(\(san) {
          list(
            gen.pure(san),
            gen.database_schema(letters[1:6], 0, 8, same_attr_name = san)
          )
        }) |>
        gen.and_then(\(lst) list(
          gen.pure(lst[[1]]),
          gen.pure(lst[[2]]),
          gen.sample_resampleable(c(FALSE, TRUE), of = length(lst[[2]]))
        )),
      \(san, ds, i) {
        is_valid_database_schema(ds[i], same_attr_name = san)

        inum <- which(i)
        is_valid_database_schema(ds[inum], same_attr_name = san)
        expect_identical(ds[i], ds[inum])

        ineg <- -setdiff(seq_along(ds), inum)
        if (!all(i)) {
          is_valid_database_schema(ds[ineg], same_attr_name = san)
          expect_identical(ds[i], ds[ineg])
        }

        is_valid_database_schema(ds[names(ds)[i]], same_attr_name = san)
        expect_identical(ds[i], ds[names(ds)[i]])

        expect_length(ds[i], sum(i))

        ints <- stats::setNames(seq_along(ds), names(ds))
        expect_identical(ds[i], ds[ints[i]])
        expect_identical(ds[ineg], ds[ints[ineg]])
        expect_identical(ds[names(ds)[i]], ds[names(ds)[ints[i]]])
      },
      curry = TRUE
    )
    forall(
      gen.database_schema(letters[1:6], 1, 8) |>
        gen.and_then(\(ds) list(
          gen.pure(ds),
          gen.element(seq_along(ds))
        )),
      \(ds, inum) {
        is_valid_database_schema(ds[[inum]])
        expect_identical(ds[inum], ds[[inum]])

        ineg <- -setdiff(seq_along(ds), inum)
        if (length(ineg) == 1) {
          is_valid_database_schema(ds[[ineg]])
          expect_identical(ds[inum], ds[[ineg]])
        }

        is_valid_database_schema(ds[[names(ds)[[inum]]]])
        expect_identical(ds[inum], ds[[names(ds)[[inum]]]])

        is_valid_database_schema(eval(rlang::expr(`$`(ds, !!names(ds)[[inum]]))))
        expect_identical(ds[inum], eval(rlang::expr(`$`(ds, !!names(ds)[[inum]]))))

        ints <- stats::setNames(seq_along(ds), names(ds))
        expect_identical(ds[[inum]], ds[[ints[[inum]]]])
        expect_identical(
          tryCatch(ds[[ineg]], error = function(e) e$message),
          tryCatch(ds[[ints[[ineg]]]], error = function(e) e$message)
        )
        expect_identical(ds[[names(ds)[[inum]]]], ds[[names(ds)[[ints[[inum]]]]]])
      },
      curry = TRUE
    )
    forall(
      gen.database_schema(letters[1:6], 1, 8),
      \(ds) {
        expect_identical(ds[[TRUE]], ds[[1]])
      }
    )
    forall(
      gen.database_schema(letters[1:6], 1, 8) |>
        gen.and_then(\(ds) list(
          ds = gen.pure(ds),
          indices = gen.sample_resampleable(
            seq_along(ds),
            from = 2,
            to = 2*length(ds)
          )
        )),
      \(ds, indices) {
        is_valid_database_schema(ds[indices])
      },
      curry = TRUE
    )
  })
  it("can be subsetted while preserving attributes order", {
    preserves_attributes_when_subsetting <- function(ds, indices, op) {
      expect_identical(attrs_order(op(ds, indices)), attrs_order(ds))
    }
    forall(
      gen.database_schema(letters[1:6], 0, 8, same_attr_name = FALSE) |>
        gen.and_then(\(ds) list(
          ds = gen.pure(ds),
          indices = gen.sample_resampleable(seq_along(ds), from = 0, to = length(ds))
        )) |>
        gen.with(\(lst) c(lst, list(op = `[`))),
      preserves_attributes_when_subsetting,
      curry = TRUE
    )
    forall(
      gen.database_schema(letters[1:6], 1, 8, same_attr_name = FALSE) |>
        gen.and_then(\(ds) list(
          ds = gen.pure(ds),
          indices = gen.int(length(ds))
        )) |>
        gen.with(\(lst) c(lst, list(op = `[[`))),
      preserves_attributes_when_subsetting,
      curry = TRUE
    )
  })
  it("keeps relevant references when subsetted", {
    keeps_relevant_references <- function(ds, indices, op) {
      expect_identical(
        references(op(ds, indices)),
        # this is too close to replicating the code for my liking
        Filter(
          \(r) all(c(r[[1]], r[[3]]) %in% names(ds)[indices]),
          references(ds)
        )
      )
    }
    forall(
      gen.database_schema(letters[1:6], 0, 8, same_attr_name = FALSE) |>
        gen.and_then(\(ds) list(
          ds = gen.pure(ds),
          indices = gen.sample_resampleable(seq_along(ds), from = 0, to = length(ds))
        )) |>
        gen.with(\(lst) c(lst, list(op = `[`))),
      keeps_relevant_references,
      curry = TRUE
    )
    forall(
      gen.database_schema(letters[1:6], 1, 8, same_attr_name = FALSE) |>
        gen.and_then(\(ds) list(
          ds = gen.pure(ds),
          indices = gen.int(length(ds))
        )) |>
        gen.with(\(lst) c(lst, list(op = `[[`))),
      keeps_relevant_references,
      curry = TRUE
    )
  })

  it("is made unique to a valid database schema", {
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.and_then(\(san) {
          list(
            gen.pure(san),
            gen.database_schema(letters[1:6], 0, 8, same_attr_name = san)
          )
        }),
      \(san, ds) {
        unique(ds) |> is_valid_database_schema(unique = TRUE, same_attr_name = san)
      },
      curry = TRUE
    )
  })
  it("is made unique with references preserved", {
    forall(
      gen.database_schema(letters[1:3], 0, 8, same_attr_name = FALSE) |>
        gen.with(unique),
      expect_biidentical(
        dup %>>% uncurry(c) %>>% unique %>>% references,
        references
      )
    )

    forall(
      gen.database_schema(letters[1:3], 0, 8, same_attr_name = FALSE) |>
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
    expect_identical(
      unique(ds),
      database_schema(
        relation_schema(
          list(
            a.1 = list(c("a", "b"), list("a")),
            b.1 = list(c("b", "c"), list("b")),
            b.2 = list(c("b", "d"), list("b"))
          ),
          letters[1:4]
        ),
        list(
          list("a.1", "b", "b.1", "b"),
          list("a.1", "b", "b.2", "b")
        )
      )
    )
  })

  it("concatenates to a valid database schema", {
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.and_then(\(san) list(
          gen.pure(san),
          gen.database_schema(letters[1:6], 0, 8, same_attr_name = san) |>
            gen.list(from = 1, to = 3)
        )),
      \(san, dss) do.call(c, dss) |> is_valid_database_schema(same_attr_name = san),
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
          gen.database_schema(letters[1:6], 0, 8, same_attr_name = san) |>
            gen.list(from = 1, to = 10)
        }),
      concatenate_lossless_for_attrs_order
    )
  })
  it("concatenates without losing attribute orderings, if consistent", {
    empty_schema_from_attrs <- with_args(
      relation_schema,
      schemas = setNames(list(), character())
    ) %>>%
      with_args(database_schema, references = list())
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
          gen.database_schema(letters[1:6], 0, 8, same_attr_name = san) |>
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
          gen.database_schema(
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

  it("can have empty-key schemas merged", {
    up_to_one_empty_key <- function(ds) {
      if (sum(vapply(keys(ds), identical, logical(1), list(character()))) <= 1)
        discard()
      res <- merge_empty_keys(ds)
      is_valid_database_schema(ds)
      expect_lte(
        sum(vapply(keys(res), identical, logical(1), list(character()))),
        1L
      )
    }
    forall(
      gen.database_schema_empty_keys(
        letters[1:6],
        1,
        8,
        single_key_pairs = FALSE,
        min_empty = 1
      ),
      up_to_one_empty_key
    )
  })
  it("is composed of its attrs(), keys(), names(), attrs_order(), and references()", {
    forall(
      gen.database_schema(letters[1:6], 0, 8, same_attr_name = FALSE),
      \(ds) expect_identical(
        database_schema(
          relation_schema(
            setNames(Map(list, attrs(ds), keys(ds)), names(ds)),
            attrs_order(ds)
          ),
          references = references(ds)
        ),
        ds
      )
    )
  })
  it("is composed of its subschemas() and references()", {
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.list(of = 2) |>
        gen.and_then(uncurry(\(san, skp) {
          gen.database_schema(
            letters[1:6],
            0,
            8,
            same_attr_name = san,
            single_key_pairs = skp
          )
        })),
      \(ds) expect_identical(
        database_schema(subschemas(ds), references(ds)),
        ds
      )
    )
  })

  it("renames relations in its references when they're renamed", {
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.list(of = 3) |>
        gen.and_then(uncurry(\(sek, san, skp) {
          gen.database_schema(letters[1:6], 0, 8, sek, san, skp)
        })),
      \(ds) {
        nms <- names(ds)
        new_nms <- letters[seq_along(ds)]
        new_ds <- ds
        names(new_ds) <- new_nms
        ref_nms <- vapply(
          references(ds),
          \(ref) c(ref[[1]], ref[[3]]),
          character(2)
        )
        new_ref_nms <- vapply(
          references(new_ds),
          \(ref) c(ref[[1]], ref[[3]]),
          character(2)
        )
        expected_new_ref_nms <- ref_nms
        expected_new_ref_nms[] <- new_nms[match(ref_nms, nms)]
        expect_identical(new_ref_nms, expected_new_ref_nms)
      }
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
        "no references",
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
        list(list("a", "b", "b", "b"))
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
        "references:\\na\\.\\{b\\} -> b\\.\\{b\\}",
        "\\Z"
      ),
      perl = TRUE
    )
    expect_output(
      print(database_schema(
        relation_schema(
          list(
            a = list(c("a", "b", "c"), list("a")),
            b = list(c("b", "c"), list(c("b", "c")))
          ),
          c("a", "b", "c")
        ),
        list(list("a", c("b", "c"), "b", c("b", "c")))
      )),
      paste0(
        "\\A",
        "database schema with 2 relation schemas",
        "\\n",
        "3 attributes: a, b, c",
        "\\n",
        "schema a: a, b, c\\n  key 1: a",
        "\\n",
        "schema b: b, c\\n  key 1: b, c",
        "\\n",
        "references:\\na\\.\\{b, c\\} -> b\\.\\{b, c\\}",
        "\\Z"
      ),
      perl = TRUE
    )
  })
})
