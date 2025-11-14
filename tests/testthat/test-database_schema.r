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
      "^reference elements must be length-four lists: element 1$"
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
      "^reference elements must be length-four lists: element 1$"
    )
    expect_error(
      database_schema(rs, list(as.list(paste0("r", 1:3)))),
      "^reference elements must be length-four lists: element 1$"
    )
  })
  it("expects valid input: unique schema names", {
    expect_error(
      database_schema(
        relation_schema(
          list(
            a = list(character(), list(character())),
            a = list(character(), list(character()))
          ),
          character()
        ),
        list()
      ),
      "^relation schema names must be unique: duplicated a$"
    )
  })
  it("expects valid input: non-empty schema names", {
    expect_error(
      database_schema(
        relation_schema(
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
      "^relation schema names must be non-empty"
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
      "^reference relation names must be within relation schema names: absent r4$"
    )
    expect_error(
      database_schema(rs, list(list("r4", "b", "r3", "b"))),
      "^reference relation names must be within relation schema names: absent r4$"
    )
  })
  it("expects valid input: reference attributes are within referrer's attributes, make one of referee's keys", {
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
      "^reference attributes must be within referrer's attributes and referee's keys: reference 1$"
    )
    # must exactly match a key, not just be contained in one
    expect_error(
      database_schema(
        relation_schema(
          list(
            a_b = list(c("a", "b"), list(c("a", "b"))),
            X  = list("a", list("a"))
          ),
          c("a", "b")
        ),
        list(list("X", "a", "a_b", "a"))
      ),
      "^reference attributes must be within referrer's attributes and referee's keys: reference 1$"
    )
  })
  it("can take referree keys out of order", {
    expect_no_error(
      database_schema(
        relation_schema(
          list(
            a = list(c("a", "b", "c"), list(c("a"))),
            b_c  = list(c("b", "c"), list(c("b", "c")))
          ),
          c("a", "b", "c")
        ),
        list(list("a", c("b", "c"), "b_c", c("b", "c")))
      )
    )
    expect_no_error(
      database_schema(
        relation_schema(
          list(
            a = list(c("a", "b", "c"), list(c("a"))),
            b_c  = list(c("b", "c"), list(c("b", "c")))
          ),
          c("a", "b", "c")
        ),
        list(list("a", c("c", "b"), "b_c", c("c", "b")))
      )
    )
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

  it("is subsetted to a valid database schema, obeys usual subsetting rules...", {
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
  it("... except allowing non-matches as NAs", {
    ds <- database_schema(
      relation_schema(
        list(a = list("a", list("a"))),
        c("a")
      ),
      list()
    )
    expect_error(
      ds[c("b", "c")],
      "^subset names that don't exist: b, c$"
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
          indices = gen.sample(seq_along(ds), replace = FALSE)
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
  it("duplicates references when taking duplicate relation schemas", {
    forall(
      gen.database_schema(letters[1:6], 1, 8, same_attr_name = FALSE) |>
        gen.and_then(\(ds) list(
          ds = gen.pure(ds),
          indices = gen.sample_resampleable(seq_along(ds), from = 2, to = 2*length(ds))
        )),
      \(ds, indices) {
        if (!anyDuplicated(indices) || length(references(ds)) == 0)
          discard()
        orig <- references(ds)
        ds_new <- ds[indices]
        expected <- subset_refs(orig, indices, names(ds), names(ds_new))
        expect_setequal(references(ds_new), expected)
      },
      curry = TRUE
    )
  })

  it("expects a database_schema value for subset re-assignment", {
    ds <- database_schema(
      relation_schema(
        list(X = list(character(), list(character()))),
        letters[1:6]
      ),
      list()
    )
    expect_error(ds[1] <- 1L, "^value must also be a database_schema object$")
    expect_error(ds[[1]] <- 1L, "^value must also be a database_schema object$")
    expect_error(ds$X <- 1L, "^value must also be a database_schema object$")
  })
  describe("can have subsets re-assigned, without changing relation names", {
    it("[<-", {
      gen.ds_reassignment_indices_format <- function(ds, subseq) {
        choices <- c(
          list(gen.pure(subseq)),
          if (length(subseq) < length(ds))
            list(gen.pure(-setdiff(seq_along(ds), subseq))),
          list(gen.pure(names(ds)[subseq])),
          list(seq_along(ds) %in% subseq)
        )
        weights <- rep(1L, 3L + (length(subseq) < length(ds)))
        do.call(gen.choice, c(choices, list(prob = weights)))
      }
      gen.ds_reassignment <- function(ds) {
        gen.subsequence(seq_along(ds)) |>
          gen.and_then(\(subseq) {
            gen.ds_reassignment_indices_format(ds, subseq) |>
              gen.and_then(\(inds) {
                gen.database_schema(letters[1:6], length(subseq), length(subseq)) |>
                  gen.with(\(rs2) {
                    list(ds, inds, rs2)
                  })
              })
          })
      }
      expect_ds_subset_reassignment_success <- function(ds, indices, value) {
        res <- ds
        res[indices] <- value
        is_valid_database_schema(res)
        switch(
          class(indices),
          character = {
            negind <- setdiff(names(res), indices)
            expect_identical(res[negind], ds[negind])
            expect_identical(res[indices], setNames(value, indices))
          },
          integer = {
            negind <- if (length(indices) == 0)
              seq_along(ds)
            else
              -indices
            expect_identical(res[negind], ds[negind])
            expect_identical(res[indices], setNames(value, names(ds)[indices]))
          },
          logical = {
            expect_identical(res[!indices], ds[!indices])
            expect_identical(res[indices], setNames(value, names(ds)[indices]))
          }
        )
      }
      forall(
        gen.database_schema(letters[1:6], 0, 8) |>
          gen.and_then(gen.ds_reassignment),
        expect_ds_subset_reassignment_success,
        curry = TRUE
      )
    })
    it("[[<-", {
      gen.ds_single_reassignment_indices_format <- function(ds, subseq) {
        choices <- c(
          list(gen.pure(subseq)),
          if (length(ds) == 2)
            list(gen.pure(-setdiff(seq_along(ds), subseq))),
          list(gen.pure(names(ds)[subseq])),
          if (length(ds) == 1)
            list(gen.pure(seq_along(ds) %in% subseq))
        )
        weights <- rep(
          1L,
          2L + (length(ds) == 2) + (length(ds) == 1)
        )
        do.call(gen.choice, c(choices, list(prob = weights)))
      }
      gen.ds_single_reassignment_success <- function(ds) {
        list(
          gen.pure(ds),
          gen.element(seq_along(ds)) |>
            gen.and_then(\(subseq) {
              gen.ds_single_reassignment_indices_format(ds, subseq)
            }),
          gen.database_schema(letters[1:6], 1, 1),
          gen.pure(NA_character_)
        )
      }
      gen.ds_single_reassignment_failure_emptyint <- function(ds) {
        list(
          gen.pure(ds),
          gen.ds_single_reassignment_indices_format(ds, integer()),
          gen.database_schema(letters[1:6], 0, 0)
        ) |>
          gen.with(\(lst) {
            c(
              lst,
              list(single_subset_failure_type(ds, lst[[2]]))
            )
          })
      }
      gen.ds_single_reassignment_failure_multiint <- function(ds) {
        list(
          gen.sample(seq_along(ds), 2, replace = FALSE),
          gen.subsequence(seq_along(ds))
        ) |>
          gen.with(unlist %>>% unique %>>% sort) |>
          gen.and_then(\(subseq) {
            gen.ds_single_reassignment_indices_format(ds, subseq) |>
              gen.and_then(\(indices) {
                gen.database_schema(letters[1:6], length(subseq), length(subseq)) |>
                  gen.with(\(rs2) {
                    list(
                      ds,
                      indices,
                      rs2,
                      single_subset_failure_type(ds, indices)
                    )
                  })
              })
          })
      }
      gen.ds_single_reassignment <- function(ds) {
        choices <- c(
          list(gen.ds_single_reassignment_success(ds)),
          list(gen.ds_single_reassignment_failure_emptyint(ds)),
          if (length(ds) > 1) list(gen.ds_single_reassignment_failure_multiint(ds))
        )
        weights <- c(70, 15, if (length(ds) > 1) 15)
        do.call(
          gen.choice,
          c(choices, list(prob = weights))
        )
      }
      expect_ds_subset_single_reassignment_success <- function(ds, ind, value) {
        res <- ds
        res[[ind]] <- value
        is_valid_database_schema(res)
        switch(
          class(ind),
          character = {
            negind <- setdiff(names(res), ind)
            expect_identical(res[negind], ds[negind])
            expect_identical(res[[ind]], setNames(value, ind))
          },
          integer = {
            expect_identical(res[-ind], ds[-ind])
            expect_identical(res[[ind]], setNames(value, names(ds)[[ind]]))
          },
          logical = {
            expect_identical(res[!ind], ds[!ind])
            expect_identical(res[[ind]], setNames(value, names(ds)[[ind]]))
          }
        )
      }
      forall(
        gen.database_schema(letters[1:6], 1, 8) |>
          gen.and_then(gen.ds_single_reassignment),
        \(ds, ind, value, error) {
          if (is.na(error)) {
            expect_ds_subset_single_reassignment_success(ds, ind, value)
          }else{
            expect_error(
              ds[[ind]] <- value,
              paste0("^", error, "$")
            )
          }
        },
        curry = TRUE
      )
    })
    it("$<-", {
      gen.ds_single_exact_reassignment_success_change <- function(ds) {
        list(
          gen.pure(ds),
          gen.element(seq_along(ds)) |>
            gen.with(\(subseq) names(ds)[[subseq]]),
          gen.database_schema(letters[1:6], 1, 1),
          gen.pure(NA_character_)
        )
      }
      gen.ds_single_exact_reassignment_success_add <- function(ds) {
        list(
          gen.pure(ds),
          gen.element(setdiff(letters, names(ds))),
          gen.database_schema(letters[1:6], 1, 1),
          gen.pure(NA_character_)
        )
      }
      gen.ds_single_exact_reassignment_failure <- function(ds) {
        gen.int(1) |>
          gen.and_then(\(n) {
            list(
              gen.pure(ds),
              gen.pure(n),
              gen.database_schema(letters[1:6], 1, 1),
              gen.pure(paste0(
                "<text>:1:4: unexpected numeric constant",
                "\n",
                "1: ds\\$", n,
                "\n",
                "       \\^"
              ))
            )
          })
      }
      gen.ds_single_exact_reassignment <- function(ds) {
        choices <- c(
          list(gen.ds_single_exact_reassignment_success_change(ds)),
          list(gen.ds_single_exact_reassignment_success_add(ds)),
          list(gen.ds_single_exact_reassignment_failure(ds))
        )
        weights <- c(40, 40, 20)
        do.call(
          gen.choice,
          c(choices, list(prob = weights))
        )
      }
      expect_ds_subset_single_exact_reassignment_success <- function(ds, ind, value) {
        res <- ds
        eval(parse(text = paste0("res$", ind, " <- value")))
        is_valid_database_schema(res)
        if (ind %in% names(ds)) {
          negind <- setdiff(names(res), ind)
          expect_identical(res[negind], ds[negind])
          expect_identical(res[[ind]], setNames(value, ind))
        }else{
          expect_identical(res[names(ds)], ds)
          expect_identical(res[[ind]], setNames(value, ind))
        }
      }
      forall(
        gen.database_schema(letters[1:6], 1, 8) |>
          gen.and_then(gen.ds_single_exact_reassignment),
        \(ds, ind, value, error) {
          if (is.na(error)) {
            expect_ds_subset_single_exact_reassignment_success(ds, ind, value)
          }else{
            expect_error(
              eval(parse(text = paste0("ds$", ind, " <- value"))),
              paste0("^", error, "$")
            )
          }
        },
        curry = TRUE
      )
    })
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

    forall(
      gen.subsequence(letters[1:8]) |>
        gen.with(\(x) if (length(x) > 3) x[1:3] else x) |>
        gen.with(empty_schema_from_attrs) |>
        gen.list(from = 2, to = 5),
      concatenate_keeps_attribute_order,
      curry = TRUE
    )

    # example where attributes aren't consistent, but are pairwise
    expect_failure(do.call(
      concatenate_keeps_attribute_order,
      list(c("a", "b"), c("b", "c"), c("c", "a")) |>
        lapply(empty_schema_from_attrs)
    ))

    forall(
      gen.subsequence(letters[1:6]) |>
        gen.with(empty_schema_from_attrs) |>
        gen.list(from = 2, to = 10),
      concatenate_keeps_attribute_order,
      curry = TRUE
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

  it("can have its attributes renamed", {
    forall(
      gen.database_schema(letters[1:6], 1, 8),
      function(ds) {
        names <- toupper(attrs_order(ds))
        ds2 <- rename_attrs(ds, names)
        expect_identical(
          ds2,
          database_schema(
            rename_attrs(subschemas(ds), names),
            lapply(
              references(ds),
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
  it("can be added to a data frame as a column", {
    rs <- relation_schema(
      list(
        a_b = list(c("a", "b", "c"), list(c("a", "b"))),
        a = list(c("a", "d"), list("a"))
      ),
      letters[1:4]
    )
    ds <- database_schema(rs, list())
    expect_no_error(tb <- data.frame(id = 1:2, schema = ds))
    expect_identical(tb$schema, ds)
  })
})
