describe("relation_schema", {
  it("expects valid input: schemas is a named list", {
    expect_error(relation_schema(1L, character()))
    expect_error(
      relation_schema(list(), character()),
      "^relation schemas must be named$"
    )
  })
  it("expects valid input: schema elements correct lengths", {
    expect_error(
      relation_schema(list(a = NULL), character()),
      "^schema elements must have length two: element 1$"
    )
    expect_error(
      relation_schema(list(a = NULL, b = 1:3), character()),
      "^schema elements must have length two: elements 1, 2$"
    )
  })
  it("expects valid input: schema elements contain characters of valid lengths", {
    expect_error(
      relation_schema(list(a = list(integer(), "a")), "a"),
      "^schema attribute sets must be characters: element 1$"
    )
    expect_error(
      relation_schema(
        list(a = list(integer(), "a"), a.1 = list(logical(), "a")),
        "a"
      ),
      "^schema attribute sets must be characters: elements 1, 2$"
    )
    expect_error(
      relation_schema(list(list(character(), 1L)), "1"),
      "^schema key sets must be lists: element 1$"
    )
    expect_error(
      relation_schema(
        list(a = list(character(), 1L), b = list(character(), FALSE)),
        "1"
      ),
      "^schema key sets must be lists: elements 1, 2$"
    )
    expect_error(
      relation_schema(list(a = list(character(), list())), "1"),
      "^schema key sets must have at least one element: element 1$"
    )
    expect_error(
      relation_schema(
        list(a = list(character(), list()), b = list(character(), list())),
        "1"
      ),
      "^schema key sets must have at least one element: elements 1, 2$"
    )
    expect_error(
      relation_schema(list(a = list(character(), list(1L))), "1"),
      "^schema key sets must have character elements: element 1\\.1$"
    )
    expect_error(
      relation_schema(
        list(a = list(character(), list(1L)), b = list("1", list("1", 1L))),
        "1"
      ),
      "^schema key sets must have character elements: elements 1\\.1, 2\\.2$"
    )
    expect_error(
      relation_schema(list(list(character(), list(character()))), 1L),
      "^expected character attrs_order$"
    )
  })
  it("expects valid input: unique schema names", {
    expect_error(
      relation_schema(
        list(
          a = list(character(), list(character())),
          a = list(character(), list(character()))
        ),
        character()
      ),
      "^relation schema names must be unique: duplicated a$"
    )
  })
  it("expects valid input: non-empty schema names", {
    expect_error(
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
      "^relation schema names must be non-empty: element 1"
    )
  })
  it("expects valid input: no duplicate attrs", {
    expect_error(
      relation_schema(list(a = list(c("a", "a"), list("a"))), "a"),
      "^relation attributes must be unique: element 1$"
    )
  })
  it("expects valid input: no duplicate attrs in keys", {
    expect_error(
      relation_schema(list(a = list("a", list(c("a", "a")))), "a"),
      "^relation key attributes must be unique: element 1\\.1\\.\\{a\\}$"
    )
    expect_error(
      relation_schema(
        list(
          a = list(c("a", "b"), list(c("a", "a", "b"))),
          b = list("b", list("b", c("b", "b", "b")))
        ),
        c("a", "b")
      ),
      "^relation key attributes must be unique: elements 1\\.1\\.\\{a\\}, 2\\.2\\.\\{b\\}$"
    )
  })
  it("expects valid input: no duplicate attrs_order", {
    expect_error(
      relation_schema(list(a = list("a", list("a"))), c("a", "a")),
      "^attrs_order must be unique: duplicated a$"
    )
  })
  it("expects valid input: all attributes given in attrs_order", {
    expect_error(
      relation_schema(list(a = list("a", list("a"))), "b"),
      "^attributes in schema must be present in attrs_order: absent a$"
    )
  })
  it("expects valid input: key attributes are in relation", {
    expect_error(
      relation_schema(list(a = list("a", list("b"))), c("a", "b")),
      "^attributes in keys must be present in relation: element 1\\.\\{b\\}$"
    )
  })

  it("returns a valid relation schema", {
    forall(
      gen.relation_schema(letters[1:2], 2, 20),
      is_valid_relation_schema
    )
  })
  it("orders key attributes with respect to order in attrs_order", {
    key_attributes_ordered <- function(rs) {
      keys_matches <- vapply(
        keys(rs),
        with_args(
          vapply,
          with_args(match, table = attrs_order(rs)) %>>%
            (Negate(is.unsorted)),
          logical(1)
        ) %>>%
          all,
        logical(1)
      )
      expect_true(all(keys_matches))
    }
    forall(gen.relation_schema(letters[1:6], 1, 8), key_attributes_ordered)
  })
  it("orders attributes with respect to appearance in keys, then by attrs_order", {
    attributes_ordered <- function(rs) {
      attr_matches <- vapply(
        seq_along(rs),
        \(n) {
          as <- attrs(rs)[[n]]
          ks <- keys(rs)[[n]]
          key_given <- unique(unlist(ks))
          rest <- setdiff(as, key_given)
          identical(
            as,
            c(
              key_given,
              rest[order(match(rest, attrs_order(rs)))]
            )
          )
        },
        logical(1)
      )
      expect_true(all(attr_matches))
    }
    forall(gen.relation_schema(letters[1:6], 1, 8), attributes_ordered)
  })

  it("is subsetted to a valid relation schema, follows usual subsetting rules...", {
    forall(
      gen.relation_schema(letters[1:6], 0, 8) |>
        gen.and_then(\(rs) list(
          gen.pure(rs),
          gen.sample_resampleable(c(FALSE, TRUE), of = length(rs))
        )),
      \(rs, i) {
        is_valid_relation_schema(rs[i])

        inum <- which(i)
        is_valid_relation_schema(rs[inum])
        expect_identical(rs[i], rs[inum])

        ineg <- -setdiff(seq_along(rs), inum)
        if (!all(i)) {
          is_valid_relation_schema(rs[ineg])
          expect_identical(rs[i], rs[ineg])
        }

        is_valid_relation_schema(rs[names(rs)[i]])
        expect_identical(rs[i], rs[names(rs)[i]])

        expect_length(rs[i], sum(i))

        ints <- stats::setNames(seq_along(rs), names(rs))
        expect_identical(rs[i], rs[ints[i]])
        expect_identical(rs[ineg], rs[ints[ineg]])
        expect_identical(rs[names(rs)[i]], rs[names(rs)[ints[i]]])
      },
      curry = TRUE
    )
    forall(
      gen.relation_schema(letters[1:6], 1, 8) |>
        gen.and_then(\(rs) list(
          gen.pure(rs),
          gen.element(seq_along(rs))
        )),
      \(rs, inum) {
        is_valid_relation_schema(rs[[inum]])
        expect_identical(rs[inum], rs[[inum]])

        ineg <- -setdiff(seq_along(rs), inum)
        if (length(ineg) == 1) {
          is_valid_relation_schema(rs[[ineg]])
          expect_identical(rs[inum], rs[[ineg]])
        }

        is_valid_relation_schema(rs[[names(rs)[[inum]]]])
        expect_identical(rs[inum], rs[[names(rs)[[inum]]]])

        is_valid_relation_schema(eval(rlang::expr(`$`(rs, !!names(rs)[[inum]]))))
        expect_identical(rs[inum], eval(rlang::expr(`$`(rs, !!names(rs)[[inum]]))))

        ints <- stats::setNames(seq_along(rs), names(rs))
        expect_identical(rs[[inum]], rs[[ints[[inum]]]])
        expect_identical(
          tryCatch(rs[[ineg]], error = function(e) e$message),
          tryCatch(rs[[ints[[ineg]]]], error = function(e) e$message)
        )
        expect_identical(rs[[names(rs)[[inum]]]], rs[[names(rs)[[ints[[inum]]]]]])
      },
      curry = TRUE
    )
    forall(
      gen.relation_schema(letters[1:6], 1, 8),
      \(rs) {
        expect_identical(rs[[TRUE]], rs[[1]])
      }
    )
    forall(
      gen.relation_schema(letters[1:6], 1, 8) |>
        gen.and_then(\(rs) list(
          rs = gen.pure(rs),
          indices = gen.sample_resampleable(
            seq_along(rs),
            from = 2,
            to = 2*length(rs)
          )
        )),
      \(rs, indices) {
        is_valid_relation_schema(rs[indices])
      },
      curry = TRUE
    )
  })
  it("... except allowing non-matches as NAs", {
    rs <- relation_schema(
      list(a = list("a", list("a"))),
      c("a")
    )
    expect_error(
      rs[c("b", "c")],
      "^subset names that don't exist: b, c$"
    )
  })
  it("can be subsetted while preserving attributes", {
    x <- relation_schema(list(a = list(c("a", "b"), list("a"))), letters[1:5])
    expect_identical(x[TRUE], x)
    expect_identical(
      x[FALSE],
      relation_schema(setNames(list(), character()), letters[1:5])
    )
    expect_identical(x[[1]], x)
    expect_error(x[[integer()]])
    expect_error(x[[c(1, 1)]])
  })

  it("expects a relation_schema value for subset re-assignment", {
    rs <- relation_schema(
      list(X = list(character(), list(character()))),
      letters[1:6]
    )
    expect_error(rs[1] <- 1L, "^value must also be a relation_schema object$")
    expect_error(rs[[1]] <- 1L, "^value must also be a relation_schema object$")
    expect_error(rs$X <- 1L, "^value must also be a relation_schema object$")
  })
  describe("can have subsets re-assigned, without changing relation names", {
    it("[<-", {
      gen.rs_reassignment_indices_format <- function(rs, subseq) {
        choices <- c(
          list(gen.pure(subseq)),
          if (length(subseq) < length(rs))
            list(gen.pure(-setdiff(seq_along(rs), subseq))),
          list(gen.pure(names(rs)[subseq])),
          list(seq_along(rs) %in% subseq)
        )
        weights <- rep(1L, 3L + (length(subseq) < length(rs)))
        do.call(gen.choice, c(choices, list(prob = weights)))
      }
      gen.rs_reassignment <- function(rs) {
        gen.subsequence(seq_along(rs)) |>
          gen.and_then(\(subseq) {
            gen.rs_reassignment_indices_format(rs, subseq) |>
              gen.and_then(\(inds) {
                gen.relation_schema(letters[1:6], length(subseq), length(subseq)) |>
                  gen.with(\(rs2) {
                    list(rs, inds, rs2)
                  })
              })
          })
      }
      expect_rs_subset_reassignment_success <- function(rs, indices, value) {
        res <- rs
        res[indices] <- value
        is_valid_relation_schema(res)
        switch(
          class(indices),
          character = {
            negind <- setdiff(names(res), indices)
            expect_identical(res[negind], rs[negind])
            expect_identical(res[indices], setNames(value, indices))
          },
          integer = {
            expect_identical(res[-indices], rs[-indices])
            expect_identical(res[indices], setNames(value, names(rs)[indices]))
          },
          logical = {
            expect_identical(res[!indices], rs[!indices])
            expect_identical(res[indices], setNames(value, names(rs)[indices]))
          }
        )
      }
      forall(
        gen.relation_schema(letters[1:6], 0, 8) |>
          gen.and_then(gen.rs_reassignment),
        expect_rs_subset_reassignment_success,
        curry = TRUE
      )
    })
    it("[[<-", {
      gen.rs_single_reassignment_indices_format <- function(rs, subseq) {
        choices <- c(
          list(gen.pure(subseq)),
          if (length(rs) == 2)
            list(gen.pure(-setdiff(seq_along(rs), subseq))),
          list(gen.pure(names(rs)[subseq])),
          if (length(rs) == 1)
            list(gen.pure(seq_along(rs) %in% subseq))
        )
        weights <- rep(
          1L,
          2L + (length(rs) == 2) + (length(rs) == 1)
        )
        do.call(gen.choice, c(choices, list(prob = weights)))
      }
      gen.rs_single_reassignment_success <- function(rs) {
        list(
          gen.pure(rs),
          gen.element(seq_along(rs)) |>
            gen.and_then(\(subseq) {
              gen.rs_single_reassignment_indices_format(rs, subseq)
            }),
          gen.relation_schema(letters[1:6], 1, 1),
          gen.pure(NA_character_)
        )
      }
      gen.rs_single_reassignment_failure_emptyint <- function(rs) {
        list(
          gen.pure(rs),
          gen.rs_single_reassignment_indices_format(rs, integer()),
          gen.relation_schema(letters[1:6], 0, 0)
        ) |>
          gen.with(\(lst) {
            c(
              lst,
              list(single_subset_failure_type(rs, lst[[2]]))
            )
          })
      }
      gen.rs_single_reassignment_failure_multiint <- function(rs) {
        list(
          gen.sample(seq_along(rs), 2, replace = FALSE),
          gen.subsequence(seq_along(rs))
        ) |>
          gen.with(unlist %>>% unique %>>% sort) |>
          gen.and_then(\(subseq) {
            gen.rs_single_reassignment_indices_format(rs, subseq) |>
              gen.and_then(\(indices) {
                gen.relation_schema(letters[1:6], length(subseq), length(subseq)) |>
                  gen.with(\(rs2) {
                    list(
                      rs,
                      indices,
                      rs2,
                      single_subset_failure_type(rs, indices)
                    )
                  })
              })
          })
      }
      gen.rs_single_reassignment <- function(rs) {
        choices <- c(
          list(gen.rs_single_reassignment_success(rs)),
          list(gen.rs_single_reassignment_failure_emptyint(rs)),
          if (length(rs) > 1) list(gen.rs_single_reassignment_failure_multiint(rs))
        )
        weights <- c(70, 15, if (length(rs) > 1) 15)
        do.call(
          gen.choice,
          c(choices, list(prob = weights))
        )
      }
      expect_rs_subset_single_reassignment_success <- function(rs, ind, value) {
        res <- rs
        res[[ind]] <- value
        is_valid_relation_schema(res)
        switch(
          class(ind),
          character = {
            negind <- setdiff(names(res), ind)
            expect_identical(res[negind], rs[negind])
            expect_identical(res[[ind]], setNames(value, ind))
          },
          integer = {
            expect_identical(res[-ind], rs[-ind])
            expect_identical(res[[ind]], setNames(value, names(rs)[[ind]]))
          },
          logical = {
            expect_identical(res[!ind], rs[!ind])
            expect_identical(res[[ind]], setNames(value, names(rs)[[ind]]))
          }
        )
      }
      forall(
        gen.relation_schema(letters[1:6], 1, 8) |>
          gen.and_then(gen.rs_single_reassignment),
        \(rs, ind, value, error) {
          if (is.na(error)) {
            expect_rs_subset_single_reassignment_success(rs, ind, value)
          }else{
            expect_error(
              rs[[ind]] <- value,
              paste0("^", error, "$")
            )
          }
        },
        curry = TRUE
      )
    })
    it("$<-", {
      gen.rs_single_exact_reassignment_success_change <- function(rs) {
        list(
          gen.pure(rs),
          gen.element(seq_along(rs)) |>
            gen.with(\(subseq) names(rs)[[subseq]]),
          gen.relation_schema(letters[1:6], 1, 1),
          gen.pure(NA_character_)
        )
      }
      gen.rs_single_exact_reassignment_success_add <- function(rs) {
        list(
          gen.pure(rs),
          gen.element(setdiff(letters, names(rs))),
          gen.relation_schema(letters[1:6], 1, 1),
          gen.pure(NA_character_)
        )
      }
      gen.rs_single_exact_reassignment_failure <- function(rs) {
        gen.int(1) |>
          gen.and_then(\(n) {
            list(
              gen.pure(rs),
              gen.pure(n),
              gen.relation_schema(letters[1:6], 1, 1),
              gen.pure(paste0(
                "<text>:1:4: unexpected numeric constant",
                "\n",
                "1: rs\\$", n,
                "\n",
                "       \\^"
              ))
            )
          })
      }
      gen.rs_single_exact_reassignment <- function(rs) {
        choices <- c(
          list(gen.rs_single_exact_reassignment_success_change(rs)),
          list(gen.rs_single_exact_reassignment_success_add(rs)),
          list(gen.rs_single_exact_reassignment_failure(rs))
        )
        weights <- c(40, 40, 20)
        do.call(
          gen.choice,
          c(choices, list(prob = weights))
        )
      }
      expect_rs_subset_single_exact_reassignment_success <- function(rs, ind, value) {
        res <- rs
        eval(parse(text = paste0("res$", ind, " <- value")))
        is_valid_relation_schema(res)
        if (ind %in% names(rs)) {
          negind <- setdiff(names(res), ind)
          expect_identical(res[negind], rs[negind])
          expect_identical(res[[ind]], setNames(value, ind))
        }else{
          expect_identical(res[names(rs)], rs)
          expect_identical(res[[ind]], setNames(value, ind))
        }
      }
      forall(
        gen.relation_schema(letters[1:6], 1, 8) |>
          gen.and_then(gen.rs_single_exact_reassignment),
        \(rs, ind, value, error) {
          if (is.na(error)) {
            expect_rs_subset_single_exact_reassignment_success(rs, ind, value)
          }else{
            expect_error(
              eval(parse(text = paste0("rs$", ind, " <- value"))),
              paste0("^", error, "$")
            )
          }
        },
        curry = TRUE
      )
    })
  })

  it("is made unique to a valid relation schema", {
    forall(
      gen.relation_schema(letters[1:6], 0, 8),
      unique %>>% is_valid_relation_schema
    )
  })
  it("is made unique with no duplicate schemas", {
    forall(
      gen.relation_schema(letters[1:6], 1, 8),
      \(rs) {
        rs2 <- c(rs, rs)
        expect_false(Negate(anyDuplicated)(rs2))
        expect_true(Negate(anyDuplicated)(unique(rs2)))
      }
    )
  })

  it("concatenates to a valid relation schema", {
    forall(
      gen.relation_schema(letters[1:6], from = 0, to = 4) |>
        gen.list(from = 1, to = 3),
      c %>>% is_valid_relation_schema,
      curry = TRUE
    )
  })
  it("concatenates with duplicates preserved", {
    forall(
      gen.relation_schema(letters[1:6], 1, 8) |>
        gen.with(\(rs) list(rs, rs)),
      \(lst) {
        expect_length(do.call(c, lst), sum(lengths(lst)))
      }
    )
  })
  it("concatenates without losing attributes", {
    concatenate_lossless_for_attrs_order <- function(...) {
      lst <- list(...)
      res <- c(...)
      for (l in lst) {
        expect_true(all(is.element(attrs_order(l), attrs_order(res))))
      }
    }
    forall(
      gen.relation_schema(letters[1:6], from = 0, to = 8) |>
        gen.list(from = 1, to = 10),
      concatenate_lossless_for_attrs_order,
      curry = TRUE
    )
  })
  it("concatenates without losing attribute orderings, if consistent", {
    forall(
      gen.sample(letters[1:8], gen.element(1:3)) |>
        gen.with(
          sort %>>%
            with_args(relation_schema, schemas = setNames(list(), character()))
        ) |>
        gen.list(from = 2, to = 5),
      concatenate_keeps_attribute_order,
      curry = TRUE
    )

    # example where attributes aren't consistent, but are pairwise
    schemas <- list(
      relation_schema(setNames(list(), character()), c("a", "b")),
      relation_schema(setNames(list(), character()), c("b", "c")),
      relation_schema(setNames(list(), character()), c("c", "a"))
    )
    expect_failure(do.call(concatenate_keeps_attribute_order, schemas))

    forall(
      gen.subsequence(letters[1:6]) |>
        gen.with(\(attrs) relation_schema(setNames(list(), character()), attrs)) |>
        gen.list(from = 2, to = 10),
      concatenate_keeps_attribute_order,
      curry = TRUE
    )
  })
  it("concatenates without losing schemas", {
    concatenate_lossless_for_schemas <- function(...) {
      lst <- list(...)
      res <- c(...)
      for (l in lst) {
        sorted <- l
        # sort attrs to keep test independent from that for
        # attribute orderings
        attrs_order(sorted) <- attrs_order(res)
        expect_true(all(is.element(
          sorted,
          res
        )))
      }
    }
    forall(
      gen.relation_schema(letters[1:6], from = 0, to = 8) |>
        gen.list(from = 1, to = 10),
      concatenate_lossless_for_schemas,
      curry = TRUE
    )
  })

  it("can have empty-key schemas merged", {
    up_to_one_empty_key <- function(rs) {
      if (sum(vapply(keys(rs), identical, logical(1), list(character()))) <= 1)
        discard()
      res <- merge_empty_keys(rs)
      is_valid_relation_schema(rs)
      expect_lte(
        sum(vapply(keys(res), identical, logical(1), list(character()))),
        1L
      )
    }
    forall(
      gen.relation_schema_empty_keys(letters[1:6], 1, 8, min_empty = 1),
      up_to_one_empty_key
    )
  })
  it("is composed of its attrs(), keys(), names(), and attrs_order()", {
    forall(
      gen.relation_schema(letters[1:6], 0, 8),
      \(rs) expect_identical(
        rs,
        relation_schema(
          setNames(Map(list, attrs(rs), keys(rs)), names(rs)),
          attrs_order = attrs_order(rs)
        )
      )
    )
  })

  it("is created with logical attribute classes", {
    forall(
      gen.relation_schema(letters[1:6], 1, 8),
      \(rs) {
        r <- create(rs)
        classes <- unlist(
          unname(lapply(records(r), \(df) lapply(df, class))),
          recursive = FALSE
        )
        if (is.null(names(classes)))
          names(classes) <- character()
        expect_identical(
          unname(classes),
          rep(list("logical"), length(classes))
        )
      }
    )
  })

  it("can have its attributes renamed", {
    forall(
      gen.relation_schema(letters[1:6], 1, 8),
      function(rs) {
        rs2 <- rename_attrs(rs, toupper(attrs_order(rs)))
        expect_identical(
          rs2,
          relation_schema(
            setNames(
              Map(
                list,
                lapply(attrs(rs), toupper),
                lapply(keys(rs), lapply, toupper)
              ),
              names(rs)
            ),
            attrs_order = toupper(attrs_order(rs))
          )
        )
      }
    )
  })

  it("prints", {
    expect_output(
      print(relation_schema(setNames(list(), character()), character())),
      "\\A0 relation schemas\\n0 attributes\\Z",
      perl = TRUE
    )
    expect_output(
      print(relation_schema(
        list(a = list(c("a", "b"), list("a"))),
        c("a", "b")
      )),
      paste0(
        "\\A",
        "1 relation schema",
        "\\n",
        "2 attributes: a, b",
        "\\n",
        "schema a: a, b\\n  key 1: a",
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
    expect_no_error(tb <- data.frame(id = 1:2, schema = rs))
    expect_identical(tb$schema, rs)
  })
  it("can be formatted, e.g. for data.frame entry", {
    rs <- relation_schema(
      list(
        a_b = list(c("a", "b", "c"), list(c("a", "b"))),
        a = list(c("a", "d"), list("a"))
      ),
      letters[1:4]
    )
    expect_identical(
      format(rs),
      c("schema a_b", "schema a")
    )
  })
})
