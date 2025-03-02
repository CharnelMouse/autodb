describe("relation", {
  it("expects valid input: relations is a named list", {
    expect_error(relation(1L, character()))
    expect_error(
      relation(list(), character()),
      "^relations must be named$"
    )
  })
  it("expects valid input: relation elements correct lengths", {
    expect_error(
      relation(list(a = NULL), character()),
      "^relation elements must have length two: element 1$"
    )
  })
  it("expects valid input: list elements contain df and keys elements, and no others, correct classes", {
    expect_error(
      relation(list(X = list(df = data.frame(), 1)), character()),
      "^relations must contain 'df' and 'keys' elements: element 1$"
    )
    expect_error(
      relation(
        list(X = list(df = data.frame(), keys = data.frame())),
        character()
      ),
      "^relation 'keys' elements must be lists: element 1$"
    )
    expect_error(
      relation(
        list(X = list(df = list(), keys = list())),
        character()
      ),
      "^relation 'df' elements must be data frames: element 1$"
    )
    expect_silent(relation(
      list(X = list(keys = setNames(list(), character()), df = data.frame())),
      character()
    ))
  })
  it("expects valid input: attrs_order is a character", {
    expect_error(relation(list(), 1L))
  })
  it("expects valid input: no duplicate attrs", {
    expect_error(
      relation(
        list(a = list(
          df = data.frame(a = logical(), a = logical(), check.names = FALSE),
          keys = list("a")
        )),
        "a"
      ),
      "^relation attributes must be unique: element 1$"
    )
  })
  it("expects valid input: no duplicate attrs in keys", {
    expect_error(
      relation(
        list(a = list(df = data.frame(a = logical()), keys = list(c("a", "a")))),
        "a"
      ),
      "^relation key attributes must be unique: element 1\\.1\\.\\{a\\}$"
    )
  })
  it("expects valid input: no duplicate attrs_order", {
    expect_error(
      relation(
        list(a = list(df = data.frame(a = logical()), keys = list("a"))),
        c("a", "a")
      ),
      "^attrs_order must be unique: duplicated a$"
    )
  })
  it("expects valid input: relation attributes are in attrs_order", {
    expect_error(
      relation(
        list(a = list(df = data.frame(a = integer()), keys = list("a"))),
        "b"
      ),
      "^relation attributes not in attrs_order: missing a$"
    )
  })
  it("expects valid input: relation attributes get ordered by key mentions first", {
    expect_silent(relation(
      list(b = list(df = data.frame(b = integer(), a = integer()), keys = list("b"))),
      c("a", "b")
    ))
  })
  it("expects valid input: keys are in columns", {
    expect_error(
      relation(
        list(a = list(
          df = data.frame(a = rep(1L, 2L), b = 1L, c = 1L),
          keys = list("b", c("c", "d"))
        )),
        c("a", "b", "c", "d")
      ),
      "^relation keys must be within relation attributes: element 1\\.2\\.\\{d\\}$"
    )
  })
  it("expects valid input: keys are satisfied", {
    expect_error(
      relation(
        list(a = list(df = data.frame(a = rep(1L, 2L)), keys = list("a"))),
        "a"
      ),
      "^relations must satisfy their keys: element 1\\.\\{a\\}$"
    )
    expect_error(
      relation(
        list(a = list(
          df = data.frame(a = 1:2),
          keys = list(character())
        )),
        "a"
      ),
      "^relations must satisfy their keys: element 1\\.\\{\\}$"
    )
  })
  it("expects valid input: unique relation names", {
    expect_error(
      relation(
        list(
          a = list(df = data.frame(), keys = list(character())),
          a = list(df = data.frame(), keys = list(character()))
        ),
        character()
      ),
      "^relation names must be unique: duplicated a$"
    )
  })
  it("expects valid input: non-empty relation names", {
    expect_error(
      relation(
        setNames(
          list(
            a = list(df = data.frame(), keys = list(character())),
            b = list(df = data.frame(), keys = list(character()))
          ),
          c("", "b")
        ),
        character()
      ),
      "^relation names must be non-empty: element 1"
    )
  })

  it("expects record reassignments to have all prime attributes, maybe others, order-independent", {
    x <- relation(
      list(a = list(df = data.frame(a = 1:4, b = 1:2), keys = list("a"))),
      attrs_order = c("a", "b")
    )
    expect_error(
      records(x) <- list(a = data.frame(b = 1:2)),
      "^record reassignments must keep key attributes$"
    )
    expect_error(
      records(x) <- list(a = data.frame(a = 1:4, c = 1)),
      "^record reassignments can not add attributes$"
    )
    y <- x
    expect_silent(records(y) <- list(a = data.frame(b = 1:2, a = 1:4)))
    expect_identical(y, x)
    expect_silent(records(y) <- list(a = data.frame(a = 1:4)))
    x2 <- relation(
      list(a = list(df = data.frame(b = 1:4, a = 1:2), keys = list("b"))),
      attrs_order = c("a", "b")
    )
    y2 <- x2
    expect_silent(records(y2) <- list(a = data.frame(a = 1:2, b = 1:4)))
    expect_identical(y2, x2)
  })
  it("expects records reassignments to have unique attribute names", {
    x <- relation(
      list(a = list(df = data.frame(a = 1:4, b = 1:2), keys = list("a"))),
      attrs_order = c("a", "b")
    )
    expect_error(
      records(x)[[1]] <- data.frame(a = 1:4, a = 1:2, check.names = FALSE)
    )
  })
  it("expect records name reassignments to result in an error or a valid relation", {
    forall(
      gen.relation(letters[1:6], 1, 8) |>
        gen.and_then(\(rel) {
          nonempty <- which(lengths(attrs(rel)) > 0)
          if (length(nonempty) == 0)
            return(list(
              gen.pure(rel),
              gen.pure(1L),
              gen.pure(attrs(rel)[[1]])
            ))
          gen.element(nonempty) |>
            gen.and_then(\(n) {
              list(
                gen.pure(rel),
                gen.pure(n),
                gen.sample_resampleable(
                  attrs(rel)[[n]],
                  to = length(attrs(rel)[[n]])
                )
              )
            })
        }),
      \(rel, n, nm) {
        res <- try(names(records(rel)[[n]]) <- nm, silent = TRUE)
        expect_true(
          class(res)[[1]] == "try-error" ||
            is.null(try(is_valid_relation(rel), silent = TRUE))
        )
      },
      curry = TRUE
    )
  })

  it("sorts relation key contents attrs_order", {
    expect_identical(
      keys(relation(
        list(
          a = list(
            df = data.frame(a = integer(), b = integer()),
            keys = list(c("b", "a"))
          )
        ),
        c("a", "b")
      )),
      list(a = list(c("a", "b")))
    )
  })
  it("sorts relation keys according to length and attrs_order", {
    expect_identical(
      keys(relation(
        list(
          a = list(
            df = data.frame(a = integer(), b = integer()),
            keys = list("b", "a")
          )
        ),
        c("a", "b")
      )),
      list(a = list("a", "b"))
    )
  })
  it("removes duplicate keys", {
    expect_no_dup_keys <- function(rel) {
      expect_true(all(vapply(keys(rel), Negate(anyDuplicated), logical(1))))
    }
    expect_no_dup_keys(relation(
      list(ab = list(
        df = data.frame(a = integer(), b = integer()),
        keys = list(c("a", "b"), c("a", "b"))
      )),
      c("a", "b")
    ))
  })
  it("sorts relation attributes according to sorted keys and attrs_order", {
    expect_identical(
      attrs(relation(
        list(
          a = list(
            df = data.frame(b = integer(), a = integer()),
            keys = list("a")
          )
        ),
        c("a", "b")
      )),
      list(a = c("a", "b"))
    )
  })

  it("is subsetted to a valid relation schema, obeys usual subsetting rules", {
    forall(
      gen.relation(letters[1:6], 0, 8) |>
        gen.and_then(\(rel) list(
          gen.pure(rel),
          gen.sample_resampleable(c(FALSE, TRUE), of = length(rel))
        )),
      \(rel, i) {
        is_valid_relation(rel[i])

        inum <- which(i)
        is_valid_relation(rel[inum])
        expect_identical(rel[i], rel[inum])

        ineg <- -setdiff(seq_along(rel), inum)
        if (!all(i)) {
          is_valid_relation(rel[ineg])
          expect_identical(rel[i], rel[ineg])
        }

        is_valid_relation(rel[names(rel)[i]])
        expect_identical(rel[i], rel[names(rel)[i]])

        expect_length(rel[i], sum(i))

        ints <- stats::setNames(seq_along(rel), names(rel))
        expect_identical(rel[i], rel[ints[i]])
        expect_identical(rel[ineg], rel[ints[ineg]])
        expect_identical(rel[names(rel)[i]], rel[names(rel)[ints[i]]])
      },
      curry = TRUE
    )
    forall(
      gen.relation(letters[1:6], 1, 8) |>
        gen.and_then(\(rel) list(
          gen.pure(rel),
          gen.element(seq_along(rel))
        )),
      \(rel, inum) {
        is_valid_relation(rel[[inum]])
        expect_identical(rel[inum], rel[[inum]])

        ineg <- -setdiff(seq_along(rel), inum)
        if (length(ineg) == 1) {
          is_valid_relation(rel[[ineg]])
          expect_identical(rel[inum], rel[[ineg]])
        }

        is_valid_relation(rel[[names(rel)[[inum]]]])
        expect_identical(rel[inum], rel[[names(rel)[[inum]]]])

        is_valid_relation(eval(rlang::expr(`$`(rel, !!names(rel)[[inum]]))))
        expect_identical(rel[inum], eval(rlang::expr(`$`(rel, !!names(rel)[[inum]]))))

        ints <- stats::setNames(seq_along(rel), names(rel))
        expect_identical(rel[[inum]], rel[[ints[[inum]]]])
        expect_identical(
          tryCatch(rel[[ineg]], error = function(e) e$message),
          tryCatch(rel[[ints[[ineg]]]], error = function(e) e$message)
        )
        expect_identical(rel[[names(rel)[[inum]]]], rel[[names(rel)[[ints[[inum]]]]]])
      },
      curry = TRUE
    )
    forall(
      gen.relation(letters[1:6], 1, 8),
      \(rel) {
        expect_identical(rel[[TRUE]], rel[[1]])
      }
    )
    forall(
      gen.relation(letters[1:6], 1, 8) |>
        gen.and_then(\(rel) list(
          rel = gen.pure(rel),
          indices = gen.sample_resampleable(
            seq_along(rel),
            from = 2,
            to = 2*length(rel)
          )
        )),
      \(rel, indices) {
        is_valid_relation(rel[indices])
      },
      curry = TRUE
    )
  })
  it("can be subsetted while preserving attributes", {
    x <- relation(
      list(a = list(
        df = data.frame(a = logical(), b = logical()),
        keys = list("a")
      )),
      letters[1:5]
    )
    expect_identical(x[TRUE], x)
    expect_identical(
      x[FALSE],
      relation(setNames(list(), character()), letters[1:5])
    )
    expect_identical(x[[1]], x)
    expect_error(x[[integer()]])
    expect_error(x[[c(1, 1)]])
  })

  it("expects a relation value for subset re-assignment", {
    rel <- create(relation_schema(
      list(X = list(character(), list(character()))),
      letters[1:6]
    ))
    expect_error(rel[1] <- 1L, "^value must also be a relation object$")
    expect_error(rel[[1]] <- 1L, "^value must also be a relation object$")
    expect_error(rel$X <- 1L, "^value must also be a relation object$")
  })
  describe("can have subsets re-assigned, without changing relation names", {
    it("[<-", {
      gen.rel_reassignment_indices_format <- function(rel, subseq) {
        choices <- c(
          list(gen.pure(subseq)),
          if (length(subseq) < length(rel))
            list(gen.pure(-setdiff(seq_along(rel), subseq))),
          list(gen.pure(names(rel)[subseq])),
          list(seq_along(rel) %in% subseq)
        )
        weights <- rep(1L, 3L + (length(subseq) < length(rel)))
        do.call(gen.choice, c(choices, list(prob = weights)))
      }
      gen.rel_reassignment <- function(rel) {
        gen.subsequence(seq_along(rel)) |>
          gen.and_then(\(subseq) {
            gen.rel_reassignment_indices_format(rel, subseq) |>
              gen.and_then(\(inds) {
                gen.relation(letters[1:6], length(subseq), length(subseq)) |>
                  gen.with(\(rs2) {
                    list(rel, inds, rs2)
                  })
              })
          })
      }
      expect_rel_subset_reassignment_success <- function(rel, indices, value) {
        res <- rel
        res[indices] <- value
        is_valid_relation(res)
        switch(
          class(indices),
          character = {
            negind <- setdiff(names(res), indices)
            expect_identical(res[negind], rel[negind])
            expect_identical(res[indices], setNames(value, indices))
          },
          integer = {
            expect_identical(res[-indices], rel[-indices])
            expect_identical(res[indices], setNames(value, names(rel)[indices]))
          },
          logical = {
            expect_identical(res[!indices], rel[!indices])
            expect_identical(res[indices], setNames(value, names(rel)[indices]))
          }
        )
      }
      forall(
        gen.relation(letters[1:6], 0, 8) |>
          gen.and_then(gen.rel_reassignment),
        expect_rel_subset_reassignment_success,
        curry = TRUE
      )
    })
    it("[[<-", {
      gen.rel_single_reassignment_indices_format <- function(rel, subseq) {
        choices <- c(
          list(gen.pure(subseq)),
          if (length(rel) == 2)
            list(gen.pure(-setdiff(seq_along(rel), subseq))),
          list(gen.pure(names(rel)[subseq])),
          if (length(rel) == 1)
            list(gen.pure(seq_along(rel) %in% subseq))
        )
        weights <- rep(
          1L,
          2L + (length(rel) == 2) + (length(rel) == 1)
        )
        do.call(gen.choice, c(choices, list(prob = weights)))
      }
      gen.rel_single_reassignment_success <- function(rel) {
        list(
          gen.pure(rel),
          gen.element(seq_along(rel)) |>
            gen.and_then(\(subseq) {
              gen.rel_single_reassignment_indices_format(rel, subseq)
            }),
          gen.relation(letters[1:6], 1, 1),
          gen.pure(NA_character_)
        )
      }
      gen.rel_single_reassignment_failure_emptyint <- function(rel) {
        list(
          gen.pure(rel),
          gen.rel_single_reassignment_indices_format(rel, integer()),
          gen.relation(letters[1:6], 0, 0)
        ) |>
          gen.with(\(lst) {
            c(
              lst,
              list(single_subset_failure_type(rel, lst[[2]]))
            )
          })
      }
      gen.rel_single_reassignment_failure_multiint <- function(rel) {
        list(
          gen.sample(seq_along(rel), 2, replace = FALSE),
          gen.subsequence(seq_along(rel))
        ) |>
          gen.with(unlist %>>% unique %>>% sort) |>
          gen.and_then(\(subseq) {
            gen.rel_single_reassignment_indices_format(rel, subseq) |>
              gen.and_then(\(indices) {
                gen.relation(letters[1:6], length(subseq), length(subseq)) |>
                  gen.with(\(rs2) {
                    list(
                      rel,
                      indices,
                      rs2,
                      single_subset_failure_type(rel, indices)
                    )
                  })
              })
          })
      }
      gen.rel_single_reassignment <- function(rel) {
        choices <- c(
          list(gen.rel_single_reassignment_success(rel)),
          list(gen.rel_single_reassignment_failure_emptyint(rel)),
          if (length(rel) > 1) list(gen.rel_single_reassignment_failure_multiint(rel))
        )
        weights <- c(70, 15, if (length(rel) > 1) 15)
        do.call(
          gen.choice,
          c(choices, list(prob = weights))
        )
      }
      expect_rel_subset_single_reassignment_success <- function(rel, ind, value) {
        res <- rel
        res[[ind]] <- value
        is_valid_relation(res)
        switch(
          class(ind),
          character = {
            negind <- setdiff(names(res), ind)
            expect_identical(res[negind], rel[negind])
            expect_identical(res[[ind]], setNames(value, ind))
          },
          integer = {
            expect_identical(res[-ind], rel[-ind])
            expect_identical(res[[ind]], setNames(value, names(rel)[[ind]]))
          },
          logical = {
            expect_identical(res[!ind], rel[!ind])
            expect_identical(res[[ind]], setNames(value, names(rel)[[ind]]))
          }
        )
      }
      forall(
        gen.relation(letters[1:6], 1, 8) |>
          gen.and_then(gen.rel_single_reassignment),
        \(rel, ind, value, error) {
          if (is.na(error)) {
            expect_rel_subset_single_reassignment_success(rel, ind, value)
          }else{
            expect_error(
              rel[[ind]] <- value,
              paste0("^", error, "$")
            )
          }
        },
        curry = TRUE
      )
    })
    it("$<-", {
      gen.rel_single_exact_reassignment_success_change <- function(rel) {
        list(
          gen.pure(rel),
          gen.element(seq_along(rel)) |>
            gen.with(\(subseq) names(rel)[[subseq]]),
          gen.relation(letters[1:6], 1, 1),
          gen.pure(NA_character_)
        )
      }
      gen.rel_single_exact_reassignment_success_add <- function(rel) {
        list(
          gen.pure(rel),
          gen.element(setdiff(letters, names(rel))),
          gen.relation(letters[1:6], 1, 1),
          gen.pure(NA_character_)
        )
      }
      gen.rel_single_exact_reassignment_failure <- function(rel) {
        gen.int(1) |>
          gen.and_then(\(n) {
            list(
              gen.pure(rel),
              gen.pure(n),
              gen.relation(letters[1:6], 1, 1),
              gen.pure(paste0(
                "<text>:1:5: unexpected numeric constant",
                "\n",
                "1: rel\\$", n,
                "\n",
                "        \\^"
              ))
            )
          })
      }
      gen.rel_single_exact_reassignment <- function(rel) {
        choices <- c(
          list(gen.rel_single_exact_reassignment_success_change(rel)),
          list(gen.rel_single_exact_reassignment_success_add(rel)),
          list(gen.rel_single_exact_reassignment_failure(rel))
        )
        weights <- c(40, 40, 20)
        do.call(
          gen.choice,
          c(choices, list(prob = weights))
        )
      }
      expect_rel_subset_single_exact_reassignment_success <- function(
        rel,
        ind,
        value
      ) {
        res <- rel
        eval(parse(text = paste0("res$", ind, " <- value")))
        is_valid_relation(res)
        if (ind %in% names(rel)) {
          negind <- setdiff(names(res), ind)
          expect_identical(res[negind], rel[negind])
          expect_identical(res[[ind]], setNames(value, ind))
        }else{
          expect_identical(res[names(rel)], rel)
          expect_identical(res[[ind]], setNames(value, ind))
        }
      }
      forall(
        gen.relation(letters[1:6], 1, 8) |>
          gen.and_then(gen.rel_single_exact_reassignment),
        \(rel, ind, value, error) {
          if (is.na(error)) {
            expect_rel_subset_single_exact_reassignment_success(rel, ind, value)
          }else{
            expect_error(
              eval(parse(text = paste0("rel$", ind, " <- value"))),
              paste0("^", error, "$")
            )
          }
        },
        curry = TRUE
      )
    })
  })

  it("is made unique to a valid relation", {
    forall(
      gen.relation(letters[1:6], 0, 8),
      unique %>>% is_valid_relation
    )
  })
  it("is made unique with no duplicate schemas", {
    forall(
      gen.relation(letters[1:6], 1, 8),
      \(rel) {
        rs2 <- c(rel, rel)
        expect_false(Negate(anyDuplicated)(rs2))
        expect_true(Negate(anyDuplicated)(unique(rs2)))
      }
    )
  })
  it("is made unique where tables with permuted rows count as duplicates", {
    rels <- relation(
      list(
        a = list(df = data.frame(a = c(T, F)), keys = list("a")),
        a.1 = list(df = data.frame(a = c(F, T)), keys = list("a"))
      ),
      "a"
    )
    expect_length(unique(rels), 1L)
  })

  it("concatenates to a valid relation schema", {
    forall(
      gen.relation(letters[1:6], from = 0, to = 4) |>
        gen.list(from = 1, to = 3),
      c %>>% with_args(is_valid_relation, single_empty_key = FALSE),
      curry = TRUE
    )
  })
  it("concatenates with duplicates preserved", {
    forall(
      gen.relation(letters[1:6], 1, 8) |>
        gen.with(\(rel) list(rel, rel)),
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
      gen.relation(letters[1:6], from = 0, to = 8) |>
        gen.list(from = 1, to = 10),
      concatenate_lossless_for_attrs_order,
      curry = TRUE
    )
  })
  it("concatenates without losing attribute orderings, if consistent", {
    concatenate_keeps_attribute_order <- function(...) {
      lst <- list(...)
      expect_silent(res <- c(...))
      for (index in seq_along(lst)) {
        expect_identical(
          attrs_order(lst[[!!index]]),
          intersect(attrs_order(res), attrs_order(lst[[!!index]]))
        )
      }
    }

    forall(
      gen.sample(letters[1:8], gen.element(1:3)) |>
        gen.with(
          sort %>>%
            with_args(relation, relations = setNames(list(), character()))
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
        gen.with(\(attrs) relation(setNames(list(), character()), attrs)) |>
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
      gen.relation(letters[1:6], from = 0, to = 8) |>
        gen.list(from = 1, to = 10),
      concatenate_lossless_for_schemas,
      curry = TRUE
    )
  })

  it("is composed of its records(), keys(), names(), and attrs_order()", {
    forall(
      gen.relation(letters[1:6], 0, 8),
      \(r) expect_identical(
        r,
        relation(
          setNames(
            Map(
              \(recs, ks) list(df = recs, keys = ks),
              records(r),
              keys(r)
            ),
            names(r)
          ),
          attrs_order = attrs_order(r)
        )
      )
    )
  })
  it("has record attributes given by attrs()", {
    forall(
      gen.relation(letters[1:6], 0, 8),
      \(r) expect_identical(
        attrs(r),
        lapply(records(r), names)
      )
    )
  })

  it("can have its attributes renamed", {
    forall(
      gen.relation(letters[1:6], 1, 8),
      function(rel) {
        rel2 <- rename_attrs(rel, toupper(attrs_order(rel)))
        expect_identical(
          rel2,
          relation(
            setNames(
              Map(
                \(recs, ks )list(df = recs, keys = ks),
                lapply(records(rel), \(df) `names<-`(df, toupper(names(df)))),
                lapply(keys(rel), lapply, toupper)
              ),
              names(rel)
            ),
            attrs_order = toupper(attrs_order(rel))
          )
        )
      }
    )
  })

  it("prints", {
    expect_output(
      print(relation(setNames(list(), character()), character())),
      "\\A0 relations\\n0 attributes\\Z",
      perl = TRUE
    )
    expect_output(
      print(relation(
        list(a = list(df = data.frame(a = logical(), b = logical()), keys = list("a"))),
        c("a", "b")
      )),
      paste0(
        "\\A",
        "1 relation",
        "\\n",
        "2 attributes: a, b",
        "\\n",
        "relation a: a, b; 0 records\\n  key 1: a",
        "\\Z"
      ),
      perl = TRUE
    )
    expect_output(
      print(relation(
        list(a = list(df = data.frame(a = FALSE, b = TRUE), keys = list("a"))),
        c("a", "b")
      )),
      paste0(
        "\\A",
        "1 relation",
        "\\n",
        "2 attributes: a, b",
        "\\n",
        "relation a: a, b; 1 record\\n  key 1: a",
        "\\Z"
      ),
      perl = TRUE
    )
  })
  it("can be added to a data frame as a column", {
    rel <- relation_schema(
      list(
        a_b = list(c("a", "b", "c"), list(c("a", "b"))),
        a = list(c("a", "d"), list("a"))
      ),
      letters[1:4]
    ) |>
      create()
    expect_no_error(tb <- data.frame(id = 1:2, relation = rel))
    expect_identical(tb$relation, rel)
  })
})
