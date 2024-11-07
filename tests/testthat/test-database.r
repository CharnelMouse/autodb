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
  it("expects valid input: reference elements are length-four lists", {
    expect_error(
      database(create(empty_rs), list("a")),
      "^reference elements must be length-four lists: element 1$"
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
      "^reference elements must be length-four lists: element 1$"
    )
    expect_error(
      database(rs, list(as.list(paste0("r", 1:3)))),
      "^reference elements must be length-four lists: element 1$"
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
      "^relation names must be unique: duplicated a$"
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
      "^reference relation names must be within relation names: absent r4$"
    )
    expect_error(
      database(rs, list(list("r4", "b", "r3", "b"))),
      "^reference relation names must be within relation names: absent r4$"
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
      "^reference attributes must be within referrer's attributes and referee's keys: reference 1$"
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

  it("handles merge duplicates due to floating-point when checking references", {
    x <- data.frame(
      a = c(F, F, F, F, F, T, T),
      b = c(
        2.69353840461766669279,
        2.69353840461766713688,
        2.69353840461766713688,
        3.74416921885076714460,
        5.50801230419353693435,
        3.72990161259524111159,
        5.50801230419353693435
      ),
      c = c(
        -3.19131542233864262670,
        -2.87397721325655908231,
        -1.98786466514381765514,
        -3.33190719804050772268,
        -3.33190719804050772268,
        -3.33190719804050772268,
        -3.33190719804050772268
      ),
      d = c(
        2.56310969849146363941,
        2.56310969849146363941,
        2.56310969849146363941,
        2.94505568569789533129,
        4.88640238864414033770,
        2.94505568569789533129,
        4.88640238864414211406
      )
    )
    # FDs: acd -> b, ab -> d, bd -> a
    # 3NF schema: abcd[acd].{ab} -> ab[ab,bd].{ab}
    ds <- discover(x, 1) |>
      normalise(remove_avoidable = TRUE)
    rel <- subschemas(ds) |> create() |> insert(x)
    refs <- references(ds)
    stopifnot(identical(
      refs,
      list(
        list("a_c_d", c("a", "b"), "a_b", c("a", "b")),
        list("a_c_d", c("b", "d"), "a_b", c("b", "d"))
      )
    ))
    referrer <- records(rel)$a_c_d
    referee <- records(rel)$a_b
    check <- df_join(referrer, referee, by = c("a", "b"))
    expect_identical(nrow(referrer), 7L)
    expect_identical(nrow(referee), 6L)
    expect_identical(nrow(check), 10L)
    expect_identical(nrow(unique(check)), 7L)
    expect_silent(db <- database(rel, refs))
    y <- rejoin(db)
    expect_true(df_equiv(y, x))
  })

  it("expects record reassignments to have all prime attributes, maybe others, order-independent", {
    x <- database(
      relation(
        list(a = list(df = data.frame(a = 1:4, b = 1:2), keys = list("a"))),
        attrs_order = c("a", "b")
      ),
      list()
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
    x2 <- database(
      relation(
        list(a = list(df = data.frame(b = 1:4, a = 1:2), keys = list("b"))),
        attrs_order = c("a", "b")
      ),
      list()
    )
    y2 <- x2
    expect_silent(records(y2) <- list(a = data.frame(a = 1:2, b = 1:4)))
    expect_identical(y2, x2)
  })
  it("expects records reassignments to have unique attribute names", {
    x <- database(
      relation(
        list(a = list(df = data.frame(a = 1:4, b = 1:2), keys = list("a"))),
        attrs_order = c("a", "b")
      ),
      list()
    )
    expect_error(
      records(x)[[1]] <- data.frame(a = 1:4, a = 1:2, check.names = FALSE)
    )
  })
  it("expects records name reassignments to result in an error or a valid database", {
    forall(
      gen.database(letters[1:6], 1, 8) |>
        gen.and_then(\(db) {
          nonempty <- which(lengths(attrs(db)) > 0)
          if (length(nonempty) == 0)
            return(list(
              gen.pure(db),
              gen.pure(1L),
              gen.pure(attrs(db)[[1]])
            ))
          gen.element(nonempty) |>
            gen.and_then(\(n) {
              list(
                gen.pure(db),
                gen.pure(n),
                gen.sample_resampleable(
                  attrs(db)[[n]],
                  to = length(attrs(db)[[n]])
                )
              )
            })
        }),
      \(db, n, nm) {
        res <- try(names(records(db)[[n]]) <- nm, silent = TRUE)
        expect_true(
          class(res)[[1]] == "try-error" ||
            class(try(is_valid_database(db), silent = TRUE))[[1]] != "try-error"
        )
      },
      curry = TRUE
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

  it("expects a database value for subset re-assignment", {
    db <- create(database_schema(
      relation_schema(
        list(X = list(character(), list(character()))),
        letters[1:6]
      ),
      list()
    ))
    expect_error(db[1] <- 1L, "^value must also be a database object$")
    expect_error(db[[1]] <- 1L, "^value must also be a database object$")
    expect_error(db$X <- 1L, "^value must also be a database object$")
  })
  describe("can have subsets re-assigned, without changing relation names", {
    it("[<-", {
      gen.db_reassignment_indices_format <- function(db, subseq) {
        choices <- c(
          list(gen.pure(subseq)),
          if (length(subseq) < length(db))
            list(gen.pure(-setdiff(seq_along(db), subseq))),
          list(gen.pure(names(db)[subseq])),
          list(seq_along(db) %in% subseq)
        )
        weights <- rep(1L, 3L + (length(subseq) < length(db)))
        do.call(gen.choice, c(choices, list(prob = weights)))
      }
      gen.db_reassignment <- function(db) {
        gen.subsequence(seq_along(db)) |>
          gen.and_then(\(subseq) {
            gen.db_reassignment_indices_format(db, subseq) |>
              gen.and_then(\(inds) {
                gen.database(letters[1:6], length(subseq), length(subseq)) |>
                  gen.with(\(rs2) {
                    list(db, inds, rs2)
                  })
              })
          })
      }
      expect_db_subset_reassignment_success <- function(db, indices, value) {
        res <- db
        res[indices] <- value
        is_valid_database(res)
        switch(
          class(indices),
          character = {
            negind <- setdiff(names(res), indices)
            expect_identical(res[negind], db[negind])
            expect_identical(res[indices], setNames(value, indices))
          },
          integer = {
            negind <- if (length(indices) == 0)
              seq_along(db)
            else
              -indices
            expect_identical(res[negind], db[negind])
            expect_identical(res[indices], setNames(value, names(db)[indices]))
          },
          logical = {
            expect_identical(res[!indices], db[!indices])
            expect_identical(res[indices], setNames(value, names(db)[indices]))
          }
        )
      }
      forall(
        gen.database(letters[1:6], 0, 8) |>
          gen.and_then(gen.db_reassignment),
        expect_db_subset_reassignment_success,
        curry = TRUE
      )
    })
    it("[[<-", {
      gen.db_single_reassignment_indices_format <- function(db, subseq) {
        choices <- c(
          list(gen.pure(subseq)),
          if (length(db) == 2)
            list(gen.pure(-setdiff(seq_along(db), subseq))),
          list(gen.pure(names(db)[subseq])),
          if (length(db) == 1)
            list(gen.pure(seq_along(db) %in% subseq))
        )
        weights <- rep(
          1L,
          2L + (length(db) == 2) + (length(db) == 1)
        )
        do.call(gen.choice, c(choices, list(prob = weights)))
      }
      gen.db_single_reassignment_success <- function(db) {
        list(
          gen.pure(db),
          gen.element(seq_along(db)) |>
            gen.and_then(\(subseq) {
              gen.db_single_reassignment_indices_format(db, subseq)
            }),
          gen.database(letters[1:6], 1, 1),
          gen.pure(NA_character_)
        )
      }
      gen.db_single_reassignment_failure_emptyint <- function(db) {
        list(
          gen.pure(db),
          gen.db_single_reassignment_indices_format(db, integer()),
          gen.database(letters[1:6], 0, 0)
        ) |>
          gen.with(\(lst) {
            c(
              lst,
              list(single_subset_failure_type(db, lst[[2]]))
            )
          })
      }
      gen.db_single_reassignment_failure_multiint <- function(db) {
        list(
          gen.sample(seq_along(db), 2, replace = FALSE),
          gen.subsequence(seq_along(db))
        ) |>
          gen.with(unlist %>>% unique %>>% sort) |>
          gen.and_then(\(subseq) {
            gen.db_single_reassignment_indices_format(db, subseq) |>
              gen.and_then(\(indices) {
                gen.database_schema(letters[1:6], length(subseq), length(subseq)) |>
                  gen.with(\(rs2) {
                    list(
                      db,
                      indices,
                      rs2,
                      single_subset_failure_type(db, indices)
                    )
                  })
              })
          })
      }
      gen.db_single_reassignment <- function(db) {
        choices <- c(
          list(gen.db_single_reassignment_success(db)),
          list(gen.db_single_reassignment_failure_emptyint(db)),
          if (length(db) > 1) list(gen.db_single_reassignment_failure_multiint(db))
        )
        weights <- c(70, 15, if (length(db) > 1) 15)
        do.call(
          gen.choice,
          c(choices, list(prob = weights))
        )
      }
      expect_db_subset_single_reassignment_success <- function(db, ind, value) {
        res <- db
        res[[ind]] <- value
        is_valid_database(res)
        switch(
          class(ind),
          character = {
            negind <- setdiff(names(res), ind)
            expect_identical(res[negind], db[negind])
            expect_identical(res[[ind]], setNames(value, ind))
          },
          integer = {
            expect_identical(res[-ind], db[-ind])
            expect_identical(res[[ind]], setNames(value, names(db)[[ind]]))
          },
          logical = {
            expect_identical(res[!ind], db[!ind])
            expect_identical(res[[ind]], setNames(value, names(db)[[ind]]))
          }
        )
      }
      forall(
        gen.database(letters[1:6], 1, 8) |>
          gen.and_then(gen.db_single_reassignment),
        \(db, ind, value, error) {
          if (is.na(error)) {
            expect_db_subset_single_reassignment_success(db, ind, value)
          }else{
            expect_error(
              db[[ind]] <- value,
              paste0("^", error, "$")
            )
          }
        },
        curry = TRUE
      )
    })
    it("$<-", {
      gen.db_single_exact_reassignment_success_change <- function(db) {
        list(
          gen.pure(db),
          gen.element(seq_along(db)) |>
            gen.with(\(subseq) names(db)[[subseq]]),
          gen.database(letters[1:6], 1, 1),
          gen.pure(NA_character_)
        )
      }
      gen.db_single_exact_reassignment_success_add <- function(db) {
        list(
          gen.pure(db),
          gen.element(setdiff(letters, names(db))),
          gen.database(letters[1:6], 1, 1),
          gen.pure(NA_character_)
        )
      }
      gen.db_single_exact_reassignment_failure <- function(db) {
        gen.int(1) |>
          gen.and_then(\(n) {
            list(
              gen.pure(db),
              gen.pure(n),
              gen.database(letters[1:6], 1, 1),
              gen.pure(paste0(
                "<text>:1:4: unexpected numeric constant",
                "\n",
                "1: db\\$", n,
                "\n",
                "       \\^"
              ))
            )
          })
      }
      gen.db_single_exact_reassignment <- function(db) {
        choices <- c(
          list(gen.db_single_exact_reassignment_success_change(db)),
          list(gen.db_single_exact_reassignment_success_add(db)),
          list(gen.db_single_exact_reassignment_failure(db))
        )
        weights <- c(40, 40, 20)
        do.call(
          gen.choice,
          c(choices, list(prob = weights))
        )
      }
      expect_db_subset_single_exact_reassignment_success <- function(db, ind, value) {
        res <- db
        eval(parse(text = paste0("res$", ind, " <- value")))
        is_valid_database(res)
        if (ind %in% names(db)) {
          negind <- setdiff(names(res), ind)
          expect_identical(res[negind], db[negind])
          expect_identical(res[[ind]], setNames(value, ind))
        }else{
          expect_identical(res[names(db)], db)
          expect_identical(res[[ind]], setNames(value, ind))
        }
      }
      forall(
        gen.database(letters[1:6], 1, 8) |>
          gen.and_then(gen.db_single_exact_reassignment),
        \(db, ind, value, error) {
          if (is.na(error)) {
            expect_db_subset_single_exact_reassignment_success(db, ind, value)
          }else{
            expect_error(
              eval(parse(text = paste0("db$", ind, " <- value"))),
              paste0("^", error, "$")
            )
          }
        },
        curry = TRUE
      )
    })
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
        "database with 0 relations",
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
        list(list("a", "b", "b", "b"))
      )),
      paste0(
        "\\A",
        "database with 2 relations",
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
        list(list("a", c("b", "c"), "b", c("b", "c")))
      )),
      paste0(
        "\\A",
        "database with 2 relations",
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
  it("can be added to a data frame as a column", {
    db <- relation_schema(
      list(
        a_b = list(c("a", "b", "c"), list(c("a", "b"))),
        a = list(c("a", "d"), list("a"))
      ),
      letters[1:4]
    ) |>
      database_schema(list()) |>
      create()
    expect_no_error(tb <- data.frame(id = 1:2, relation = db))
    expect_identical(tb$relation, db)
  })
})
