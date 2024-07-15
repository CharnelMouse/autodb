library(hedgehog)

describe("attrs<-", {
  gen.strict_subsequence <- function(x) {
    if (length(x) == 0)
      stop("empty collections have no strict subsequences")
    inds <- seq_along(x)
    gen.element(inds) |>
      gen.and_then(\(remove) gen.subsequence(inds[-remove])) |>
      gen.with(\(keep) x[keep])
  }

  gen.single <- function(selections, gen) {
    list(
      gen(selections$necessary),
      gen.subsequence(selections$available)
    ) |>
      gen.with(unlist) |>
      gen.and_then(gen.sample)
  }

  rs_selections <- function(ks, attrs_order) {
    necessary <- unique(unlist(ks))
    list(
      necessary = necessary,
      available = setdiff(attrs_order, necessary)
    )
  }
  ds_selections <- function(ks, attrs_order, nm, refs) {
    referring <- refs |>
      Filter(f = \(ref) ref[[1]] == nm) |>
      lapply(`[[`, 2) |>
      unlist() |>
      as.character()
    referred <- refs |>
      Filter(f = \(ref) ref[[3]] == nm) |>
      lapply(`[[`, 4) |>
      unlist() |>
      as.character()
    necessary <- unique(c(unlist(ks), referring, referred))
    list(
      necessary = necessary,
      available = setdiff(attrs_order, necessary)
    )
  }
  rel_selections <- function(ks, attrs) {
    necessary <- unique(unlist(ks))
    list(
      necessary = necessary,
      available = setdiff(attrs, necessary)
    )
  }

  gen.single_success <- function(selections) {
    gen.single(selections, gen.pure)
  }
  gen.single_failure_prime <- function(selections) {
    gen.single(selections, gen.strict_subsequence)
  }

  gen.success <- function(selections) {
    lapply(selections, gen.single_success)
  }
  gen.failure_prime <- function(selections, failable) {
    list( # ensure at least one element
      gen.element(failable),
      gen.subsequence(failable)
    ) |>
      gen.with(uncurry(c) %>>% unique %>>% sort) |>
      gen.and_then(\(fail) {
        x <- rep(list(NULL), length(selections))
        x[-fail] <- lapply(selections[-fail], gen.single_success)
        x[fail] <- lapply(selections[fail], gen.single_failure_prime)
        x
      })
  }

  gen.attrs_assignment_from_selections <- function(x, selections) {
    failable_prime <- which(vapply(
      selections,
      \(sel) length(sel$necessary) > 0,
      logical(1)
    ))
    if (length(failable_prime) == 0) {
      list(
        gen.pure(x),
        gen.success(selections),
        gen.pure("success")
      )
    }else{
      gen.choice(
        list(
          gen.pure(x),
          gen.success(selections),
          gen.pure("success")
        ),
        list(
          gen.pure(x),
          gen.failure_prime(selections, failable_prime),
          gen.pure("failure_prime")
        )
      )
    }
  }
  gen.rs_attrs_assignment <- function(rs) {
    selections <- lapply(keys(rs), rs_selections, attrs_order(rs))
    gen.attrs_assignment_from_selections(rs, selections)
  }
  gen.ds_attrs_assignment <- function(ds) {
    selections <- Map(
      with_args(
        ds_selections,
        refs = references(ds),
        attrs_order = attrs_order(ds)
      ),
      keys(ds),
      names(ds)
    )
    gen.attrs_assignment_from_selections(ds, selections)
  }
  gen.rel_attrs_assignment <- function(rel) {
    selections <- Map(rel_selections, keys(rel), attrs(rel))
    gen.attrs_assignment_from_selections(rel, selections)
  }

  expect_attrs_assignment_success <- function(x, value, other) {
    x2 <- x
    attrs(x2) <- value
    # it changes attrs to value, sorted for keys and attrs_order
    sorted_value <- Map(
      \(as, ks) {
        necessary <- unique(unlist(ks))
        c(
          necessary,
          intersect(setdiff(attrs_order(x), necessary), as)
        )
      },
      value,
      keys(x)
    )
    expect_identical(unname(attrs(x2)), unname(sorted_value))

    # it doesn't affect other parts of the object
    for (component in other) {
      expect_identical(component(x2), component(x))
    }
  }
  expect_attrs_assignment_failure <- function(x, value, regexp = NULL) {
    x2 <- x
    expect_error(attrs(x2) <- value, regexp)
  }
  expect_rs_attrs_failure_prime <- function(rs, value) {
    expect_attrs_assignment_failure(
      rs,
      value,
      "^attributes in keys must be present in relation$"
    )
  }
  expect_ds_attrs_assignment_failure <- function(ds, value) {
    expect_attrs_assignment_failure(
      ds,
      value
    )
  }
  expect_rel_attrs_failure_prime <- function(rel, value) {
    expect_attrs_assignment_failure(
      rel,
      value,
      "^record reassignments must keep key attributes$"
    )
  }

  it("works for relation_schema: prime attrs must be kept", {
    forall(
      # must include prime attrs, other attrs optional, order irrelevant
      gen.relation_schema(letters[1:6], 0, 8) |>
        gen.and_then(gen.rs_attrs_assignment),
      \(rs, value, case = c("success", "failure_prime")) switch(
        match.arg(case),
        success = expect_attrs_assignment_success(
          rs,
          value,
          list(keys, attrs_order)
        ),
        failure_prime = expect_rs_attrs_failure_prime(rs, value)
      ),
      curry = TRUE
    )
  })
  it("works for database_schema: prime/reference attrs must be kept", {
    forall(
      # must include prime attrs and attrs in references, other attrs optional,
      # order irrelevant
      gen.database_schema(letters[1:6], 0, 8) |>
        gen.and_then(gen.ds_attrs_assignment),
      \(ds, value, case = c("success", "failure_prime")) switch(
        match.arg(case),
        success = expect_attrs_assignment_success(
          ds,
          value,
          list(keys, attrs_order, references)
        ),
        failure_prime = expect_ds_attrs_assignment_failure(ds, value)
      ),
      curry = TRUE
    )
  })
  it("works for relation: prime attrs must be kept", {
    forall(
      # must include prime attrs, other attrs optional, order irrelevant
      gen.relation(letters[1:6], 0, 8) |>
        gen.and_then(gen.rel_attrs_assignment),
      \(rel, value, case = c("success", "failure_prime")) switch(
        match.arg(case),
        success = expect_attrs_assignment_success(
          rel,
          value,
          list(keys, attrs_order)
        ),
        failure_prime = expect_rel_attrs_failure_prime(rel, value)
      ),
      curry = TRUE
    )
  })
})

describe("create", {
  it("creates a valid structure", {
    forall(
      gen.relation_schema(letters[1:6], 0, 10),
      create %>>% is_valid_relation
    )
    forall(
      gen.database_schema(letters[1:6], 0, 10),
      create %>>% is_valid_database
    )
  })
  it("is commutative with adding foreign key constraints", {
    # need the same for create_insert and create %>>% insert once generating data
    forall(
      list(
        gen.relation_schema(letters[1:6], 0, 10),
        gen.element(c(FALSE, TRUE))
      ) |>
        gen.and_then(uncurry(\(rs, skp) {
          list(
            gen.pure(rs),
            gen.references(rs, skp)
          )
        })),
      \(rs, fks) {
        expect_biidentical(
          with_args(database_schema, references = fks) %>>% create,
          create %>>% with_args(database, references = fks)
        )(rs)
      },
      curry = TRUE
    )
  })
})

describe("insert", {
  it("expects relations to be unique elements", {
    rel <- create(relation_schema(list(a = list("a", list("a"))), "a"))
    expect_error(
      insert(rel, data.frame(a = FALSE), "b"),
      "^given relations must exist$"
    )
    expect_error(
      insert(rel, data.frame(a = FALSE), c("a", "a")),
      "^given relations must be unique$"
    )
  })
  it("does nothing when inserting nothing into nonempty relations, replaces into empty", {
    forall(
      gen.relation(letters[1:4], 0L, 6L, rows_from = 1L) |>
        gen.and_then(\(rel) {
          list(
            gen.pure(rel),
            gen.subsequence(names(rel))
          )
        }),
      \(r, relnames) {
        expect_biidentical(
          identity,
          with_args(
            insert,
            vals = data.frame(setNames(
              lapply(attrs_order(r), \(x) logical()),
              attrs_order(r)
            )),
            relations = relnames
          )
        )(r)
      },
      curry = TRUE
    )
    forall(
      gen.relation_schema(letters[1:4], 0L, 6L) |>
        gen.and_then(\(schema) {
          list(
            gen.pure(create(schema)),
            gen.subsequence(names(schema)),
            gen.attrs_class(attrs_order(schema)) |>
              gen.and_then(\(classes) {
                gen.df_fixed_ranges(
                  classes,
                  attrs_order(schema),
                  0L,
                  FALSE
                )
              })
          )
        }),
      \(rel, relnames, df) {
        expected <- rel
        records(expected)[relnames] <- lapply(
          records(expected)[relnames],
          \(recs) df[, names(recs), drop = FALSE]
        )
        expect_identical(insert(rel, df, relations = relnames), expected)
      },
      curry = TRUE
    )
    forall(
      gen.database(letters[1:6], 0L, 6L, rows_from = 1L) |>
        gen.and_then(\(db) {
          list(
            gen.pure(db),
            gen.subsequence(names(db))
          )
        }),
      \(db, relnames) expect_biidentical(
        identity,
        with_args(
          insert,
          vals = data.frame(setNames(
            lapply(attrs_order(db), \(x) logical()),
            attrs_order(db)
          )),
          relations = relnames
        )
      )(db),
      curry = TRUE
    )
    forall(
      gen.database_schema(letters[1:4], 0L, 6L) |>
        gen.and_then(\(schema) {
          gen.attrs_class(attrs_order(schema), references(schema)) |>
            gen.and_then(\(classes) list(
              gen.pure(create(schema)),
              gen.subsequence(names(schema)),
              gen.pure(classes),
              gen.df_fixed_ranges(
                classes,
                attrs_order(schema),
                0L,
                FALSE
              )
            ))
        }),
      \(db, relnames, classes, df) {
        expected <- db
        records(expected)[relnames] <- lapply(
          records(expected)[relnames],
          \(recs) df[, names(recs), drop = FALSE]
        )
        expect_identical(insert(db, df, relations = relnames), expected)
      },
      curry = TRUE
    )
  })
  it("returns an error when inserting key violations (i.e. same key, different record)", {
    df <- data.frame(a = 1:3, b = c(1:2, 1L), c = 1L)
    deps <- discover(df, 1)
    ds <- normalise(deps)
    db <- decompose(df, ds)
    dr <- subrelations(db)
    expect_error(
      insert(
        dr,
        data.frame(a = 1:2, b = 2:1)
      ),
      "^insertion violates key constraints in 1 relation: a$"
    )
    expect_error(
      insert(
        db,
        data.frame(a = 1:2, b = 2:1)
      ),
      "^insertion violates key constraints in 1 relation: a$"
    )
    expect_error(
      insert(
        dr,
        data.frame(a = 1:2, b = 2:1),
        relations = "a"
      ),
      "^insertion violates key constraints in 1 relation: a$"
    )
    expect_error(
      insert(
        db,
        data.frame(a = 1:2, b = 2:1),
        relations = "a"
      ),
      "^insertion violates key constraints in 1 relation: a$"
    )
  })
  it("returns an error if given extraneous attributes to inserted", {
    df <- data.frame(a = 1:3, b = c(1L, 1L, 2L))
    r <- decompose(df, normalise(discover(df, 1)))
    expect_error(
      insert(r, data.frame(a = 1L, b = 1L, c = 1L)),
      "^inserted attributes aren't included in target: c$"
    )
    db <- autodb(df)
    expect_error(
      insert(db, data.frame(a = 1L, b = 1L, c = 1L)),
      "^inserted attributes aren't included in target: c$"
    )
  })
  it("can insert only partial sets of attributes", {
    df <- data.frame(a = 1:4, b = c(1:3, 1L), c = c(1L, 1L, 2L, 1L))
    r <- insert(create(synthesise(discover(df, 1))), df)
    expect_identical(
      insert(r, data.frame(b = 4L, c = 3L)),
      relation(
        list(
          a = list(
            df = data.frame(a = 1:4, b = c(1:3, 1L)),
            keys = list("a")
          ),
          b = list(
            df = data.frame(b = 1:4, c = c(1L, 1L, 2L, 3L)),
            keys = list("b")
          )
        ),
        letters[1:3]
      )
    )
    db <- autodb(df)
    expect_identical(
      insert(db, data.frame(b = 4L, c = 3L)),
      database(
        relation(
          list(
            a = list(
              df = records(r)$a,
              keys = keys(r)$a
            ),
            b = list(
              df = data.frame(b = c(1:4), c = c(1L, 1L, 2L, 3L)),
              keys = list("b")
            )
          ),
          attrs_order(r)
        ),
        references(db)
      )
    )
  })
  it("returns an error when inserting foreign key violations", {
    df <- data.frame(a = 1:4, b = c(1:3, 1L), c = c(1L, 1L, 2L, 1L))
    expect_error(
      insert(
        autodb(df),
        data.frame(a = 5L, b = 4L)
      ),
      "^insertion violates 1 reference:\na.\\{b\\} -> b.\\{b\\}$"
    )
  })
  it("returns a valid object when given data that can be legally inserted", {
    forall(
      gen.relation(letters[1:4], 0, 6) |>
        gen.and_then(\(r) {
          list(
            gen.pure(r),
            gen.int(10) |>
              gen.and_then(with_args(
                gen.df_fixed_ranges,
                classes = rep("logical", length(attrs_order(r))),
                nms = attrs_order(r),
                remove_dup_rows = TRUE
              )) |>
              gen.with(with_args(remove_insertion_key_violations, relation = r)),
            gen.subsequence(names(r))
          )
        }),
      insert %>>% is_valid_relation,
      curry = TRUE
    )
    forall(
      # same_attr_name = TRUE very low high chance of FK violations
      # to be removed, but = FALSE is invalid for common table insertion
      gen.database(letters[1:4], 0, 6, same_attr_name = FALSE) |>
        gen.and_then(\(d) {
          list(
            gen.pure(d),
            gen.int(10) |>
              gen.and_then(with_args(
                gen.df_fixed_ranges,
                classes = rep("logical", length(attrs_order(d))),
                nms = attrs_order(d),
                remove_dup_rows = TRUE
              )) |>
              gen.with(with_args(remove_insertion_key_violations, relation = d)) |>
              gen.with(with_args(remove_insertion_reference_violations, database = d))
          )
        }) |>
        gen.and_then(\(lst) {
          list(
            gen.pure(lst[[1]]),
            gen.pure(lst[[2]]),
            minimal_legal_insertion_sets(lst[[1]], lst[[2]]) |>
              gen.subsequence() |>
              gen.with(unlist %>>% unique) |>
              gen.with(\(x) if (length(x) == 0) character() else x)
          )
        }),
      insert %>>% is_valid_database,
      curry = TRUE
    )
  })
  it("is commutative with adding foreign key constraints", {
    gen.ex_from_table <- list(
      # mincol to give good chance of non-zero count for references
      gen_df(6, 7, minrow = 2, mincol = 3, remove_dup_rows = FALSE),
      gen.element(c(FALSE, TRUE))
    ) |>
      gen.with(uncurry(\(df, skp) {
        if (nrow(df) < 2)
          stop(print(df))
        df1 <- df[seq_len(floor(nrow(df)/2)), , drop = FALSE]
        db_schema <- normalise(discover(df, 1))
        rel_schema <- subschemas(db_schema)
        df2 <- df[-seq_len(floor(nrow(df)/2)), , drop = FALSE]
        stopifnot(
          nrow(df) >= 2,
          nrow(df1) >= 1,
          nrow(df2) >= 1
        )
        relats <- references(db_schema)
        rel <- create(rel_schema) |> insert(df1)
        list(rel, df1, df2, relats)
      })) |>
      gen.and_then(uncurry(\(rel, df1, df2, relats) list(
        gen.pure(rel),
        gen.pure(df1),
        gen.pure(df2),
        gen.pure(relats),
        gen.subsequence(names(rel)) |>
          gen.with(\(nms) {
            # add relevant ancestors to ensure no reference violations
            get_new_parents <- function(nms) {
              as <- attrs(rel)
              used_nms <- nms[vapply(
                nms,
                \(nm) all(is.element(as[[nm]], names(df2))),
                logical(1)
              )]
              valid_refs <- Filter(
                \(ref) {
                  ref[[1]] %in% used_nms &&
                    all(is.element(attrs(rel)[[ref[[3]]]], names(df2)))
                },
                relats
              ) |>
                vapply(\(ref) ref[[3]], character(1))
              parents <- setdiff(valid_refs, used_nms)
              parents
            }
            parents <- get_new_parents(nms)
            while (length(parents) > 0) {
              nms <- c(nms, parents)
              parents <- get_new_parents(parents)
              parents <- setdiff(parents, nms)
            }
            nms
          })
      )))
    gen.ex <- list(
      gen_df(6, 7, minrow = 2),
      gen.relation(letters[1:4], 0, 6, rows_from = 1L, rows_to = 1L),
      gen.element(c(FALSE, TRUE))
    ) |>
      gen.and_then(uncurry(\(r, skp) {
        list(
          gen.pure(r),
          gen.element(40:50) |>
            gen.and_then(with_args(
              gen.df_fixed_ranges,
              classes = rep("logical", length(attrs_order(r))),
              nms = attrs_order(r),
              remove_dup_rows = TRUE
            )) |>
            gen.with(with_args(remove_insertion_key_violations, relation = r)),
          gen.references(r, skp) |>
            gen.with(with_args(remove_violated_references, relation = r))
        ) |>
          gen.with(uncurry(\(r, df, rels) {
            changed <- TRUE
            n <- 0L
            while (changed) {
              new_df <- remove_insertion_reference_violations(
                df,
                database(r, rels)
              )
              new_rels <- remove_violated_references(
                rels,
                insert(r, new_df)
              )
              changed <- !identical(new_df, df) || !identical(new_rels, rels)
              df <- new_df
              rels <- new_rels
            }
            list(r, df, rels)
          }))
      }))
    expect_both_valid_db_then <- function(fn) {
      function(x, y) {
        is_valid_database(x)
        is_valid_database(y)
        fn(x, y)
      }
    }
    forall(
      gen.ex_from_table,
      \(r, old_df, df, rels, relnames) {
        if (nrow(df) == 0L || length(rels) == 0L)
          discard()
        (
          biapply(
            with_args(database, references = rels) %>>%
              with_args(insert, vals = df, relations = relnames),
            with_args(insert, vals = df, relations = relnames) %>>%
              with_args(database, references = rels)
          ) %>>%
            (uncurry(expect_both_valid_db_then(expect_identical)))
        )(r)
      },
      curry = TRUE,
      tests = 300,
      discard.limit = 210
    )
  })
})

describe("subrelations", {
  it("returns a valid relation for database", {
    forall(
      gen.database(letters[1:6], 0, 6),
      subrelations %>>% is_valid_relation
    )
  })
  it("returns a valid relation_schema for database_schema", {
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.and_then(with_args(
          gen.database_schema,
          x = letters[1:6],
          from = 0,
          to = 6
        )),
      subschemas %>>% is_valid_relation_schema
    )
  })
})

describe("names<-", {
  it("requires unique names for relation schemas, relations, etc.", {
    rs <- relation_schema(
      list(
        a = list(c("a", "b"), list("a")),
        b = list(c("b", "c", "d"), list("b"))
      ),
      letters[1:4]
    )
    ds <- autoref(rs)
    r <- create(rs)
    d <- create(ds)
    expect_error(`names<-`(rs, rep("a", 4)), "^relation schema names must be unique$")
    expect_error(`names<-`(r, rep("a", 4)), "^relation names must be unique$")
    expect_error(`names<-`(ds, rep("a", 4)), "^relation schema names must be unique$")
    expect_error(`names<-`(d, rep("a", 4)), "^relation names must be unique$")
  })
})
