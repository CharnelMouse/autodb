describe("attrs<-", {
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
    necessary <- unique(c(unlist(ks)))
    ref <- unique(c(referring, referred))
    list(
      necessary = necessary,
      ref = setdiff(ref, necessary),
      available = setdiff(attrs_order, c(necessary, ref))
    )
  }
  rel_selections <- function(ks, attrs, attrs_order) {
    necessary <- unique(unlist(ks))
    list(
      necessary = necessary,
      available = setdiff(attrs, necessary),
      banned = setdiff(attrs_order, attrs)
    )
  }
  db_selections <- function(ks, attrs, attrs_order, nm, refs) {
    necessary <- unique(unlist(ks))
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
    ref <- unique(c(referring, referred))
    list(
      necessary = necessary,
      ref = setdiff(ref, necessary),
      available = setdiff(attrs, c(necessary, ref)),
      banned = setdiff(attrs_order, attrs)
    )
  }

  gen.none <- function(x) gen.pure(x[FALSE])
  gen.strict_subsequence <- function(x) {
    if (length(x) == 0)
      stop("empty collections have no strict subsequences")
    inds <- seq_along(x)
    gen.element(inds) |>
      gen.and_then(\(remove) gen.subsequence(inds[-remove])) |>
      gen.with(\(keep) x[keep])
  }
  gen.single <- function(
    selections,
    necessary = gen.pure,
    ref = gen.pure,
    available = gen.subsequence,
    banned = gen.none
  ) {
    list(
      necessary(selections$necessary),
      ref(selections$ref),
      available(selections$available),
      banned(selections$banned)
    ) |>
      gen.with(unlist) |>
      gen.and_then(gen.sample)
  }
  gen.single_success <- function(selections) {
    gen.single(selections)
  }
  gen.single_failure_prime <- function(selections) {
    gen.single(selections, necessary = gen.strict_subsequence)
  }
  gen.single_failure_ref <- function(selections) {
    gen.single(selections, ref = gen.strict_subsequence)
  }
  gen.single_failure_add <- function(selections) {
    gen.single(selections, banned = gen.element)
  }

  gen.success <- function(selections) {
    lapply(selections, gen.single)
  }
  gen.failure <- function(selections, failable, gen) {
    list( # ensure at least one element
      gen.element(failable),
      gen.subsequence(failable)
    ) |>
      gen.with(uncurry(c) %>>% unique %>>% sort) |>
      gen.and_then(\(fail) {
        x <- rep(list(NULL), length(selections))
        x[-fail] <- lapply(selections[-fail], gen.single)
        x[fail] <- lapply(selections[fail], gen)
        x
      })
  }

  gen.attrs_assignment_from_selections <- function(x, selections) {
    failable_prime <- which(vapply(
      selections,
      \(sel) length(sel$necessary) > 0,
      logical(1)
    ))
    failable_ref <- which(vapply(
      selections,
      \(sel) length(sel$ref) > 0,
      logical(1)
    ))
    failable_add <- which(vapply(
      selections,
      \(sel) length(sel$banned) > 0,
      logical(1)
    ))
    choices <- c(
      list(
        list(
          gen.pure(x),
          gen.success(selections),
          gen.pure("success")
        )
      ),
      if (length(failable_prime) > 0)
        list(
          list(
            gen.pure(x),
            gen.failure(selections, failable_prime, gen.single_failure_prime),
            gen.pure("failure_prime")
          )
        ),
      if (length(failable_ref) > 0)
        list(
          list(
            gen.pure(x),
            gen.failure(selections, failable_ref, gen.single_failure_ref),
            gen.pure("failure_ref")
          )
        ),
      if (length(failable_add) > 0)
        list(
          list(
            gen.pure(x),
            gen.failure(selections, failable_add, gen.single_failure_add),
            gen.pure("failure_add")
          )
        )
    )
    do.call(gen.choice, choices)
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
    selections <- Map(
      with_args(rel_selections, attrs_order = attrs_order(rel)),
      keys(rel),
      attrs(rel)
    )
    gen.attrs_assignment_from_selections(rel, selections)
  }
  gen.db_attrs_assignment <- function(db) {
    selections <- Map(
      with_args(
        db_selections,
        refs = references(db),
        attrs_order = attrs_order(db)
      ),
      keys(db),
      attrs(db),
      names(db)
    )
    gen.attrs_assignment_from_selections(db, selections)
  }

  expect_attrs_assignment_success <- function(x, value, unaffected) {
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
    for (component in unaffected) {
      expect_identical(component(x2), component(x))
    }
  }
  expect_attrs_assignment_failure <- function(x, value, regexp = NULL) {
    x2 <- x
    expect_error(attrs(x2) <- value, regexp)
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
          unaffected = list(keys, attrs_order)
        ),
        failure_prime = expect_attrs_assignment_failure(
          rs,
          value,
          "^attrs reassignments must keep attributes used in keys$"
        )
      ),
      curry = TRUE
    )
  })
  it("works for database_schema: prime/reference attrs must be kept, can't add", {
    forall(
      # must include prime attrs and attrs in references, other attrs optional,
      # order irrelevant
      gen.database_schema(letters[1:6], 0, 8) |>
        gen.and_then(gen.ds_attrs_assignment),
      \(ds, value, case = c("success", "failure_prime", "failure_ref")) switch(
        match.arg(case),
        success = expect_attrs_assignment_success(
          ds,
          value,
          unaffected = list(keys, attrs_order, references)
        ),
        failure_prime = expect_attrs_assignment_failure(
          ds,
          value,
          "^attrs reassignments must keep attributes used in keys$"
        ),
        failure_ref = expect_attrs_assignment_failure(
          ds,
          value,
          "^attrs reassignments must keep attributes used in references$"
        )
      ),
      curry = TRUE
    )
  })
  it("works for relation: prime attrs must be kept", {
    forall(
      # must include prime attrs, other attrs optional, order irrelevant
      gen.relation(letters[1:6], 0, 8) |>
        gen.and_then(gen.rel_attrs_assignment),
      \(db, value, case = c("success", "failure_prime", "failure_add")) switch(
        match.arg(case),
        success = expect_attrs_assignment_success(
          db,
          value,
          unaffected = list(keys, attrs_order)
        ),
        failure_prime = expect_attrs_assignment_failure(
          db,
          value,
          "^record reassignments must keep key attributes$"
        ),
        failure_add = expect_attrs_assignment_failure(
          db,
          value,
          "^attrs reassignments for relational data objects can not add attributes$"
        )
      ),
      curry = TRUE
    )
  })
  it("works for database: prime and reference attrs must be kept, can't add", {
    forall(
      # must include prime attrs, other attrs optional, order irrelevant
      gen.database(letters[1:6], 0, 8) |>
        gen.and_then(gen.db_attrs_assignment),
      \(
        db,
        value,
        case = c("success", "failure_prime", "failure_ref", "failure_add")
      ) switch(
        match.arg(case),
        success = expect_attrs_assignment_success(
          db,
          value,
          unaffected = list(keys, attrs_order)
        ),
        failure_prime = expect_attrs_assignment_failure(
          db,
          value,
          "^record reassignments must keep key attributes$"
        ),
        failure_ref = expect_attrs_assignment_failure(
          db,
          value,
          "^attrs reassignments must keep attributes used in references$"
        ),
        failure_add = expect_attrs_assignment_failure(
          db,
          value,
          "^attrs reassignments for relational data objects can not add attributes$"
        )
      ),
      curry = TRUE
    )
  })
})

describe("keys<-", {
  candidates <- function(attrs) {
    if (length(attrs) == 0)
      return(data.frame(a = 1)[, FALSE, drop = FALSE])
    do.call(
      expand.grid,
      setNames(rep(list(c(FALSE, TRUE)), length(attrs)), attrs)
    )
  }
  to_sets <- function(arr) {
    if (nrow(arr) == 0) {
      list()
    }
    else
      unname(apply(
        arr,
        1,
        \(as) colnames(arr)[as],
        simplify = FALSE
      ))
  }
  assess_keys <- function(df) {
    sets <- candidates(names(df))
    is_superkey <- apply(
      sets,
      1,
      \(set) !df_anyDuplicated(df[, set, drop = FALSE])
    )
    list(
      valid = to_sets(sets[is_superkey, , drop = FALSE]),
      invalid = to_sets(sets[!is_superkey, , drop = FALSE])
    )
  }
  rs_selections <- function(attrs, attrs_order) {
    list(
      valid = to_sets(candidates(attrs)),
      banned = setdiff(attrs_order, attrs)
    )
  }
  ds_selections <- function(attrs, attrs_order, nm, refs) {
    referred_keys <- refs |>
      Filter(f = \(ref) ref[[3]] == nm) |>
      lapply(`[[`, 4)

    list(
      valid = to_sets(candidates(attrs)),
      necessary = unique(referred_keys),
      banned = setdiff(attrs_order, attrs)
    )
  }
  rel_selections <- function(df, attrs_order) {
    validity <- assess_keys(df)
    list(
      valid = validity$valid,
      invalid = validity$invalid,
      banned = setdiff(attrs_order, names(df))
    )
  }
  db_selections <- function(df, attrs_order, nm, refs) {
    validity <- assess_keys(df)
    referred_keys <- refs |>
      Filter(f = \(ref) ref[[3]] == nm) |>
      lapply(`[[`, 4)
    list(
      valid = validity$valid,
      necessary = unique(referred_keys),
      invalid = validity$invalid,
      banned = setdiff(attrs_order, names(df))
    )
  }

  gen.none <- function(x) gen.pure(x[FALSE])
  gen.single_success <- function(selections) {
    list(
      gen.pure(selections$valid),
      gen.none(selections$invalid)
    ) |>
      gen.with(with_args(unlist, recursive = FALSE)) |>
      gen.and_then(gen.element) |>
      gen.and_then(gen.sample) |>
      gen.list(
        from = length(selections$necessary) == 0,
        to = 5
      ) |>
      gen.with(\(lst) c(selections$necessary, lst)) |>
      gen.with(unique)
  }
  gen.single_failure_add <- function(selections) {
    # ensure at least one key with a banned attribute
    definitely_banned <- list(
      gen.element(selections$valid),
      gen.element(selections$banned)
    ) |>
      gen.with(unlist) |>
      gen.and_then(gen.sample) |>
      gen.list(from = 1, to = 5)
    others <- gen.single_success(selections)
    list(definitely_banned, others) |>
      gen.with(with_args(unlist, recursive = FALSE)) |>
      gen.with(unique)
  }
  gen.single_failure_ref <- function(selections) {
    gen.element(selections$necessary) |>
      gen.and_then(\(k) {
        selections_less <- selections
        selections_less$necessary <- setdiff(
          selections_less$necessary,
          list(k)
        )
        selections_less$valid <- setdiff(
          selections_less$valid,
          list(k)
        )
        gen.single_success(selections_less)
      })
  }
  gen.single_failure_invalid <- function(selections) {
    list(
      gen.single_success(selections),
      gen.element(selections$invalid) |>
        gen.list(from = 1, to = 5) |>
        gen.with(unique)
    ) |>
      gen.with(with_args(unlist, recursive = FALSE))
  }
  gen.success <- function(selections) {
    lapply(selections, gen.single_success)
  }
  gen.failure <- function(selections, failable, gen) {
    list( # ensure at least one element
      gen.element(failable),
      gen.subsequence(failable)
    ) |>
      gen.with(uncurry(c) %>>% unique %>>% sort) |>
      gen.and_then(\(fail) {
        x <- rep(list(NULL), length(selections))
        x[-fail] <- lapply(selections[-fail], gen.single_success)
        x[fail] <- lapply(selections[fail], gen)
        x
      })
  }
  gen.keys_assignment_from_selections <- function(
    x,
    selections,
    include_records = FALSE
  ) {
    failable_add <- which(vapply(
      selections,
      \(sel) length(sel$banned) > 0,
      logical(1)
    ))
    failable_ref <- which(vapply(
      selections,
      \(sel) length(sel$necessary) > length(sel$valid),
      logical(1)
    ))
    failable_invalid <- which(vapply(
      selections,
      \(sel) length(sel$invalid) > 0,
      logical(1)
    ))
    choices <- c(
      list(
        c(
          list(gen.pure(x)),
          if (include_records) list(gen.pure(records(x))),
          list(
            gen.success(selections),
            gen.pure("success")
          )
        )
      ),
      if (length(failable_add) > 0)
        list(
          c(
            list(gen.pure(x)),
            if (include_records) list(gen.pure(records(x))),
            list(
              gen.failure(selections, failable_add, gen.single_failure_add),
              gen.pure("failure_add")
            )
          )
        ),
      if (length(failable_ref) > 0)
        list(
          c(
            list(gen.pure(x)),
            if (include_records) list(gen.pure(records(x))),
            list(
              gen.failure(selections, failable_ref, gen.single_failure_ref),
              gen.pure("failure_ref")
            )
          )
        ),
      if (length(failable_invalid) > 0)
        list(
          c(
            list(gen.pure(x)),
            if (include_records) list(gen.pure(records(x))),
            list(
              gen.failure(selections, failable_invalid, gen.single_failure_invalid),
              gen.pure("failure_invalid")
            )
          )
        )
    )
    do.call(gen.choice, choices)
  }
  gen.rs_keys_assignment <- function(rs) {
    selections <- lapply(attrs(rs), rs_selections, attrs_order(rs))
    gen.keys_assignment_from_selections(rs, selections)
  }
  gen.ds_keys_assignment <- function(ds) {
    selections <- Map(
      with_args(
        ds_selections,
        attrs_order = attrs_order(ds),
        refs = references(ds)
      ),
      attrs(ds),
      names(ds)
    )
    gen.keys_assignment_from_selections(ds, selections)
  }
  gen.rel_keys_assignment <- function(rel) {
    selections <- lapply(records(rel), rel_selections, attrs_order(rel))
    gen.keys_assignment_from_selections(rel, selections, include_records = TRUE)
  }
  gen.db_keys_assignment <- function(db) {
    selections <- Map(
      with_args(
        db_selections,
        attrs_order = attrs_order(db),
        refs = references(db)
      ),
      records(db),
      names(db)
    )
    gen.keys_assignment_from_selections(db, selections, include_records = TRUE)
  }

  expect_keys_assignment_success <- function(x, value, unaffected) {
    x2 <- x
    keys(x2) <- value
    # it changes keys to value, sorted for length and attrs_order
    sorted_value <- lapply(
      value,
      \(ks) {
        sorted <- lapply(ks, \(k) k[order(match(k, attrs_order(x)))])
        indices <- lapply(sorted, match, attrs_order(x))
        ord <- keys_order(indices)
        unique(sorted[ord])
      }
    )
    expect_identical(unname(keys(x2)), unname(sorted_value))

    # rearranges attrs with new key order
    new_attrs <- Map(
      \(as, ks) {
        prime <- unique(unlist(ks))
        c(prime, setdiff(intersect(attrs_order(x), as), prime))
      },
      attrs(x),
      sorted_value
    )
    expect_identical(new_attrs, attrs(x2))

    # it doesn't affect other parts of the object
    for (component in unaffected) {
      expect_identical(component(x2), component(x))
    }
  }
  expect_keys_assignment_failure <- function(x, value, regexp = NULL) {
    x2 <- x
    expect_error(keys(x2) <- value, regexp)
  }

  sort_cols <- function(df) df[, sort(names(df)), drop = FALSE]
  sorted_record_cols <- records %>>% with_args(lapply, FUN = sort_cols)

  it("works for relation_schema", {
    forall(
      # must include prime attrs, other attrs optional, order irrelevant
      gen.relation_schema(letters[1:6], 0, 8) |>
        gen.and_then(gen.rs_keys_assignment),
      \(rs, value, case = c("success", "failure_add")) switch(
        match.arg(case),
        success = expect_keys_assignment_success(
          rs,
          value,
          unaffected = list(attrs_order)
        ),
        failure_add = expect_keys_assignment_failure(
          rs,
          value,
          "^attributes in keys must be present in relation"
        )
      ),
      curry = TRUE
    )
  })
  it("works for database_schema: must preserve referenced keys", {
    forall(
      # must include prime attrs, other attrs optional, order irrelevant
      gen.database_schema(letters[1:6], 0, 8) |>
        gen.and_then(gen.ds_keys_assignment),
      \(ds, value, case = c("success", "failure_add", "failure_ref")) switch(
        match.arg(case),
        success = expect_keys_assignment_success(
          ds,
          value,
          unaffected = list(attrs_order)
        ),
        failure_add = expect_keys_assignment_failure(
          ds,
          value,
          "^attributes in keys must be present in relation"
        ),
        failure_ref = expect_keys_assignment_failure(
          ds,
          value,
          "^reference attributes must be within referrer's attributes and referee's keys$"
        )
      ),
      curry = TRUE
    )
  })
  it("works for relation: keys must hold", {
    forall(
      gen.relation(letters[1:6], 0, 8) |>
        gen.and_then(gen.rel_keys_assignment),
      \(rel, recs, value, case = c("success", "failure_add", "failure_invalid")) switch(
        match.arg(case),
        success = expect_keys_assignment_success(
          rel,
          value,
          unaffected = list(
            attrs_order,
            sorted_record_cols
          )
        ),
        failure_add = expect_keys_assignment_failure(
          rel,
          value,
          "^relation keys must be within relation attributes"
        ),
        failure_invalid = expect_keys_assignment_failure(
          rel,
          value,
          "^relations must satisfy their keys"
        )
      ),
      curry = TRUE
    )
  })
  it("works for database: keys must hold, must preserve referenced keys", {
    forall(
      # must include prime attrs, other attrs optional, order irrelevant
      gen.database(letters[1:6], 0, 8) |>
        gen.and_then(gen.db_keys_assignment),
      \(db, recs, value, case = c("success", "failure_add", "failure_ref", "failure_invalid")) switch(
        match.arg(case),
        success = expect_keys_assignment_success(
          db,
          value,
          unaffected = list(
            attrs_order,
            sorted_record_cols
          )
        ),
        failure_add = expect_keys_assignment_failure(
          db,
          value,
          "^relation keys must be within relation attributes"
        ),
        failure_ref = expect_keys_assignment_failure(
          db,
          value,
          "^reference attributes must be within referrer's attributes and referee's keys"
        ),
        failure_invalid = expect_keys_assignment_failure(
          db,
          value,
          "^relations must satisfy their keys"
        )
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
  it("expects unique value column names", {
    rel <- create(relation_schema(list(a = list("a", list("a"))), "a"))
    vals <- data.frame(a = FALSE, a = TRUE, check.names = FALSE)
    expect_error(
      insert(rel, vals),
      "^duplicate column names in vals$"
    )
  })
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
    expect_identical_dataclass <- function(x, relnames, ...) {
      expect_biidentical(
        with_args(
          insert,
          vals = data.frame(setNames(
            lapply(attrs_order(x), \(x) logical()),
            attrs_order(x)
          )),
          relations = relnames
        ),
        with_args(
          rel2df %>>%
            \(rels) {
              nr <- vapply(records(rels)[relnames], nrow, integer(1))
              records(rels)[relnames][nr == 0] <- lapply(
                records(rels)[relnames][nr == 0],
                \(rel) {
                  rel[] <- lapply(rel, as.logical)
                  rel
                }
              )
              rels
            },
          relations = relnames
        )
      )(x)
    }
    forall(
      gen.relation(letters[1:4], 0L, 6L, rows_from = 1L) |>
        gen.and_then(\(rel) {
          list(
            gen.pure(rel),
            gen.subsequence(names(rel))
          )
        }),
      expect_identical_dataclass,
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
          \(recs) as.data.frame(df)[, names(recs), drop = FALSE]
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
            gen.subsequence(names(db)),
            records(db)
          )
        }),
      expect_identical_dataclass,
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
    deps <- discover(df)
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
    r <- decompose(df, normalise(discover(df)))
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
    r <- insert(create(synthesise(discover(df))), df)
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
  it("returns an error if missing attributes when all = TRUE", {
    rel <- relation(
      list(
        a = list(df = data.frame(a = logical(), b = logical()), keys = list("a")),
        b = list(df = data.frame(b = logical(), c = logical()), keys = list("b"))
      ),
      c("a", "b", "c")
    )
    expect_no_error(insert(rel, data.frame(a = logical(), b = logical(), c = logical()), all = TRUE))
    expect_error(
      insert(rel, data.frame(a = logical(), b = logical()), all = TRUE),
      "vals missing required attributes: c"
    )
    expect_no_error(insert(rel, data.frame(a = logical(), b = logical()), relations = "a", all = TRUE))
    expect_error(
      insert(rel, data.frame(a = logical()), all = TRUE),
      "vals missing required attributes: b"
    )

    db <- database(rel, list(list("a", "b", "b", "b")))
    expect_no_error(insert(db, data.frame(a = logical(), b = logical(), c = logical()), all = TRUE))
    expect_error(
      insert(db, data.frame(a = logical(), b = logical()), all = TRUE),
      "vals missing required attributes: c"
    )
    expect_no_error(insert(db, data.frame(a = logical(), b = logical()), relations = "a", all = TRUE))
    expect_error(
      insert(db, data.frame(a = logical()), all = TRUE),
      "vals missing required attributes: b"
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
    add_relevant_descendants <- function(nms, df2, as, relats) {
      # add relevant ancestors to ensure no reference violations
      get_new_parents <- function(nms) {
        # of rels we already include, we already use those whose attributes are
        # all in df2, since we're inserting that
        used_nms <- Filter(
          \(nm) all(is.element(as[[nm]], names(df2))),
          nms
        )
        # of all the references, interested in those with a child in the used
        # rels, and a parent whose attrs are all in df2
        valid_refs <- Filter(
          \(ref) {
            ref[[1]] %in% used_nms &&
              all(is.element(as[[ref[[3]]]], names(df2)))
          },
          relats
        ) |>
          vapply(\(ref) ref[[3]], character(1))
        # for those references, new parents are the parents that aren't already used
        parents <- setdiff(valid_refs, used_nms)
        parents
        # Currently df2 keeps all the attributes for insertion,
        # so most of these filtering conditions are redundant.
        # As such, the above is equivalent to
        # valid_refs <- Filter(
        #   \(ref) ref[[1]] %in% nms,
        #   relats
        # ) |>
        #   vapply(\(ref) ref[[3]], character(1))
        # or, once we have proper methods for references,
        # valid_refs <- parent(relats[child(relats) %in% nms])

        # in summary, we take the relations currently being inserted into,
        # and add any non-included children by foreign key references.
      }
      parents <- get_new_parents(nms)
      while (length(parents) > 0) {
        nms <- c(nms, parents)
        parents <- get_new_parents(parents)
        parents <- setdiff(parents, nms)
      }
      nms
    }
    process <- function(df, skp) {
      if (nrow(df) < 2)
        stop(print(df))
      inds <- seq_len(floor(nrow(df)/2))
      df1 <- df[inds, , drop = FALSE]
      df2 <- df[-inds, , drop = FALSE]
      stopifnot(
        nrow(df) >= 2,
        nrow(df1) >= 1,
        nrow(df2) >= 1
      )
      db_schema <- normalise(discover(df))
      rel_schema <- subschemas(db_schema)
      relats <- references(db_schema)
      rel <- create(rel_schema) |> insert(df1)
      list(rel, df1, df2, relats)
    }
    add.gen.insertees <- function(rel, df1, df2, relats) list(
      gen.pure(rel),
      gen.pure(df1),
      gen.pure(df2),
      gen.pure(relats),
      gen.subsequence(names(rel)) |>
        gen.with(with_args(
          add_relevant_descendants,
          df2 = df2,
          as = attrs(rel),
          relats = relats
        ))
    )
    gen.ex_from_table <- list(
      # mincol to give good chance of non-zero count for references
      gen_df(6, 7, minrow = 2, mincol = 5, remove_dup_rows = FALSE),
      gen.element(c(FALSE, TRUE))
    ) |>
      gen.with(uncurry(process)) |>
      gen.and_then(uncurry(add.gen.insertees))
    expect_both_valid_db_then <- function(fn) {
      function(x, y) {
        is_valid_database(x)
        is_valid_database(y)
        fn(x, y)
      }
    }
    forall(
      gen.ex_from_table,
      \(r, old_df, new_df, rels, relnames) {
        if (nrow(new_df) == 0L || length(rels) == 0L)
          discard()
        (
          biapply(
            with_args(database, references = rels) %>>%
              with_args(insert, vals = new_df, relations = relnames),
            with_args(insert, vals = new_df, relations = relnames) %>>%
              with_args(database, references = rels)
          ) %>>%
            (uncurry(expect_both_valid_db_then(expect_identical)))
        )(r)
      },
      discard.limit = 200,
      curry = TRUE
    )
  })
  it("can fail if inserting with different float precision", {
    x <- data.frame(
      a = c(0.12345678, 0.12345679),
      b = c(FALSE, TRUE)
    )
    rs <- relation_schema(
      list(
        constants = list("a", list(character())),
        b = list("b", list("b"))
      ),
      c("a", "b")
    )
    expect_error(insert(create(rs), x, digits = NA))
    expect_no_error(insert(create(rs), x, digits = 7))

    ds <- database_schema(rs, list())
    expect_error(insert(create(ds), x, digits = NA))
    expect_no_error(insert(create(ds), x, digits = 7))
  })
  it("can include row names", {
    x <- data.frame(a = rep(1, 9), row.names = letters[1:9])
    rel <- create(relation_schema(
      list(row = list(c("row", "a"), list("row"))),
      c("row", "a")
    ))
    db <- database(rel, list())
    rel <- insert(rel, x, keep_rownames = TRUE)
    db <- insert(db, x, keep_rownames = TRUE)
    expect_identical(nrow(records(rel)[[1]]), 9L)
    expect_identical(nrow(records(db)[[1]]), 9L)
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
    expect_error(`names<-`(rs, rep("a", 4)), "^relation schema names must be unique: duplicated a$")
    expect_error(`names<-`(r, rep("a", 4)), "^relation names must be unique: duplicated a$")
    expect_error(`names<-`(ds, rep("a", 4)), "^relation schema names must be unique: duplicated a$")
    expect_error(`names<-`(d, rep("a", 4)), "^relation names must be unique: duplicated a$")
  })
})

describe("merge_schemas", {
  it("removes schemas in to_remove, even if mentioned in merge_into", {
    rs <- relation_schema(
     list(
       a = list(c("a", "b"), list("a")),
       b = list(c("b", "c"), list("b")),
       b.1 = list(c("b", "d"), list("b")),
       d = list(c("d", "e"), list("d", "e"))
     ),
     letters[1:5]
    )
    ds <- database_schema(
     rs,
     list(
       list("a", "b", "b", "b"),
       list("b.1", "d", "d", "d")
      )
    )
    expect_identical(
      merge_schemas(rs, 3, 2),
      relation_schema(
        list(
          a = list(c("a", "b"), list("a")),
          b = list(c("b", "c", "d"), list("b")),
          d = list(c("d", "e"), list("d", "e"))
        ),
        letters[1:5]
      )
    )
    expect_identical(
      merge_schemas(ds, 3, 2),
      relation_schema(
        list(
          a = list(c("a", "b"), list("a")),
          b = list(c("b", "c", "d"), list("b")),
          d = list(c("d", "e"), list("d", "e"))
        ),
        letters[1:5]
      ) |>
        database_schema(
          list(
            list("a", "b", "b", "b"),
            list("b", "d", "d", "d")
          )
        )
    )
    expect_identical(
      merge_schemas(rs, 3, 3),
      rs[-3]
    )
    expect_identical(
      merge_schemas(ds, 3, 3),
      ds[-3]
    )
  })
})

describe("merge_relations", {
  it("removes relations in to_remove, even if mentioned in merge_into", {
    rel <- relation_schema(
      list(
        a = list(c("a", "b"), list("a")),
        b = list(c("b", "c"), list("b")),
        b.1 = list(c("b", "c"), list("b")),
        c = list(c("c", "d"), list("c", "d"))
      ),
      letters[1:4]
    ) |>
      create()
    db <- database(
      rel,
      list(
        list("a", "b", "b", "b"),
        list("b.1", "c", "c", "c")
      )
    )
    expect_identical(
      merge_relations(rel, 3, 2),
      relation_schema(
        list(
          a = list(c("a", "b"), list("a")),
          b = list(c("b", "c"), list("b")),
          c = list(c("c", "d"), list("c", "d"))
        ),
        letters[1:4]
      ) |>
        create()
    )
    expect_identical(
      merge_database_relations(db, 3, 2),
      relation_schema(
        list(
          a = list(c("a", "b"), list("a")),
          b = list(c("b", "c"), list("b")),
          c = list(c("c", "d"), list("c", "d"))
        ),
        letters[1:4]
      ) |>
        database_schema(
          list(
            list("a", "b", "b", "b"),
            list("b", "c", "c", "c")
          )
        ) |>
        create()
    )
    expect_identical(
      merge_relations(rel, 3, 3),
      rel[-3]
    )
    expect_identical(
      merge_database_relations(db, 3, 3),
      db[-3]
    )
  })
})

describe("add_lookup", {
  it("expects a single existing column name (WRT attrs_order and appearance)", {
    rs <- relation_schema(
      list(ab = list(c("a", "b"), list(c("a", "b")))),
      c("a", "b", "c")
    )
    ds <- database_schema(rs, list())
    rel <- create(rs)
    db <- create(ds)
    expect_no_error(add_lookup(rs, c("a", "b")))
    expect_no_error(add_lookup(ds, c("a", "b")))
    expect_no_error(add_lookup(rel, c("a", "b")))
    expect_no_error(add_lookup(db, c("a", "b")))
    expect_no_error(add_lookup(rs, "c"))
    expect_no_error(add_lookup(ds, "c"))
    expect_no_error(add_lookup(rel, "c"))
    expect_no_error(add_lookup(db, "c"))
    expect_error(
      add_lookup(rs, "d"),
      "^attribute d does not exist in x$"
    )
    expect_error(
      add_lookup(ds, "d"),
      "^attribute d does not exist in x$"
    )
    expect_error(
      add_lookup(rel, "d"),
      "^attribute d does not exist in x$"
    )
    expect_error(
      add_lookup(db, "d"),
      "^attribute d does not exist in x$"
    )
  })
  it(
    paste(
      "satisfies the following for each attribute:",
      "- leaves the original relations and references unchanged",
      "- is idempotent",
      "- if object has data:",
      "  - each attr is a key for a relation with all of its observed values",
      "  - value class is preserved if identical everywhere in input",
      "- if object has references:",
      "  - adds non-transitive references referring to new lookups",
      "  - results in a unique final-reference stop for each attribute",
      "  - has no original relation with attr and no outgoing reference using attr",
      "- adds no relations iff all attrs are keys",
      "- can add relations only for given attributes",
      sep = "\n"
    ),
    {
      x <- gen.choice(
        gen.relation_schema(letters[1:8], 0, 10),
        gen.database_schema(letters[1:8], 0, 10),
        gen.relation(letters[1:8], 0, 10),
        gen.database(letters[1:8], 0, 10)
      )
      forall(
        list(
          gen.sample(letters[1:8], gen.int(10), replace = TRUE) |>
            gen.shrink(shrinker = shrink.list),
          gen.with(
            x,
            \(x) list(
              x,
              if (inherits(x, c("relation", "database")))
                lapply(
                  records(x),
                  \(y) lapply(
                    y,
                    \(z) {
                      if (inherits(z, "numeric"))
                        format(z, digits = 21)
                      else
                        z
                    }
                  )
                )
            )
          )
        ),
        function(as, x) {
          x <- x[[1]]
          has_data <- inherits(x, c("relation", "database"))
          has_refs <- inherits(x, c("database_schema", "database"))
          same_relsch <- function(x, y) {
            identical(attrs(y[names(x)]), attrs(x)) &&
              identical(keys(y[names(x)]), keys(x))
          }
          same_rels <- function(x, y) {
            identical(records(y[names(x)]), records(x)) &&
              identical(keys(y[names(x)]), keys(x))
          }
          nondata_has_final <- function(y, nonkey_as) {
            vapply(
              nonkey_as,
              \(a) {
                has_a <- vapply(attrs(y), is.element, logical(1), el = a)
                refs_a <- references(y) |>
                  Filter(f = \(ref) is.element(a, ref[[2]])) |>
                  vapply(
                    \(ref) match(c(ref[[1]], ref[[3]]), names(y)[has_a]),
                    integer(2)
                  )
                mat_a <- matrix(FALSE, nrow = sum(has_a), ncol = sum(has_a))
                diag(mat_a) <- TRUE
                for (r in seq_len(ncol(refs_a)))
                  mat_a[refs_a[[1, r]], refs_a[[2, r]]] <- TRUE
                old <- mat_a
                new <- mat_a %*% mat_a
                new <- matrix(as.logical(new), nrow = nrow(mat_a), ncol = ncol(mat_a))
                while (!identical(new, old)) {
                  old <- new
                  new <- new %*% mat_a
                  new <- matrix(as.logical(new), nrow = nrow(mat_a), ncol = ncol(mat_a))
                }
                all(apply(new, 1, any))
              },
              logical(1)
            )
          }
          data_has_final <- function(y, as) {
            vapply(
              as,
              \(a) {
                has_a <- vapply(attrs(y), is.element, logical(1), el = a)
                refs_a <- references(y) |>
                  Filter(f = \(ref) is.element(a, ref[[2]])) |>
                  vapply(
                    \(ref) match(c(ref[[1]], ref[[3]]), names(y)[has_a]),
                    integer(2)
                  )
                mat_a <- matrix(FALSE, nrow = sum(has_a), ncol = sum(has_a))
                diag(mat_a) <- TRUE
                for (r in seq_len(ncol(refs_a)))
                  mat_a[refs_a[[1, r]], refs_a[[2, r]]] <- TRUE
                old <- mat_a
                new <- mat_a %*% mat_a
                new <- matrix(as.logical(new), nrow = nrow(mat_a), ncol = ncol(mat_a))
                while (!identical(new, old)) {
                  old <- new
                  new <- new %*% mat_a
                  new <- matrix(as.logical(new), nrow = nrow(mat_a), ncol = ncol(mat_a))
                }
                all(apply(new, 1, any))
              },
              logical(1)
            )
          }
          nondata_orphans <- function(x, y, nonkey_as) {
            vapply(
              nonkey_as,
              \(a) {
                attr_rels <- names(x)[vapply(attrs(x), is.element, logical(1), el = a)]
                !all(vapply(
                  attr_rels,
                  \(nm) any(vapply(
                    references(y),
                    \(ref) ref[[1]] == nm && is.element(a, ref[[2]]),
                    logical(1)
                  )),
                  logical(1)
                ))
              },
              logical(1)
            )
          }
          data_orphans <- function(x, y, nonkey_as) {
            vapply(
              nonkey_as,
              \(a) {
                orig_rels <- names(x)[vapply(attrs(x), is.element, logical(1), el = a)]
                all_rels <- names(y)[vapply(attrs(y), is.element, logical(1), el = a)]
                # if no new rels for a, then one rel is a lookup, else none are
                child <- vapply(
                  orig_rels,
                  \(nm) any(vapply(
                    references(y),
                    \(ref) ref[[1]] == nm && is.element(a, ref[[2]]),
                    logical(1)
                  )),
                  logical(1)
                )
                # if no new lookup, exactly one rel should have no parents
                sum(!child) != (length(all_rels) == length(orig_rels))
              },
              logical(1)
            )
          }

          msg <- character()

          as <- unique(as)
          y <- try(add_lookup(x, as), silent = TRUE)
          if (class(y)[[1]] == "try-error") {
            if (inherits(x, "database"))
              discard()
            else
              return(fail(paste(
                "unexpected error:",
                attr(y, "condition")$message
              )))
          }
          ks <- Reduce(c, keys(x), init = list())
          key_present <- vapply(
            as,
            \(a) any(vapply(ks, identical, logical(1), a)),
            logical(1)
          )
          nonkey_as <- as[!key_present]

          # leaves original relations/references unchanged
          if (!has_data && !same_relsch(x, y))
            msg <- c(msg, "original relations affected")
          if (has_data && !same_rels(x, y))
            msg <- c(msg, "original relations affected")
          if (
            has_refs &&
            any(!is.element(references(x), references(y[names(x)])))
          )
            msg <- c(msg, "original references affected")

          # idempotent: equivalence fine, but identical means no extra work
          z <- try(add_lookup(y, as), silent = TRUE)
          if (class(z)[[1]] == "try-error") {
            return(fail(paste(
              "unexpected error on second application:",
              attr(z, "condition")$message
            )))
          }
          if (!identical(z, y))
            msg <- c(
              msg,
              paste(
                "not idempotent:\n",
                records(z)[setdiff(names(z), names(y))] |>
                  vapply(
                    \(w) paste0(
                      "- ",
                      names(w)[[1]],
                      " = {",
                      toString(
                        if (inherits(w[[1]], "numeric"))
                          format(w[[1]], digits = 15)
                        else
                          w[[1]]
                      ),
                      "}"
                    ),
                    character(1)
                  ) |>
                  paste(collapse = "\n")
              )
            )

          # if object has data...
          if (has_data) {
            ## each attr is a key for a relation with all of its observed values
            key_cover <- vapply(
              as[key_present],
              \(a) {
                value_rels <- vapply(attrs(y), is.element, logical(1), el = a)
                key_rels <- vapply(
                  keys(y),
                  \(ks) any(vapply(ks, identical, logical(1), a)),
                  logical(1)
                )
                value_sets <- records(y)[value_rels] |>
                  lapply(\(x) unique(coarsen_if_float(x[[a]], getOption("digits"))))
                values <- Reduce(c, value_sets, init = logical()) |>
                  unique()
                any(lengths(value_sets) == length(values))
              },
              logical(1)
            )
            if (any(!key_cover))
              msg <- c(
                msg,
                paste(
                  by_number(sum(!key_cover), "attribute", "", "s"),
                  toString(as[!key_cover]),
                  by_number(sum(!key_cover), "ha", "s", "ve"),
                  "no all-values key relation"
                )
              )

            ## preserves class if unique original class vector
            original_classes <- lapply(
              as,
              \(a) {
                value_rels <- vapply(attrs(x), is.element, logical(1), el = a)
                value_classes <- records(x)[value_rels] |>
                  lapply(\(x) class(x[[a]])) |>
                  unique()
              }
            )
            classes <- lapply(
              as,
              \(a) {
                value_rels <- vapply(attrs(y), is.element, logical(1), el = a)
                value_classes <- records(y)[value_rels] |>
                  lapply(\(x) class(x[[a]])) |>
                  unique()
              }
            )
            new_class <- lengths(classes)[lengths(original_classes) == 1] != 1
            if (any(new_class))
              msg <- c(
                msg,
                paste(
                  by_number(sum(new_class), "attribute", "", "s"),
                  toString(as[new_class]),
                  by_number(sum(new_class), "ha", "s", "ve"),
                  "lookup with different value class"
                )
              )
          }

          # if object has references:
          # - adds non-transitive references referring to new lookups
          if (has_refs && !has_data) {
            ## for database_schema, don't choose which existing key relation
            ## to use as a lookup, so references only have new relations
            ## as parents
            new_refs <- setdiff(references(y), references(x))
            if (!all(vapply(
              new_refs,
              \(ref) all(vapply(ref[2:4], is.element, logical(1), make.unique_after(as, names(x)))),
              logical(1)
            )))
              msg <- c(msg, "there are new references not referring to the new lookups")
          }
          if (has_refs && has_data) {
            ## for database, existing key relations can be lookups, but
            ## new references should only refer to one relation per attribute,
            ## and that relation is new when one is available
            new_refs <- setdiff(references(y), references(x))
            new_attrs <- vapply(new_refs, `[[`, character(1), 2)
            new_parents <- vapply(new_refs, `[[`, character(1), 3)
            if (any(!is.element(new_attrs, as)))
              msg <- c(msg, "there are new references for attributes not in as")
            new_groups <- split(new_parents, new_attrs)[intersect(new_attrs, as)] |>
              lapply(unique)
            new_multirel <- lengths(new_groups) > 1
            if (any(new_multirel))
              msg <- c(
                msg,
                paste(
                  by_number(sum(new_multirel), "attribute", "", "s"),
                  toString(names(new_groups)[new_multirel]),
                  by_number(sum(new_multirel), "ha", "s", "ve"),
                  "multiple parents in new references"
                )
              )
          }

          # - results in a unique final-reference stop for each attribute
          if (has_refs && !has_data) {
            has_final <- nondata_has_final(y, as)
            if (any(!has_final))
              msg <- c(
                msg,
                paste(
                  by_number(sum(!has_final), "attribute", "", "s"),
                  toString(as[!has_final]),
                  by_number(sum(!has_final), "ha", "s", "ve"),
                  "no universal lookup table"
                )
              )
          }
          if (has_data && has_refs) {
            has_final <- data_has_final(y, as)
            if (any(!has_final))
              msg <- c(
                msg,
                paste(
                  by_number(sum(!has_final), "attribute", "", "s"),
                  toString(as[!has_final]),
                  by_number(sum(!has_final), "ha", "s", "ve"),
                  "no universal lookup table"
                )
              )
          }

          # - has no original non-lookup relation with attr and no outgoing reference using attr
          if (has_refs && !has_data) {
            orphans <- nondata_orphans(x, y, nonkey_as)
            if (any(orphans))
              msg <- c(
                msg,
                paste(
                  "non-key",
                  by_number(sum(orphans), "attribute", "", "s"),
                  toString(nonkey_as[orphans]),
                  by_number(sum(orphans), "ha", "s", "ve"),
                  "original relations with no outgoing reference"
                )
              )
          }
          if (has_data && has_refs) {
            orphans <- data_orphans(x, y, nonkey_as)
            if (any(orphans))
              msg <- c(
                msg,
                paste(
                  by_number(sum(orphans), "attribute", "", "s"),
                  toString(nonkey_as[orphans]),
                  by_number(sum(orphans), "ha", "s", "ve"),
                  "original non-lookup relations with no outgoing reference"
                )
              )
          }

          # adds no relations iff all attrs are keys
          if (all(key_present)) {
            if (!has_data && !same_relsch(x, y))
              msg <- c(msg, "all attrs already have keys, but relations changed")
            if (has_data && !same_rels(x, y))
              msg <- c(msg, "all attrs already have keys, but relations changed")
          }
          if (!all(key_present) && identical(y, x))
            msg <- c(msg, paste(toString(nonkey_as), "are not keys, but object not changed"))

          # can add relations only for given attributes
          extra <- setdiff(names(y), make.unique(c(names(x), as)))
          if (length(extra) > 0)
            msg <- c(
              msg,
              paste0(
                "unexpected relation ",
                by_number(length(extra), "name", "", "s"),
                ": ",
                toString(extra)
              )
            )

          if (length(msg) == 0)
            succeed()
          else
            fail(paste(msg, collapse = "\n"))
        }
      )
    })
})
