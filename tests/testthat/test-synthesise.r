library(hedgehog)

gen.nonempty_key <- gen.sample.int(2, gen.int(5), replace = TRUE)
gen.key <- gen.sample.int(2, gen.sample(0:5, 1), replace = TRUE)

describe("synthesise", {
  expect_relation_schema <- function(current, target) {
    expect_identical(
      current,
      relation_schema(
        setNames(Map(list, target$attrs, target$keys), target$relation_names),
        target$all_attrs
      )
    )
  }
  gets_unique_table_names <- function(fds) {
    expect_true(!anyDuplicated(names(synthesise(fds))))
  }

  it("gives valid schemas", {
    # rare failing case:
    # removing avoidables can muck up attrs order
    # if keys aren't reordered afterwards
    deps <- functional_dependency(
      list(
        list(c("bcr_j", "vfzc."), "mdaoyx"),
        list("vfzc.", "bcr_j"),
        list(c("mdaoyx", "bcr_j", "bi", "vfzc."), "fvoxk"),
        list(c("mdaoyx", "bcr_j", "eqro", "vfzc."), "fvoxk"),
        list(c("mdaoyx", "eqro", "bi"), "fvoxk"),
        list(c("mdaoyx", "vfzc."), "fvoxk"),
        list(c("mdaoyx", "bcr_j", "eqro", "fvoxk", "vfzc."), "bi"),
        list(c("mdaoyx", "eqro", "bi"), "vfzc."),
        list(c("bcr_j", "eqro", "fvoxk"), "vfzc."),
        list(c("mdaoyx", "bcr_j", "fvoxk", "bi"), "vfzc."),
        list(c("bcr_j", "fvoxk", "bi"), "vfzc.")
      ),
      c("mdaoyx", "bcr_j", "eqro", "fvoxk", "bi", "vfzc.")
    )
    deps |>
      (apply_both(
        synthesise %>>% is_valid_relation_schema,
        with_args(synthesise, remove_avoidable = TRUE) %>>%
          is_valid_relation_schema
      ))()

    forall(
      gen_flat_deps(7, 20, to = 20L),
      apply_both(
        synthesise %>>% is_valid_relation_schema,
        with_args(synthesise, remove_avoidable = TRUE) %>>%
          is_valid_relation_schema
      )
    )
  })
  it("is invariant to dependency reordering", {
    gen_permutation <- gen.int(10) |>
      gen.and_then(\(n) list(
        gen_named_flat_deps_fixed_size(letters[1:10], n, 5, unique = FALSE),
        gen.sample.int(n),
        n
      )) |>
      gen.with(\(lst) {
        if (length(lst[[1]]) != length(lst[[2]]))
          stop(print(lst))
        lst[1:2]
      }) |>
      gen.with(\(lst) {
        list(lst[[1]], lst[[1]][lst[[2]]])
      })
    normalisation_permutation_invariant <- if_discard_else(
      uncurry(identical),
      with_args(lapply, synthesise) %>>% (uncurry(expect_identical))
    )

    # currently-rarely-generated case:
    # given a choice of which dependency to remove as extraneous,
    # order matters
    deps <- functional_dependency(
      list(
        list(c("C", "D"), "B"),
        list(c("A", "B"), "C"),
        list(c("A", "D"), "B"),
        list(c("A", "D"), "C")
      ),
      attrs = c("A", "B", "C", "D")
    )
    normalisation_permutation_invariant(list(deps, deps[c(1, 2, 4, 3)]))

    forall(gen_permutation, normalisation_permutation_invariant)
  })
  it("removes longer/later-attributed dependency sets if given a choce", {
    schema <- synthesise(functional_dependency(
      list(
        list(c("C", "D"), "B"),
        list(c("A", "B"), "C"),
        list(c("A", "D"), "B"),
        list(c("A", "D"), "C")
      ),
      attrs = c("A", "B", "C", "D")
    ))
    expect_setequal(names(schema), c("A_B", "A_D", "C_D"))
    ord <- match("A_D", names(schema))
    expect_identical(setdiff(attrs(schema)[[ord]], c("A", "D")), "B")
  })
  it("removes extraneous attributes", {
    dependencies <- functional_dependency(
      list(
        list("a", "b"),
        list(c("a", "b"), "c")
      ),
      attrs = c("a", "b", "c")
    )
    norm.dependencies <- synthesise(dependencies)
    expect_relation_schema(
      norm.dependencies,
      list(
        attrs = list(c("a", "b", "c")),
        keys = list(list("a")),
        all_attrs = c("a", "b", "c"),
        relation_names = "a"
      )
    )
  })
  it("removes extraneous dependencies", {
    dependencies <- functional_dependency(
      list(
        list("a", "b"),
        list("a", "c"),
        list("b", "c")
      ),
      attrs = c("a", "b", "c")
    )
    norm.dependencies <- synthesise(dependencies)
    expect_relation_schema(
      norm.dependencies,
      list(
        attrs = list(c("a", "b"), c("b", "c")),
        keys = list(list("a"), list("b")),
        all_attrs = c("a", "b", "c"),
        relation_names = c("a", "b")
      )
    )
  })
  it("merges equivalent keys", {
    dependencies <- functional_dependency(
      list(
        list("d", "a"),
        list("d", "b"),
        list("a", "d"),
        list(c("b", "c"), "d")
      ),
      attrs = c("a", "b", "c", "d")
    )
    norm.dependencies <- synthesise(dependencies)
    expect_relation_schema(
      norm.dependencies,
      list(
        attrs = list(c("a", "d", "b"), c("b", "c", "a")),
        keys = list(list("a", "d"), list(c("b", "c"))),
        all_attrs = c("a", "b", "c", "d"),
        relation_names = c("a", "b_c")
      )
    )
  })
  it("resolves simple bijections with no splits", {
    # A -> B, B -> A => A <-> B
    dependencies <- functional_dependency(
      list(
        list("a", "b"),
        list("b", "a")
      ),
      attrs = c("a", "b")
    )
    norm.df <- synthesise(dependencies)
    expect_relation_schema(
      norm.df,
      list(
        attrs = list(c("a", "b")),
        keys = list(list("a", "b")),
        all_attrs = c("a", "b"),
        relation_names = "a"
      )
    )
  })
  it("can handle basic bijections with dependents", {
    # A -> B, B -> A, A -> C, B -> C, A -> D, B -> F, D -> E, F -> E
    # => A <-> B, A -> CDF, D -> E, F -> E
    dependencies <- functional_dependency(
      list(
        list("a", "b"),
        list("b", "a"),
        list("a", "c"),
        list("b", "c"),
        list("a", "d"),
        list("b", "f"),
        list("d", "e"),
        list("f", "e")
      ),
      attrs = c("a", "b", "c", "d", "e", "f")
    )
    norm.dependencies <- synthesise(dependencies)
    expect_relation_schema(
      norm.dependencies,
      list(
        attrs = list(c("a", "b", "c", "d", "f"), c("d", "e"), c("f", "e")),
        keys = list(list("a", "b"), list("d"), list("f")),
        all_attrs = c("a", "b", "c", "d", "e", "f"),
        relation_names = c("a", "d", "f")
      )
    )
  })
  it("removes transient dependencies after-merging keys (DFD fig. 3)", {
    dependencies <- functional_dependency(
      list(
        list(c("x1", "x2"), "a"),
        list(c("x1", "x2"), "d"),
        list(c("c", "d"), "x1"),
        list(c("c", "d"), "x2"),
        list(c("a", "x1"), "b"),
        list(c("b", "x2"), "c"),
        list("c", "a")
      ),
      attrs = c("x1", "x2", "a", "b", "c", "d")
    )
    norm.dep <- synthesise(dependencies)
    expected_attrs <- list(
      c("x1", "x2", "c", "d"), # contains a if trans_deps not removed
      c("x1", "a", "b"),
      c("x2", "b", "c"),
      c("c", "a")
    )
    expected_keys <- list(
      list(c("x1", "x2"), c("c", "d")),
      list(c("x1", "a")),
      list(c("x2", "b")),
      list("c")
    )
    expect_setequal(attrs(norm.dep), expected_attrs)
    expect_setequal(keys(norm.dep), expected_keys)
    expect_identical(
      match(attrs(norm.dep), expected_attrs),
      match(keys(norm.dep), expected_keys)
    )
  })
  it("replaces keys / non-key attributes with their bijection set's chosen index", {
    dependencies <- functional_dependency(
      list(
        list(c("A", "B"), "C"),
        list("C", "A"),
        list("C", "B"),
        list("C", "D"),
        list(c("A", "B", "E"), "F")
      ),
      attrs = c("A", "B", "C", "D", "E", "F")
    )
    norm.dep <- synthesise(dependencies)
    expect_relation_schema(
      norm.dep,
      list(
        attrs = list(c("C", "A", "B", "D"), c("C", "E", "F")),
        keys = list(list("C", c("A", "B")), list(c("C", "E"))),
        all_attrs = c("A", "B", "C", "D", "E", "F"),
        relation_names = c("C", "C_E")
      )
    )
  })
  it("gives unique names if constants appears in attribute names and via constant attributes", {
    fds <- functional_dependency(
      list(
        list("constants", "a"),
        list(character(), "b")
      ),
      attrs = c("constants", "a", "b")
    )
    gets_unique_table_names(fds)
  })
  it("can remove avoidable attributes", {
    # example 6.24 from Maier
    # A <-> B, AC -> D, AC -> E, BD -> C
    # expected without removing avoidable: A <-> B, AC <-> BD -> E
    # expected with removing avoidable: A <-> B, AC <-> AD -> E
    deps <- functional_dependency(
      list(
        list("A", "B"),
        list("B", "A"),
        list(c("A", "C"), "D"),
        list(c("A", "C"), "E"),
        list(c("B", "D"), "C")
      ),
      attrs = c("A", "B", "C", "D", "E")
    )
    norm.deps <- synthesise(deps, remove_avoidable = FALSE)
    expect_relation_schema(
      norm.deps,
      list(
        attrs = list(c("A", "B"), c("A", "C", "B", "D", "E")),
        keys = list(list("A", "B"), list(c("A", "C"), c("B", "D"))),
        all_attrs = c("A", "B", "C", "D", "E"),
        relation_names = c("A", "A_C")
      )
    )
    norm.deps2 <- synthesise(deps, remove_avoidable = TRUE)
    expect_relation_schema(
      norm.deps2,
      list(
        attrs = list(c("A", "B"), c("A", "C", "D", "E")),
        keys = list(list("A", "B"), list(c("A", "C"), c("A", "D"))),
        all_attrs = c("A", "B", "C", "D", "E"),
        relation_names = c("A", "A_C")
      )
    )

    still_lossless_with_less_or_same_attributes_dep <- function(flat_deps) {
      norm_deps_avoid <- synthesise(
        flat_deps,
        remove_avoidable = TRUE
      )
      norm_deps_noavoid <- synthesise(
        flat_deps,
        remove_avoidable = FALSE
      )
      lengths_avoid <- lengths(norm_deps_avoid$attrs)
      lengths_noavoid <- lengths(norm_deps_noavoid$attrs)
      expect_identical(length(lengths_avoid), length(lengths_noavoid))
      expect_true(all(lengths_avoid <= lengths_noavoid))
      expect_true(all(lengths(norm_deps_avoid) <= lengths(norm_deps_noavoid)))
      expect_true(all(mapply(
        \(av, noav) all(is.element(av, noav)),
        norm_deps_avoid$attrs,
        norm_deps_noavoid$attrs
      )))
    }

    still_lossless_with_less_or_same_attributes <- function(df) {
      flat_deps <- discover(df, 1)
      schema_avoid_lossless <- cross_reference(synthesise(
        flat_deps,
        remove_avoidable = TRUE
      ))

      # schema_avoid_lossless should be lossless
      database_avoid_lossless <- decompose(df, schema_avoid_lossless)
      df2 <- rejoin(database_avoid_lossless)
      expect_identical_unordered_table(df2, df)

      still_lossless_with_less_or_same_attributes_dep(flat_deps)
    }

    forall(
      gen_df(10, 7, remove_dup_rows = TRUE),
      still_lossless_with_less_or_same_attributes
    )
    forall(
      gen_flat_deps(7, 20, to = 20L),
      still_lossless_with_less_or_same_attributes_dep,
      tests = 1000
    )
  })
  it("gives database schemas that enforce the given functional dependencies", {
    expect_all_enforced <- function(deps, schema) {
      implied_fds <- synthesised_fds(
        attrs(schema),
        keys(schema)
      )
      implied_flat_fds <- implied_fds
      if (length(implied_flat_fds) > 0)
        implied_flat_fds <- unlist(implied_flat_fds, recursive = FALSE)
      implied_flat_fds <- functional_dependency(implied_flat_fds, attrs(deps))
      dep_closures <- lapply(
        detset(deps),
        find_closure,
        detset(implied_flat_fds),
        dependent(implied_flat_fds)
      )
      fds_reproduced <- mapply(
        \(closure, dep) dep %in% closure,
        dep_closures,
        dependent(deps)
      )

      act <- quasi_label(rlang::enquo(deps), arg = "object")
      act$nonrep <- act$val[!fds_reproduced]
      act$n <- length(act$nonrep)
      expect(
        act$n == 0L,
        sprintf(paste0(
          act$n,
          " dependencies not represented:\n",
          paste(
            vapply(
              act$nonrep,
              \(fd) paste0("{", toString(detset(fd)[[1]]), "} -> ", dependent(fd)),
              character(1)
            ),
            collapse = "\n"
          )
        ))
      )
      invisible(act$val)
    }
    enforces_fds <- function(deps, remove_avoidable = FALSE) {
      if (length(deps) == 0L)
        discard()
      schema <- synthesise(deps, remove_avoidable = remove_avoidable)
      expect_all_enforced(deps, schema)
    }

    # example of when no violations only if removables avoided
    deps <- functional_dependency(
      list(
        list(c("C", "G"), "A"),
        list("E", "B"),
        list("F", "C"),
        list(c("A", "G"), "C"),
        list("F", "D"),
        list("B", "E"),
        list(c("F", "G"), "E"),
        list(c("C", "D"), "F"),
        list(c("D", "G"), "F"),
        list(c("C", "E"), "F"),
        list("E", "G"),
        list(c("A", "C"), "G")
      ),
      c("A", "B", "C", "D", "E", "F", "G")
    )
    enforces_fds(deps, TRUE)

    forall(
      gen_flat_deps_fixed_names(7, 20, from = 1L, to = 20L),
      enforces_fds,
      discard.limit = 10L
    )
  })
})

test_that("drop_primary_dups", {
  df <- data.frame(
    city = c(
      'honolulu', 'boston', 'honolulu', 'dallas', 'seattle',
      'honolulu', 'boston', 'honolulu', 'seattle', 'boston'
    ),
    state = c(
      'HI', 'MA', 'HI', 'TX', 'WA',
      'AL', 'MA', 'HI', 'WA', 'NA'
    ),
    is_liberal = c(
      TRUE, TRUE, TRUE, FALSE, TRUE,
      TRUE, TRUE, TRUE, TRUE, FALSE
    )
  )
  new_df <- drop_primary_dups(df, "city")
  df_new_dic <- data.frame(
    city = c("boston", "dallas", "honolulu", "seattle"),
    state = c("MA", "TX", "HI", "WA"),
    is_liberal = c(TRUE, FALSE, TRUE, TRUE)
  )
  expect_identical(new_df, df_new_dic)

  df <- data.frame(
    requires_light = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
    is_dark = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
    light_on = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  new_df <- drop_primary_dups(df, c('requires_light', 'is_dark'))
  expect_identical(nrow(new_df), 3L)
  expect_true(all(new_df$light_on == (new_df$requires_light & new_df$is_dark)))
})

describe("keys_order", {
  it("works like order() for single-length elements", {
    forall(
      gen.sample.int(100, gen.sample(0:10, 1)),
      expect_biidentical(as.list %>>% keys_order, order)
    )
  })
  it("gives a sorted list if applied as subsetter", {
    forall(
      gen.sample.int(100, gen.sample(0:10, 1)),
      expect_biidentical(subset_by(keys_order) %>>% keys_order, seq_along)
    )
  })
  it("orders by length first", {
    forall(
      gen.emptyable_list(gen.key, 10),
      expect_biidentical(subset_by(keys_order) %>>% lengths, lengths %>>% sort)
    )
  })
  it("orders by values, in given order, within lengths", {
    same_length_sorted <- function(lst) {
      len <- length(lst[[1]])
      stopifnot(all(lengths(lst) == len))
      if (len == 0)
        return(TRUE)
      firsts <- vapply(lst, `[`, integer(1), 1)
      if (is.unsorted(firsts))
        return(FALSE)
      rest <- lapply(lst, `[`, -1)
      all(tapply(rest, firsts, \(x) length(x) <= 1 || same_length_sorted(x)))
    }
    orders_by_values_with_lengths <- function(lst) {
      ord <- keys_order(lst)
      sorted_keys <- lst[ord]
      sorted_lengths <- lengths(lst)[ord]
      expect_true(all(tapply(sorted_keys, sorted_lengths, same_length_sorted)))
    }
    forall(
      gen.emptyable_list(gen.sample.int(2, gen.int(5), replace = TRUE), 10),
      orders_by_values_with_lengths
    )
  })
  it("returns an order, i.e. sequential integers from 1", {
    is_order <- function(x) expect_setequal(x, seq_along(x))
    forall(
      gen.emptyable_list(gen.key, 10),
      keys_order %>>% is_order
    )
  })
})

describe("keys_rank", {
  it("is equal to order of keys_order when there are no ties", {
    gen_unique_lst <- gen.with(gen.emptyable_list(gen.key, 10), unique)
    forall(gen_unique_lst, expect_biequal(keys_rank, keys_order %>>% order))
  })
  it("returns ranks, i.e. reals in [1,length] that sum to same as 1:length", {
    is_rank <- function(r) {
      len <- length(r)
      expect_true(all(r >= 1 & r <= len) && sum(r) == len*(len + 1)/2)
    }
    forall(
      gen.emptyable_list(gen.subsequence(0:10), 10),
      keys_rank %>>% is_rank
    )
  })
  it("gives equal rank to equal keys", {
    no_multielement <- function(x) all(lengths(x) <= 1L)
    expect_monovalued_elements <- function(grouped) {
      expect_true(all(vapply(
        grouped,
        \(x) all(vapply(x, identical, logical(1), x[[1]])),
        logical(1)
      )))
    }
    forall(
      gen.list_with_dups(gen.nonempty_key, 10),
      split_by(keys_rank) %>>%
        if_discard_else(no_multielement, expect_monovalued_elements)
    )
  })
})
