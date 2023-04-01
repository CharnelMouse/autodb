library(hedgehog)

gen.nonempty_key <- gen.sample.int(2, gen.int(5), replace = TRUE)
gen.key <- gen.sample.int(2, gen.sample(0:5, 1), replace = TRUE)

describe("normalise", {
  expect_database_schema <- function(current, target) {
    expect_identical(
      current,
      structure(target, class = c("database_schema", "list"))
    )
  }
  gets_unique_table_names <- function(fds) {
    expect_true(!anyDuplicated(normalise(fds)$relation_names))
  }

  it("is invariant to dependency reordering", {
    gen_permutation <- gen_flat_deps(4, 5) |>
      gen.and_then(\(fds) list(fds, gen.sample(fds, length(fds))))
    normalisation_permutation_invariant <- function(lst) {
      deps <- lst[[1]]
      perm <- lst[[2]]
      nds <- normalise(deps)
      new_nds <- normalise(perm)
      expect_identical(new_nds$all_attrs, nds$all_attrs)
      expect_identical(length(new_nds$relation_names), length(nds$relation_names))
      relperm <- order(match(new_nds$relation_names, nds$relation_names))
      expect_identical(new_nds$keys[relperm], nds$keys)
      expect_identical(new_nds$attrs[relperm], nds$attrs)
    }

    # currently-rarely-generated case:
    # given a choice of which dependency to remove as extraneous,
    # order matters
    normalisation_permutation_invariant(list(
      functional_dependency(
        list(
          list(c("C", "D"), "B"),
          list(c("A", "B"), "C"),
          list(c("A", "D"), "B"),
          list(c("A", "D"), "C")
        ),
        attrs = c("A", "B", "C", "D")
      ),
      functional_dependency(
        list(
          list(c("C", "D"), "B"),
          list(c("A", "B"), "C"),
          list(c("A", "D"), "C"),
          list(c("A", "D"), "B")
        ),
        attrs = c("A", "B", "C", "D")
      )
    ))

    forall(gen_permutation, normalisation_permutation_invariant)
  })
  it("removes longer/later-attributed dependency sets if given a choce", {
    schema <- normalise(functional_dependency(
      list(
        list(c("C", "D"), "B"),
        list(c("A", "B"), "C"),
        list(c("A", "D"), "B"),
        list(c("A", "D"), "C")
      ),
      attrs = c("A", "B", "C", "D")
    ))
    expect_setequal(schema$relation_names, c("A_B", "A_D", "C_D"))
    ord <- match("A_D", schema$relation_names)
    expect_identical(setdiff(schema$attrs[[ord]], c("A", "D")), "B")
  })
  it("removes extraneous attributes", {
    dependencies <- functional_dependency(
      list(
        list("a", "b"),
        list(c("a", "b"), "c")
      ),
      attrs = c("a", "b", "c")
    )
    norm.dependencies <- normalise(dependencies)
    expect_database_schema(
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
    norm.dependencies <- normalise(dependencies)
    expect_database_schema(
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
    norm.dependencies <- normalise(dependencies)
    expect_database_schema(
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
    norm.df <- normalise(dependencies)
    expect_database_schema(
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
    norm.dependencies <- normalise(dependencies)
    expect_database_schema(
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
    norm.dep <- normalise(dependencies)
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
    expect_setequal(norm.dep$attrs, expected_attrs)
    expect_setequal(norm.dep$keys, expected_keys)
    expect_identical(
      match(norm.dep$attrs, expected_attrs),
      match(norm.dep$keys, expected_keys)
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
    norm.dep <- normalise(dependencies)
    expect_database_schema(
      norm.dep,
      list(
        attrs = list(c("C", "A", "B", "D"), c("C", "E", "F")),
        keys = list(list("C", c("A", "B")), list(c("C", "E"))),
        all_attrs = c("A", "B", "C", "D", "E", "F"),
        relation_names = c("C", "C_E")
      )
    )
  })
  it("ensures starting dependencies have key elements ordered by attributes", {
    dependencies <- functional_dependency(
      list(list(c("a", "b"), "c")),
      attrs = c("a", "b", "c")
    )
    norm_deps <- normalise(dependencies)
    dependencies2 <- functional_dependency(
      list(list(c("b", "a"), "c")),
      attrs = c("a", "b", "c")
    )
    norm_deps2 <- normalise(dependencies2)
    expect_identical(norm_deps, norm_deps2)
  })
  it("gives unique names to all tables", {
    forall(
      gen_flat_deps(7, 20),
      gets_unique_table_names
    )
  })
  it("gives unique non-zero-length names to tables", {
    gets_nonempty_table_names <- function(fds) {
      nms <- normalise(fds)$relation_names
      expect_true(all(nchar(nms) > 0))
      expect_true(!anyDuplicated(nms))
    }
    forall(
      gen_flat_deps(7, 20),
      gets_nonempty_table_names
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
  it("gives names that aren't empty (e.g. are valid names in Graphviz plots)", {
    gets_nonempty_table_names <- function(fds) {
      schema <- normalise(fds)
      expect_true(all(nchar(schema$relation_names) > 0L))
    }
    forall(gen_flat_deps(7, 20), gets_nonempty_table_names)
  })
  it("gives keys with attributes in original order", {
    gives_ordered_attributes_in_keys <- function(fds) {
      schema <- normalise(fds)
      all_keys <- unlist(schema$keys, recursive = FALSE)
      key_indices <- lapply(all_keys, match, schema$all_attrs)
      expect_false(any(vapply(key_indices, is.unsorted, logical(1))))

      schema2 <- normalise(fds, remove_avoidable = TRUE)
      all_keys2 <- lapply(schema2$keys, "[[", 1)
      key_indices2 <- lapply(all_keys2, match, schema2$all_attrs)
      expect_false(any(vapply(key_indices2, is.unsorted, logical(1))))
    }
    forall(
      gen_flat_deps(7, 20),
      gives_ordered_attributes_in_keys
    )
  })
  it("gives unique keys for each relation", {
    gives_unique_keys <- function(flat_deps) {
      schema <- normalise(flat_deps)
      for (keyset in schema$keys)
        expect_true(!anyDuplicated(keyset))
    }

    deps <- list(
      list(
        A = list(c("C", "G")),
        B = list("E"),
        C = list("F", c("A", "G")),
        D = list("F"),
        E = list("B", c("F", "G")),
        F = list(c("C", "D"), c("D", "G"), c("C", "E")),
        G = list("E", c("A", "C"))
      ),
      attrs = c("A", "B", "C", "D", "E", "F", "G")
    )
    gives_unique_keys(flatten(deps))

    deps2 <- functional_dependency(
      list(
        list("ri", "fvjxtkbal"),
        list("fvjxtkbal", "suwxbd"),
        list(c("fvjxtkbal", "suwxbd", "cvz_tj", "ri"), "q"),
        list(c("cvz_tj", "j"), "ri"),
        list(c("fvjxtkbal", "suwxbd", "cvz_tj", "q", "bgreow", "j"), "ri"),
        list(c("fvjxtkbal", "cvz_tj", "q", "bgreow", "j"), "ri"),
        list(c("fvjxtkbal", "cvz_tj", "j"), "ri"),
        list("suwxbd", "ri"),
        list(c("cvz_tj", "ri"), "bgreow"),
        list(c("suwxbd", "cvz_tj"), "bgreow"),
        list(c("fvjxtkbal", "suwxbd", "cvz_tj", "q", "ri", "bgreow"), "j"),
        list(c("suwxbd", "cvz_tj", "q", "ri", "bgreow"), "j")
      ),
      attrs = c("fvjxtkbal", "suwxbd", "cvz_tj", "q", "ri", "bgreow", "j")
    )
    gives_unique_keys(deps2)

    forall(gen_flat_deps(7, 20), gives_unique_keys)
  })
  it("can only return up to one relation schema with no keys", {
    has_up_to_one_keyless_relation_schema <- function(fds) {
      keys <- normalise(fds)$keys
      expect_lte(sum(lengths(keys) == 0), 1)
    }
    forall(
      gen_flat_deps(7, 20),
      has_up_to_one_keyless_relation_schema
    )
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
    norm.deps <- normalise(deps, remove_avoidable = FALSE)
    expect_database_schema(
      norm.deps,
      list(
        attrs = list(c("A", "B"), c("A", "C", "B", "D", "E")),
        keys = list(list("A", "B"), list(c("A", "C"), c("B", "D"))),
        all_attrs = c("A", "B", "C", "D", "E"),
        relation_names = c("A", "A_C")
      )
    )
    norm.deps2 <- normalise(deps, remove_avoidable = TRUE)
    expect_database_schema(
      norm.deps2,
      list(
        attrs = list(c("A", "B"), c("A", "C", "D", "E")),
        keys = list(list("A", "B"), list(c("A", "C"), c("A", "D"))),
        all_attrs = c("A", "B", "C", "D", "E"),
        relation_names = c("A", "A_C")
      )
    )

    still_lossless_with_less_or_same_attributes_dep <- function(flat_deps) {
      norm_deps_avoid <- normalise(
        flat_deps,
        remove_avoidable = TRUE
      )
      norm_deps_noavoid <- normalise(
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
      flat_deps <- flatten(dfd(df, 1))
      schema_avoid_lossless <- cross_reference(normalise(
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
      gen_flat_deps(7, 20),
      still_lossless_with_less_or_same_attributes_dep,
      tests = 1000
    )
  })
  it("gives database schemas that reproduce the given functional dependencies", {
    expect_nofds <- function(fds) {
      act <- quasi_label(rlang::enquo(fds), arg = "object")
      act$n <- length(act$val)
      expect(
        act$n == 0L,
        sprintf(paste0(
          length(fds),
          " dependencies not represented:\n",
          paste(
            vapply(
              fds,
              \(fd) paste0("{", toString(fd[[1]]), "} -> ", fd[2]),
              character(1)
            ),
            collapse = "\n"
          )
        ))
      )
      invisible(act$val)
    }
    reproduces_fds <- function(flat_deps) {
      if (length(flat_deps) == 0L)
        discard()
      schema <- normalise(flat_deps)
      implied_fds <- synthesised_fds(schema$attrs, schema$keys)
      if (length(implied_fds) > 0)
        implied_fds <- unlist(implied_fds, recursive = FALSE)
      implied_detsets <- lapply(implied_fds, "[[", 1)
      implied_deps <- vapply(implied_fds, "[[", character(1), 2)
      fds_reproduced <- vapply(
        flat_deps$dependencies,
        \(fd) {
          closure <- find_closure(fd[[1]], implied_detsets, implied_deps)
          fd[[2]] %in% closure
        },
        logical(1)
      )
      expect_nofds(flat_deps$dependencies[!fds_reproduced])
    }

    deps <- list(
      dependencies = list(
        A = list(c("C", "G")),
        B = list("E"),
        C = list("F", c("A", "G")),
        D = list("F"),
        E = list("B", c("F", "G")),
        F = list(c("C", "D"), c("D", "G"), c("C", "E")),
        G = list("E", c("A", "C"))
      ),
      attrs = c("A", "B", "C", "D", "E", "F", "G")
    )
    reproduces_fds(flatten(deps))

    forall(
      gen_flat_deps_fixed_names(7, 20),
      reproduces_fds,
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
