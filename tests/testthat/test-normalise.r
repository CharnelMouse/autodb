library(hedgehog)

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

  it("doesn't change relation attribute order if dependencies are reordered", {
    df <- data.frame(
      a = rep(1:2, each = 2),
      b = rep(1:2, each = 2),
      c = rep(1:2, each = 2),
      d = rep(1:2, each = 2),
      e = rep(1:2, each = 2),
      f = rep(1:2, each = 2)
    )
    deps <- list(
      list(
        a = as.list(letters[1:6][-1]),
        b = as.list(letters[1:6][-2]),
        c = as.list(letters[1:6][-3]),
        d = as.list(letters[1:6][-4]),
        e = as.list(letters[1:6][-5]),
        f = as.list(letters[1:6][-6])
      ),
      attrs = letters[1:6]
    )
    deps <- flatten(deps)
    nds <- normalise(deps)
    gen_permutation <- gen.sample(deps$dependencies, length(deps$dependencies))
    normalisation_permutation_invariant <- function(perm) {
      new_deps <- deps
      new_deps$dependencies <- perm
      new_nds <- normalise(new_deps)
      expect_identical(nds, new_nds)
    }
    forall(gen_permutation, normalisation_permutation_invariant)
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
  it("can handle basic bijections", {
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
  expect_false(new_df$light_on[new_df$requires_light & !new_df$is_dark])
  expect_false(new_df$light_on[!new_df$requires_light & new_df$is_dark])
  expect_true(new_df$light_on[new_df$requires_light & new_df$is_dark])
})

describe("keys_order", {
  it("works like order() for single-length elements", {
    same_as_order <- function(ints) {
      expect_identical(order(ints), keys_order(ints))
    }
    forall(gen.sample.int(100, 10), same_as_order)
  })
  it("gives a sorted list if applied as subsetter", {
    sorts_input_to_itself <- function(ints) {
      ord <- keys_order(ints)
      expect_identical(seq_along(ints), keys_order(ints[ord]))
    }
    forall(gen.sample.int(100, 10), sorts_input_to_itself)
  })
  it("orders by length first", {
    gen_el <- generate(for (n in gen.sample(0:4, 1)) {
      generate(for (start in gen.int(10)) {
        as.integer(start - 1L + seq_len(n))
      })
    })
    gen_lst <- gen.list(gen_el, to = 10)
    orders_by_length_first <- function(lst) {
      ord <- keys_order(lst)
      ord_sorted_within_lengths <- ord |>
        tapply(lengths(lst)[ord], sort, simplify = FALSE) |>
        unlist(use.names = FALSE)
      expect_identical(order(lengths(lst)), ord_sorted_within_lengths)
    }
    forall(gen_lst, orders_by_length_first)
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
    gen_el <- generate(for (n in gen.int(5)) {
      gen.sample.int(2, n, replace = TRUE)
    })
    gen_lst <- gen.list(gen_el, to = 10)
    orders_by_values_with_lengths <- function(lst) {
      ord <- keys_order(lst)
      sorted_keys <- lst[ord]
      sorted_lengths <- lengths(lst)[ord]
      expect_true(all(tapply(sorted_keys, sorted_lengths, same_length_sorted)))
    }
    forall(gen_lst, orders_by_values_with_lengths)
  })
})

describe("normalise() replacing normalize_step()", {
  expect_database_schema <- function(current, target) {
    expect_identical(
      current,
      structure(target, class = c("database_schema", "list"))
    )
  }

  it("removes extraneous dependencies", {
    dependencies <- functional_dependency(
      list(
        list("a", "b"),
        list(c("a", "b"), "c")
      ),
      attrs = c("a", "b", "c")
    )
    norm.df <- normalise(dependencies)
    expect_database_schema(
      norm.df,
      list(
        attrs = list(c("a", "b", "c")),
        keys = list(list("a")),
        all_attrs = c("a", "b", "c"),
        relation_names = "a"
      )
    )
  })
  it("resolves a simple bijection with no splits", {
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
  it("correctly splits example data.frame for original make_indexes() test", {
    # has multi-keys getting combined, so tests construct_relations
    # handles multiple values in RHS of input relations
    df <- data.frame(
      id = c(
        0, 1, 2, 3, 4,
        5, 6, 7, 8, 9
      ),
      month = c(
        'dec', 'dec', 'jul', 'jul', 'dec',
        'jul', 'jul', 'jul', 'dec', 'jul'
      ),
      hemisphere = c(
        'N', 'N', 'N', 'N', 'S',
        'S', 'S', 'S', 'S', 'N'
      ),
      is_winter = c(
        TRUE, TRUE, FALSE, FALSE, FALSE,
        TRUE, TRUE, TRUE, FALSE, FALSE
      )
    )
    deps <- functional_dependency(
      list(
        list("id", "month"),
        list("id", "hemisphere"),
        list("id", "is_winter"),
        list(c("month", "hemisphere"), "is_winter"),
        list(c("month", "is_winter"), "hemisphere"),
        list(c("hemisphere", "is_winter"), "month")
      ),
      attrs = c("id", "month", "hemisphere", "is_winter")
    )
    new_deps <- normalise(deps)
    expected_parent <- list(
      attrs = c("id", "month", "hemisphere"),
      keys = list("id")
    )
    expect_identical(length(new_deps$attrs[[1]]), 3L)
    expect_true("id" %in% new_deps$attrs[[1]])
    expect_identical(
      length(intersect(
        new_deps$attrs[[1]],
        c("month", "hemisphere", "is_winter")
      )),
      2L
    )
    expect_identical(new_deps$keys[[1]], list("id"))
    expected_child_attrs <- c("month", "hemisphere", "is_winter")
    expected_child_keys <- list(
      c("month", "hemisphere"),
      c("month", "is_winter"),
      c("hemisphere", "is_winter")
    )
    expect_identical(new_deps$attrs[[2]], expected_child_attrs)
    expect_identical(new_deps$keys[[2]], expected_child_keys)
  })
  it("DepDF", {
    deps <- functional_dependency(
      list(
        list(c("player_name", "jersey_num"), "team"),
        list(c("player_name", "team"), "jersey_num"),
        list(c("team", "jersey_num"), "player_name"),
        list("team", "city"),
        list("state", "city"),
        list(c("player_name", "jersey_num"), "city"),
        list("team", "state"),
        list(c("player_name", "jersey_num"), "state"),
        list("city", "state")
      ),
      attrs = c("player_name", "jersey_num", "team", "state", "city")
    )
    new_deps <- normalise(deps)
    expected_schema <- list(
      attrs = list(
        c("player_name", "jersey_num", "team"),
        c("team", "state"),
        c("state", "city")
      ),
      keys = list(
        list(
          c("player_name", "jersey_num"),
          c("player_name", "team"),
          c("jersey_num", "team")
        ),
        list("team"),
        list("state", "city")
      )
    )
    expect_setequal(new_deps$attrs, expected_schema$attrs)
    expect_setequal(new_deps$keys, expected_schema$keys)
    expect_identical(
      match(new_deps$attrs, expected_schema$attrs),
      match(new_deps$keys, expected_schema$keys)
    )
  })
})
