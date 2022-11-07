library(hedgehog)

describe("cross_reference", {
  it("returns relationships", {
    df <- data.frame(a = integer(), b = integer(), c = integer())
    norm_deps <- list(
      attrs = list(
        c("a", "b"),
        c("b", "c")
      ),
      keys = list(
        list("a"),
        list("b")
      )
    )
    database <- cross_reference(norm_deps)
    expected_parents = list(2L, integer())
    expected_relations <- list(
      list(c(1L, 2L), "b")
    )
    expect_identical(database$parents, expected_parents)
    expect_identical(database$relationships, expected_relations)
  })
  it("only links children to parents by exactly one parent key", {
    links_by_exactly_one_parent_key <- function(df) {
      deps <- dfd(df, 1)
      scheme <- normalise(flatten(deps))
      if (length(scheme$keys) <= 1)
        succeed()
      else{
        linked <- cross_reference(scheme)
        relationship_tables <- lapply(linked$relationships, `[[`, 1)
        relationship_attrs <- vapply(linked$relationships, `[[`, character(1), 2)
        link_sets <- tapply(
          relationship_attrs,
          as.data.frame(do.call(rbind, relationship_tables)),
          \(as) sort(unique(as))
        )
        for (column in seq_len(ncol(link_sets))) {
          parent <- strtoi(colnames(link_sets)[column])
          attribute_sets <- link_sets[, column]
          attribute_sets <- attribute_sets[!vapply(attribute_sets, is.null, logical(1))]
          expect_lte(length(setdiff(attribute_sets, linked$keys[[parent]])), 1)
        }
      }
    }
    forall(
      gen_df(6, 7, nonempty = TRUE, remove_dup_rows = TRUE),
      links_by_exactly_one_parent_key
    )
  })
  it("leaves a single ultimate child if given a lossless decomposition", {
    single_ultimate_child_if_lossless_decomposition <- function(df) {
      deps <- dfd(df, 1)
      scheme <- normalise(flatten(deps))
      linked <- cross_reference(scheme, ensure_lossless = TRUE)
      if (length(linked$keys) == 1)
        succeed()
      else{
        relationship_tables <- lapply(linked$relationships, `[[`, 1)
        parents <- vapply(relationship_tables, `[`, integer(1), 2)
        children <- vapply(relationship_tables, `[`, integer(1), 1)
        non_parents <- setdiff(children, parents)
        expect_length(non_parents, 1)
      }
    }
    forall(
      gen_df(6, 7, nonempty = TRUE, remove_dup_rows = TRUE),
      single_ultimate_child_if_lossless_decomposition
    )
  })
  it("leaves no stranded tables if given a lossless decomposition", {
    no_stranded_if_lossless_decomposition <- function(df) {
      deps <- dfd(df, 1)
      scheme <- normalise(flatten(deps))
      linked <- cross_reference(scheme, ensure_lossless = TRUE)
      if (length(linked$keys) == 1)
        succeed()
      else{
        relationship_tables <- lapply(linked$relationships, `[[`, 1)
        stranded <- setdiff(
          seq_along(linked$keys),
          unlist(relationship_tables)
        )
        expect_length(stranded, 0)
      }
    }
    forall(
      gen_df(6, 7, nonempty = FALSE, remove_dup_rows = TRUE),
      no_stranded_if_lossless_decomposition
    )
  })
  it("reintroduces attributes not in dependencies if ensuring lossless", {
    reintroduces_missing_attrs_if_lossless <- function(df) {
      deps <- dfd(df, 1)
      lone_attr <- LETTERS[length(deps$attrs) + 1]
      deps$dependencies <- c(deps$dependencies, setNames(list(list()), lone_attr))
      deps$attrs <- c(deps$attrs, lone_attr)
      scheme <- normalise(flatten(deps))
      linked <- cross_reference(scheme, ensure_lossless = TRUE)
      expect_true(lone_attr %in% unlist(linked$attrs))
    }
    forall(
      gen_df(6, 7, remove_dup_rows = TRUE),
      reintroduces_missing_attrs_if_lossless
    )
  })
  it("has no change in added table for losslessness if avoidable attributes removed", {
    still_lossless_with_less_or_same_attributes_dep <- function(flat_deps) {
      norm_deps_avoid <- normalise(
        flat_deps,
        remove_avoidable = TRUE
      )
      norm_deps_noavoid <- normalise(
        flat_deps,
        remove_avoidable = FALSE
      )

      # failing example:
      # flat_deps <- list(
      #   dependencies = list(
      #     list("F", "B"),
      #     list(c("B", "E"), "C"),
      #     list(c("A", "B", "E", "F", "G"), "D"),
      #     list(c("B", "C", "E", "F"), "D"),
      #     list(c("A", "B", "C", "E", "F"), "D"),
      #     list(c("A", "B", "C", "E", "F", "G"), "D"),
      #     list(c("B", "C", "D"), "F"),
      #     list("C", "F"),
      #     list(c("C", "D", "E"), "F"),
      #     list(c("B", "C"), "F"),
      #     list(c("B", "C", "D", "E", "G"), "F"),
      #     list(c("A", "B", "C", "D", "E", "G"), "F"),
      #     list(c("D", "F"), "G"),
      #     list(c("A", "B", "C", "D", "E", "F"), "G"),
      #     list(c("A", "B", "D"), "G"),
      #     list("C", "G")
      #   ),
      #   attrs = LETTERS[1:7]
      # )

      # removing attributes shouldn't add an extra table for losslessness if
      # there wasn't one before
      scheme_avoid_lossy <- cross_reference(
        norm_deps_avoid,
        ensure_lossless = FALSE
      )
      scheme_noavoid_lossy <- cross_reference(
        norm_deps_noavoid,
        ensure_lossless = FALSE
      )
      scheme_avoid_lossless <- cross_reference(
        norm_deps_avoid,
        ensure_lossless = TRUE
      )
      scheme_noavoid_lossless <- cross_reference(
        norm_deps_noavoid,
        ensure_lossless = TRUE
      )
      lengths_avoid_lossy <- lengths(scheme_avoid_lossy$attrs)
      lengths_noavoid_lossy <- lengths(scheme_noavoid_lossy$attrs)
      lengths_avoid_lossless <- lengths(scheme_avoid_lossless$attrs)
      lengths_noavoid_lossless <- lengths(scheme_noavoid_lossless$attrs)

      # Losslessness should add 0 or 1 tables
      expect_gte(length(lengths_avoid_lossless), length(lengths_avoid_lossy))
      expect_lte(length(lengths_avoid_lossless), length(lengths_avoid_lossy) + 1)
      expect_gte(length(lengths_noavoid_lossless), length(lengths_noavoid_lossy))
      expect_lte(length(lengths_noavoid_lossless), length(lengths_noavoid_lossy) + 1)

      # # Sometimes removing avoidable attributes allows not adding an extra table
      # # to keep decomposition lossless, so can't always expect length of lengths
      # # to be identical: lengths2 might be one longer.
      expect_lte(length(lengths_avoid_lossless), length(lengths_noavoid_lossless))
      expect_gte(length(lengths_avoid_lossless), length(lengths_noavoid_lossless) - 1)
      # for (l in seq_len(min(length(lengths_avoid_lossless), length(lengths_noavoid_lossless)))) {
      #   expect_lte(lengths_avoid_lossless[l], lengths_noavoid_lossless[l])
      # }

      # additional tests to add:
      # - Accounting for extra tables. Any combination of noavoid and avoid
      # having one is possible.
      # - something about not introducing an extra table? or not making it
      # wider?
      # - something about not changing table hierarchy / cross-references?
    }

    forall(
      gen_flat_deps(7, 20),
      still_lossless_with_less_or_same_attributes_dep
    )
  })
  it("only return non-extraneous table relationships", {
    only_returns_non_extraneous_relationships <- function(df) {
      scheme <- normalise(flatten(dfd(df, 1)))
      linked <- cross_reference(scheme, ensure_lossless = TRUE)
      table_relationships <- unique(lapply(linked$relationships, `[[`, 1))
      table_relationships <- list(
        determinant_sets = lapply(table_relationships, `[`, 1),
        dependents = vapply(table_relationships, `[`, integer(1), 2)
      )
      expect_identical(
        remove_extraneous_dependencies(table_relationships),
        table_relationships
      )
    }
    forall(
      gen_df(6, 7, nonempty = FALSE, remove_dup_rows = TRUE),
      only_returns_non_extraneous_relationships
    )
  })
  it("returns relations that return themselves if normalised again", {
    gen.keysize <- gen.sample.int(10)
    gen.key <- generate(
      for (keysize in gen.keysize) {
        letters[1:10][sort(sample(1:10, keysize))]
      }
    )
    gen.relation <- generate(
      for (key in gen.key) {
        nonkey <- setdiff(letters[1:10], key)
        structure(
          list(
            attrs = list(c(key, nonkey)),
            keys = list(list(key)),
            parents = list(integer()),
            relationships = list()
          ),
          class = c("database_scheme", "list")
        )
      }
    )
    returns_itself <- function(relation) {
      nonkey <- setdiff(unlist(relation$attrs), unlist(relation$keys))
      deps <- flatten(list(
        dependencies = setNames(
          lapply(
            nonkey,
            \(x) relation$keys[[1]]
          ),
          nonkey
        ),
        attrs = relation$attrs[[1]]
      ))
      redo <- cross_reference(normalise(deps), ensure_lossless = TRUE)
      expect_identical(redo, relation)
      expect_length(redo$attrs, 1)
      expect_identical(redo$attrs[[1]], relation$attrs[[1]])
      expect_identical(redo$keys[[1]], relation$keys[[1]])
    }
    forall(gen.relation, returns_itself)
  })
})
