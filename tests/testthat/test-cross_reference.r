library(hedgehog)

describe("cross_reference", {
  it("returns relationships", {
    scheme <- list(
      attrs = list(
        c("a", "b"),
        c("b", "c")
      ),
      keys = list(
        list("a"),
        list("b")
      ),
      all_attrs = c("a", "b", "c")
    )
    database <- cross_reference(scheme)
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
        tables_index <- as.data.frame(do.call(rbind, relationship_tables))
        if (nrow(tables_index) == 0)
          succeed()
        else{
          link_sets <- tapply(
            relationship_attrs,
            tables_index,
            \(as) sort(unique(as))
          )
          for (column in seq_len(ncol(link_sets))) {
            parent <- strtoi(colnames(link_sets)[column])
            attribute_sets <- link_sets[, column]
            attribute_sets <- na.omit(attribute_sets[!vapply(
              attribute_sets,
              is.null,
              logical(1)
            )])
            expect_length(
              setdiff(attribute_sets, lapply(linked$keys[[parent]], sort)),
              0
            )
          }
        }
      }
    }
    forall(
      gen_df(6, 7, nonempty = TRUE, remove_dup_rows = TRUE),
      links_by_exactly_one_parent_key
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

      # losslessness should add 0 or 1 tables
      expect_gte(
        length(lengths_avoid_lossless),
        length(lengths_avoid_lossy)
      )
      expect_lte(
        length(lengths_avoid_lossless),
        length(lengths_avoid_lossy) + 1
      )
      expect_gte(
        length(lengths_noavoid_lossless),
        length(lengths_noavoid_lossy)
      )
      expect_lte(
        length(lengths_noavoid_lossless),
        length(lengths_noavoid_lossy) + 1
      )

      # removing avoidable attributes doesn't affect whether extra table added
      expect_identical(
        length(lengths_avoid_lossless),
        length(lengths_noavoid_lossless)
      )

      # removing avoidable attributes can't make tables wider
      lossless_length <- length(lengths_avoid_lossless)
      for (l in seq_len(lossless_length)) {
        expect_lte(lengths_avoid_lossless[l], lengths_noavoid_lossless[l])
      }

      # if extra table added, avoidance shouldn't affect its attributes
      if (length(lengths_avoid_lossless) > length(lengths_avoid_lossy))
        expect_identical(
          scheme_avoid_lossless$attrs[[lossless_length]],
          scheme_noavoid_lossless$attrs[[lossless_length]]
        )
    }

    forall(
      gen_flat_deps(7, 20),
      still_lossless_with_less_or_same_attributes_dep,
      tests = 1000
    )
  })
  it("adds table with key with attributes in original order", {
    adds_ordered_primary_keys <- function(fds) {
      scheme <- cross_reference(normalise(fds), ensure_lossless = TRUE)
      all_keys <- unlist(scheme$keys, recursive = FALSE)
      key_indices <- lapply(all_keys, match, scheme$all_attrs)
      expect_false(any(vapply(key_indices, is.unsorted, logical(1))))
    }
    forall(
      gen_flat_deps(7, 20),
      adds_ordered_primary_keys,
      tests = 1000
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
            relationships = list(),
            relation_names = paste(key, collapse = "_"),
            all_attrs = letters[1:10]
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
      expect_length(redo$attrs, 1)
      expect_identical(redo$attrs[[1]], relation$attrs[[1]])
      expect_identical(redo$keys[[1]], relation$keys[[1]])
      expect_identical(redo$relation_names, relation$relation_names)
      expect_setequal(redo$all_attrs, relation$all_attrs)
    }
    forall(gen.relation, returns_itself)
  })
})
