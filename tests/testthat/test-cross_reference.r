library(hedgehog)

describe("cross_reference", {
  it("returns relationships", {
    schema <- list(
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
    database <- cross_reference(schema)
    expected_parents = list(2L, integer())
    expected_relations <- list(
      list(c(1L, 2L), "b")
    )
    expect_identical(database$parents, expected_parents)
    expect_identical(database$relationships, expected_relations)
  })
  it("gives valid schemas", {
    # same as test for normalise, need synthesis result generator
    # table_dum and table_dee
    empty_fds <- functional_dependency(list(), attrs = character())
    empty_schema <- normalise(empty_fds)
    is_valid_database_schema(empty_schema)

    forall(
      gen_flat_deps(7, 20),
      apply_both(
        normalise %>>% is_valid_database_schema,
        with_args(normalise, ensure_lossless = FALSE) %>>%
          is_valid_database_schema
      )
    )
  })
  it("only links children to parents by exactly one parent key", {
    links_by_exactly_one_parent_key <- function(deps) {
      schema <- normalise(deps)
      if (length(schema$keys) <= 1)
        discard()
      if (length(schema$relationships) == 0)
        discard()
      relationship_tables <- lapply(schema$relationships, `[[`, 1)
      relationship_attrs <- vapply(schema$relationships, `[[`, character(1), 2)
      tables_index <- as.data.frame(do.call(rbind, relationship_tables))
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
          setdiff(attribute_sets, lapply(schema$keys[[parent]], sort)),
          0
        )
      }
    }
    forall(
      gen_flat_deps(20, 2),
      links_by_exactly_one_parent_key,
      discard.limit = 10
    )
  })
  it("reintroduces attributes not in dependencies if ensuring lossless", {
    reintroduces_missing_attrs_if_lossless <- function(deps) {
      lone_attr <- LETTERS[length(attrs(deps)) + 1]
      attr(deps, "attrs") <- c(attrs(deps), lone_attr)
      linked <- normalise(deps, ensure_lossless = TRUE)
      expect_true(lone_attr %in% unlist(linked$attrs))
    }
    forall(
      gen_flat_deps(7, 20),
      reintroduces_missing_attrs_if_lossless
    )
  })
  it("has no change in added table for losslessness if avoidable attributes removed", {
    still_lossless_with_less_or_same_attributes_dep <- function(flat_deps) {
      schema_avoid_lossy <- normalise(
        flat_deps,
        ensure_lossless = FALSE,
        remove_avoidable = TRUE
      )
      schema_noavoid_lossy <- normalise(
        flat_deps,
        ensure_lossless = FALSE,
        remove_avoidable = FALSE
      )
      schema_avoid_lossless <- normalise(
        flat_deps,
        ensure_lossless = TRUE,
        remove_avoidable = TRUE
      )
      schema_noavoid_lossless <- normalise(
        flat_deps,
        ensure_lossless = TRUE,
        remove_avoidable = FALSE
      )
      lengths_avoid_lossy <- lengths(schema_avoid_lossy$attrs)
      lengths_noavoid_lossy <- lengths(schema_noavoid_lossy$attrs)
      lengths_avoid_lossless <- lengths(schema_avoid_lossless$attrs)
      lengths_noavoid_lossless <- lengths(schema_noavoid_lossless$attrs)

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
          schema_avoid_lossless$attrs[[lossless_length]],
          schema_noavoid_lossless$attrs[[lossless_length]]
        )
    }

    forall(
      gen_flat_deps(7, 20),
      still_lossless_with_less_or_same_attributes_dep
    )
  })
  it("adds table with key with attributes in original order", {
    adds_ordered_primary_keys <- function(fds) {
      schema <- normalise(fds, ensure_lossless = TRUE)
      all_keys <- unlist(schema$keys, recursive = FALSE)
      key_indices <- lapply(all_keys, match, schema$all_attrs)
      expect_false(any(vapply(key_indices, is.unsorted, logical(1))))
    }
    forall(
      gen_flat_deps(7, 20),
      adds_ordered_primary_keys
    )
  })
  it("only returns non-extraneous table relationships", {
    only_returns_non_extraneous_relationships <- function(deps) {
      linked <- normalise(deps, ensure_lossless = TRUE)
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
      gen_flat_deps(7, 20),
      only_returns_non_extraneous_relationships
    )
  })
  it("is idempotent", {
    forall(
      gen_flat_deps(7, 20),
      normalise %>>%
        expect_biidentical(identity, cross_reference)
    )
  })
  it("returns relations that return themselves if normalised again, if lossless", {
    gen.key <- gen.sample(letters[1:10], gen.int(10)) |>
      gen.with(sort)
    gen.relation <- gen.key |>
      gen.and_then(function(key) {
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
          class = c("database_schema", "list")
        )
      })
    returns_itself <- function(relation) {
      nonkey <- setdiff(unlist(relation$attrs), unlist(relation$keys))
      deps <- functional_dependency(
        if (length(nonkey) == 0L)
          list()
        else
          unlist(
            lapply(
              nonkey,
              \(x) lapply(relation$keys[[1]], \(k) list(k, x))
            ),
            recursive = FALSE
          ),
        relation$attrs[[1]]
      )
      redo <- normalise(deps, ensure_lossless = TRUE)
      expect_length(redo$attrs, 1)
      expect_identical(redo$attrs[[1]], relation$attrs[[1]])
      expect_identical(redo$keys[[1]], relation$keys[[1]])
      expect_identical(redo$relation_names, relation$relation_names)
      expect_setequal(redo$all_attrs, relation$all_attrs)
    }
    forall(gen.relation, returns_itself)
  })
})
