library(hedgehog)

describe("cross_reference", {
  it("returns a valid database_schema", {
    forall(
      list(
        schema = gen.relation_schema(letters[1:6], 0, 6),
        ensure_lossless = gen.element(c(FALSE, TRUE))
      ),
      cross_reference %>>%
        with_args(
          is_valid_database_schema,
          unique = FALSE,
          single_empty_key = FALSE,
          same_attr_name = TRUE,
          single_key_pairs = TRUE
        ),
      curry = TRUE
    )
  })
  it("generates valid schemas with same-attribute-names foreign key references", {
    forall(
      list(
        schema = gen.relation_schema(letters[1:4], 0, 6),
        ensure_lossless = gen.element(c(FALSE, TRUE))
      ),
      cross_reference %>>%
        with_args(is_valid_database_schema, same_attr_name = TRUE),
      curry = TRUE
    )
  })
  it("returns relationships", {
    schema <- relation_schema(
      list(
        a = list(c("a", "b"), list("a")),
        b = list(c("b", "c"), list("b"))
      ),
      attrs_order = c("a", "b", "c")
    )
    database <- cross_reference(schema)
    expected_relations <- list(list("a", "b", "b", "b"))
    expect_identical(relationships(database), expected_relations)
  })
  it("reintroduces attributes not in dependencies if ensuring lossless", {
    reintroduces_missing_attrs_if_lossless <- function(rs) {
      lone_attr <- LETTERS[length(attrs_order(rs)) + 1]
      new_rs <- relation_schema(
        unclass(rs),
        attrs_order = c(attrs_order(rs), lone_attr),
        attrs_class = c(attrs_class(rs), setNames(list("logical"), lone_attr))
      )
      linked <- cross_reference(rs, ensure_lossless = TRUE)
      expect_true(all(is.element(attrs_order(rs), unlist(attrs(linked)))))
    }
    forall(
      gen.relation_schema(letters[1:7], 0, 6),
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
      lengths_avoid_lossy <- lengths(attrs(schema_avoid_lossy))
      lengths_noavoid_lossy <- lengths(attrs(schema_noavoid_lossy))
      lengths_avoid_lossless <- lengths(attrs(schema_avoid_lossless))
      lengths_noavoid_lossless <- lengths(attrs(schema_noavoid_lossless))

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
          attrs(schema_avoid_lossless)[[lossless_length]],
          attrs(schema_noavoid_lossless)[[lossless_length]]
        )
    }

    forall(
      gen_flat_deps(7, 20, to = 20L),
      still_lossless_with_less_or_same_attributes_dep
    )
  })
  it("adds table with key with attributes in original order", {
    adds_ordered_primary_keys <- function(fds) {
      schema <- normalise(fds, ensure_lossless = TRUE)
      all_keys <- unlist(keys(schema), recursive = FALSE)
      key_indices <- lapply(all_keys, match, attrs_order(schema))
      expect_false(any(vapply(key_indices, is.unsorted, logical(1))))
    }
    forall(
      gen_flat_deps(7, 20, to = 20L),
      adds_ordered_primary_keys
    )
  })
  it("only returns non-extraneous table relationships", {
    only_returns_non_extraneous_relationships <- function(deps) {
      linked <- normalise(deps, ensure_lossless = TRUE)
      table_relationships <- unique(lapply(relationships(linked), `[`, c(1, 3)))
      table_relationships <- list(
        determinant_sets = vapply(table_relationships, `[[`, character(1), 1),
        dependents = vapply(table_relationships, `[[`, character(1), 2)
      )
      table_relationships_indices <- lapply(
        table_relationships,
        \(nms) match(nms, names(linked))
      )
      expect_identical(
        remove_extraneous_dependencies(table_relationships_indices),
        table_relationships_indices
      )
    }
    forall(
      gen_flat_deps(7, 20, to = 20L),
      only_returns_non_extraneous_relationships
    )
  })
  it("is idempotent", {
    forall(
      list(
        gen.relation_schema(letters[1:7], 0, 6),
        gen.element(c(FALSE, TRUE))
      ),
      \(rs, el) {
        cross_reference(rs, ensure_lossless = el) |>
          expect_biidentical(
            identity,
            with_args(cross_reference, ensure_lossless = el)
          )()
      },
      curry = TRUE
    )
  })
  it("returns relations that return themselves if normalised again, if lossless", {
    gen.database_schema_single_lossless <- function(x) {
      gen.choice(
        gen.pure(x),
        gen.pure(x[FALSE]),
        gen.subsequence(x)
      ) |>
        gen.with(function(key) {
          nonkey <- setdiff(x, key)
          attrs <- list(c(key, nonkey))
          keys <- list(list(key))
          nm <- if (length(key) == 0L)
            "constants"
          else
            paste(key, collapse = "_")
          database_schema(
            relation_schema(
              setNames(
                Map(list, attrs, keys),
                nm
              ),
              attrs_order = x
            ),
            list()
          )
        })
    }
    returns_itself <- function(relation) {
      deps <- functional_dependency(
        unlist(synthesised_fds(attrs(relation), keys(relation)), recursive = FALSE),
        attrs_order(relation)
      )
      redo <- normalise(deps)
      expect_identical(redo, relation)
    }
    forall(
      gen.database_schema_single_lossless(letters[1:10]),
      returns_itself
    )
  })
})
