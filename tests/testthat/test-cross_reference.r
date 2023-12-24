library(hedgehog)

describe("cross_reference", {
  it("returns a valid database_schema", {
    forall(
      gen.relation_schema(letters[1:6], 0, 6),
      cross_reference %>>%
        with_args(
          is_valid_database_schema,
          unique = FALSE,
          single_empty_key = FALSE,
          same_attr_name = TRUE,
          single_key_pairs = TRUE
        )
    )
  })
  it("generates valid schemas with same-attribute-names foreign key references", {
    forall(
      gen.relation_schema(letters[1:4], 0, 6),
      cross_reference %>>%
        with_args(is_valid_database_schema, same_attr_name = TRUE)
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
  it("is idempotent", {
    forall(
      gen.relation_schema(letters[1:7], 0, 6),
      expect_biidentical(
        cross_reference,
        cross_reference %>>% cross_reference
      )
    )
  })
  it("only returns non-extraneous table relationships", {
    only_returns_non_extraneous_relationships <- function(rs) {
      linked <- cross_reference(rs)
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
      gen.relation_schema(letters[1:6], 0, 6),
      only_returns_non_extraneous_relationships
    )
  })
})
