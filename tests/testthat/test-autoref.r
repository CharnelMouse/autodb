library(hedgehog)

describe("autoref", {
  it("returns a valid database_schema", {
    forall(
      gen.relation_schema(letters[1:6], 0, 6),
      autoref %>>%
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
      autoref %>>%
        with_args(is_valid_database_schema, same_attr_name = TRUE)
    )
  })
  it("returns references", {
    schema <- relation_schema(
      list(
        a = list(c("a", "b"), list("a")),
        b = list(c("b", "c"), list("b"))
      ),
      attrs_order = c("a", "b", "c")
    )
    database <- autoref(schema)
    expected_relations <- list(list("a", "b", "b", "b"))
    expect_identical(references(database), expected_relations)
  })
  it("is idempotent", {
    forall(
      gen.relation_schema(letters[1:7], 0, 6),
      expect_biidentical(
        autoref,
        autoref %>>% autoref
      )
    )
  })
  it("only returns non-extraneous table references", {
    only_returns_non_extraneous_references <- function(rs) {
      linked <- autoref(rs)
      table_references <- unique(lapply(references(linked), `[`, c(1, 3)))
      table_references <- list(
        determinant_sets = vapply(table_references, `[[`, character(1), 1),
        dependants = vapply(table_references, `[[`, character(1), 2)
      )
      table_references_as_fds <- functional_dependency(
        Map(list, table_references$determinant_sets, table_references$dependants),
        names(rs)
      )
      expect_identical(
        remove_extraneous(table_references_as_fds),
        table_references_as_fds
      )
    }
    forall(
      gen.relation_schema(letters[1:6], 0, 6),
      only_returns_non_extraneous_references
    )
  })
})
