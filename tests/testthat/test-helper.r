library(hedgehog)

test_that("gen_flat_deps_fixed_names generates valid", {
  forall(gen_flat_deps_fixed_names(7, 20, to = 20L), is_valid_functional_dependency)
})

test_that("gen_flat_deps generates valid", {
  forall(gen_flat_deps(7, 20, to = 20L), is_valid_functional_dependency)
})

test_that("gen.relation_schema generates valid relation schemas", {
  forall(gen.relation_schema(letters[1:6], 0, 8), is_valid_relation_schema)
})

test_that("gen.database_schema generates valid database schemas", {
  forall(gen.database_schema(letters[1:6], 0, 8), is_valid_database_schema)
})
