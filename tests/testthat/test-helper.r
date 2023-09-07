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
  forall(
    gen.element(c(FALSE, TRUE)) |>
      gen.and_then(\(san) list(
        gen.pure(san),
        gen.database_schema(letters[1:6], 0, 8, same_attr_name = san)
      )),
    \(san, ds) is_valid_database_schema(
      ds,
      same_attr_name = san,
      single_key_pairs = san
    ),
    curry = TRUE
  )
})

test_that("gen.relation generates valid relations", {
  forall(
    gen.relation(letters[1:4], 6, 7),
    is_valid_relation
  )
})

test_that("gen.database generates valid databases", {
  forall(
    gen.element(c(FALSE, TRUE)) |>
      gen.and_then(\(san) list(
        gen.pure(san),
        gen.database(letters[1:7], from = 0, to = 6, same_attr_name = san)
      )),
    \(san, ds) is_valid_database(
      ds,
      same_attr_name = san,
      single_key_pairs = san
    ),
    curry = TRUE
  )
})
