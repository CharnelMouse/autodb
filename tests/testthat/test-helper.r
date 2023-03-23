library(hedgehog)

test_that("gen_flat_deps_fixed_names generates valid", {
  forall(gen_flat_deps_fixed_names(7, 20), is_valid_functional_dependency)
})

test_that("gen_flat_deps generates valid", {
  forall(gen_flat_deps(7, 20), is_valid_functional_dependency)
})
