library(hedgehog)

describe("reduce", {
  it("is idempotent", {
    has_idempotent_reduction <- function(df) {
      es <- autonorm(as.data.frame(df), 1, ensure_lossless = FALSE)
      once <- reduce(es)
      twice <- reduce(once)
      expect_identical(twice, once)
    }
    forall(gen_df(6, 7), has_idempotent_reduction)
  })
  it("does nothing to a lossless database", {
    has_lossless_decomp_invariant_to_reduce <-       function(df) {
      es <- autonorm(as.data.frame(df), 1, ensure_lossless = TRUE)
      once <- reduce(es)
      expect_identical(once, es)
    }
    forall(gen_df(6, 7), has_lossless_decomp_invariant_to_reduce)
  })
  it("removes added tables with less rows than existing non-parent tables", {
    removes_added_non_parent_with_non_maximum_nrow <- function(df) {
      es <- autonorm(df, 1, ensure_lossless = TRUE)
      once <- reduce(es)
      once_plus_small <- once
      once_plus_small$tables <- c(
        once_plus_small$tables,
        list(
          extra_table = list(
            df = data.frame(extra_attr = logical()),
            keys = list("extra_attr"),
            index = "extra_attr",
            parents = character()
          )
        )
      )
      twice <- reduce(once_plus_small)
      expect_identical(twice, once)
    }
    forall(
      gen_df(6, 7, nonempty = TRUE),
      removes_added_non_parent_with_non_maximum_nrow
    )
  })
  it("returns a subset", {
    reduced_to_subset <- function(df) {
      es <- autonorm(df, 1, ensure_lossless = FALSE)
      reduced <- reduce(es)
      expect_identical(reduced$name, es$name)
      expect_true(all(reduced$tables %in% es$tables))
      expect_true(all(reduced$relationships %in% es$relationships))
    }
    forall(gen_df(6, 7, nonempty = TRUE), reduced_to_subset)
  })
  it("returns a database where non-parent tables have the same maximal number of rows", {
    all_non_parents_in_reduction_have_same_nrow <- function(df) {
      es <- autonorm(df, 1, ensure_lossless = FALSE)
      if (length(es$tables) == 0)
        succeed()
      else{
        reduced <- reduce(es)
        non_parents <- setdiff(
          names(reduced$tables),
          vapply(reduced$relationships, `[`, character(1), 3)
        )
        non_parent_nrows <- vapply(
          reduced$tables[non_parents],
          \(table) nrow(table$df),
          integer(1)
        )
        max_table_nrow <- max(vapply(es$tables, \(table) nrow(table$df), integer(1)))
        expect_true(all(non_parent_nrows == max_table_nrow))
      }
    }
    forall(
      gen_df(6, 7, nonempty = TRUE),
      all_non_parents_in_reduction_have_same_nrow
    )
  })
})
