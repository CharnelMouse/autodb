library(hedgehog)

describe("reduce", {
  it("is idempotent", {
    has_idempotent_reduction <- function(df) {
      database <- autonorm(as.data.frame(df), 1, ensure_lossless = FALSE)
      once <- reduce(database)
      twice <- reduce(once)
      expect_identical(twice, once)
    }
    forall(gen_df(6, 7), has_idempotent_reduction)
  })
  it("removes added relations with less rows than existing non-parent relations", {
    removes_added_non_parent_with_non_maximum_nrow <- function(df) {
      database <- autonorm(df, 1, ensure_lossless = TRUE)
      once <- reduce(database)
      once_plus_small <- once
      once_plus_small$relations <- c(
        once_plus_small$relations,
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
      database <- autonorm(df, 1, ensure_lossless = FALSE)
      reduced <- reduce(database)
      expect_identical(reduced$name, database$name)
      expect_true(all(reduced$relations %in% database$relations))
      expect_true(all(reduced$relationships %in% database$relationships))
    }
    forall(gen_df(6, 7, nonempty = TRUE), reduced_to_subset)
  })
  it("returns a database where non-parent relations have the same maximal number of rows", {
    all_non_parents_in_reduction_have_same_nrow <- function(df) {
      database <- autonorm(df, 1, ensure_lossless = FALSE)
      if (length(database$relations) == 0)
        succeed()
      else{
        reduced <- reduce(database)
        non_parents <- setdiff(
          names(reduced$relations),
          vapply(reduced$relationships, `[`, character(1), 3)
        )
        non_parent_nrows <- vapply(
          reduced$relations[non_parents],
          \(table) nrow(table$df),
          integer(1)
        )
        max_table_nrow <- max(vapply(database$relations, \(table) nrow(table$df), integer(1)))
        expect_true(all(non_parent_nrows == max_table_nrow))
      }
    }
    forall(
      gen_df(6, 7, nonempty = TRUE),
      all_non_parents_in_reduction_have_same_nrow
    )
  })
})
