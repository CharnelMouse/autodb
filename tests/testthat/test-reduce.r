library(hedgehog)

describe("reduce.database", {
  it("is idempotent", {
    has_idempotent_reduction <- function(df) {
      database <- autodb(as.data.frame(df), 1, ensure_lossless = FALSE)
      once <- reduce(database)
      twice <- reduce(once)
      expect_identical(twice, once)
    }
    forall(gen_df(6, 7), has_idempotent_reduction)
  })
  it("removes added relations with less rows than existing non-parent relations", {
    removes_added_non_parent_with_non_maximum_nrow <- function(df) {
      database <- autodb(df, 1, ensure_lossless = TRUE)
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
      gen_df(6, 7, minrow = 1L),
      removes_added_non_parent_with_non_maximum_nrow
    )
  })
  it("returns a subset", {
    reduced_to_subset <- function(df) {
      database <- autodb(df, 1, ensure_lossless = FALSE)
      reduced <- reduce(database)
      expect_identical(reduced$name, database$name)
      expect_true(all(reduced$relations %in% database$relations))
      expect_true(all(reduced$relationships %in% database$relationships))
    }
    forall(gen_df(6, 7, minrow = 1L), reduced_to_subset)
  })
  it("returns a database where non-parent relations have the same maximal number of rows", {
    all_non_parents_in_reduction_have_same_nrow <- function(df) {
      database <- autodb(df, 1, ensure_lossless = FALSE)
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
      gen_df(6, 7, minrow = 1L),
      all_non_parents_in_reduction_have_same_nrow
    )
  })
})

describe("reduce.database_schema", {
  it("is idempotent", {
    has_idempotent_reduction <- function(df) {
      database_schema <- dfd(as.data.frame(df), 1) |>
        flatten() |>
        normalise() |>
        cross_reference(ensure_lossless = TRUE)
      once_schema <- reduce(database_schema, database_schema$relation_names[1L])
      twice_schema <- reduce(once_schema, database_schema$relation_names[1L])
      expect_identical(twice_schema, once_schema)
    }
    forall(gen_df(6, 7), has_idempotent_reduction)
  })
  it("removes added relations with less rows than existing non-parent relations", {
    removes_added_non_parent_with_non_maximum_nrow <- function(df) {
      database_schema <- dfd(df, 1) |>
        flatten() |>
        normalise() |>
        cross_reference(ensure_lossless = TRUE)
      once <- reduce(database_schema, database_schema$relation_names[1L])
      once_plus_small <- once
      once_plus_small$attrs <- c(once_plus_small$attrs, list("extra_attr"))
      once_plus_small$keys <- c(once_plus_small$keys, list(list("extra_attr")))
      once_plus_small$parents <- c(once_plus_small$parents, list(integer()))
      once_plus_small$relation_names <- c(
        once_plus_small$relation_names,
        "extra_table"
      )
      once_plus_small$all_attrs <- c(once_plus_small$all_attrs, "extra_attr")
      twice <- reduce(once_plus_small, database_schema$relation_names[1L])
      twice_minus_small_attr <- twice
      twice_minus_small_attr$all_attrs <- setdiff(twice$all_attrs, "extra_attr")
      expect_identical(twice_minus_small_attr, once)
    }
    forall(
      gen_df(6, 7, minrow = 1L),
      removes_added_non_parent_with_non_maximum_nrow
    )
  })
  it("returns a subset", {
    reduced_to_subset <- function(df) {
      database_schema <- dfd(df, 1) |>
        flatten() |>
        normalise() |>
        cross_reference(ensure_lossless = TRUE)
      reduced <- reduce(database_schema, database_schema$relation_names[1L])
      kept <- match(reduced$relation_names, database_schema$relation_names)
      expect_true(all(!is.na(kept)))
      expect_true(!anyDuplicated(kept))
      expect_identical(reduced$attrs, database_schema$attrs[kept])
      expect_identical(reduced$keys, database_schema$keys[kept])
      expect_identical(
        lapply(reduced$parents, \(p) reduced$relation_names[p]),
        lapply(database_schema$parents[kept], \(p) database_schema$relation_names[p])
      )
      expect_identical(
        lapply(
          reduced$relationships,
          \(r) {r[[1]] <- reduced$relation_names[r[[1]]]; r}
        ),
        lapply(
          Filter(
            \(r) all(r[[1]] %in% kept) && r[[2]] %in% reduced$relation_names,
            database_schema$relationships
          ),
          \(r) {r[[1]] <- database_schema$relation_names[r[[1]]]; r}
        )
      )
      expect_identical(reduced$all_attrs, database_schema$all_attrs)
    }
    forall(gen_df(6, 7, minrow = 1L), reduced_to_subset)
  })
})
