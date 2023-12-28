library(hedgehog)

describe("reduce.database", {
  it("is idempotent", {
    has_idempotent_reduction <- function(df) {
      database <- autodb(as.data.frame(df), ensure_lossless = FALSE)
      once <- reduce(database)
      twice <- reduce(once)
      expect_identical(twice, once)
    }
    forall(gen_df(6, 7), has_idempotent_reduction)
  })
  it("removes added relations with less rows than existing non-parent relations", {
    removes_added_non_parent_with_non_maximum_nrow <- function(df) {
      database <- autodb(df, ensure_lossless = TRUE)
      once <- reduce(database)
      new_relations <- c(
        once,
        list(
          extra_table = list(
            df = data.frame(extra_attr = logical()),
            keys = list("extra_attr")
          )
        )
      )
      once_plus_small <- database(
        relation(
          new_relations,
          attrs_order = c(attrs_order(once), "extra_attr")
        ),
        relationships = relationships(once),
        name(once)
      )
      twice <- reduce(once_plus_small)
      once_plus_attr <- once
      attrs_order(once_plus_attr) <- attrs_order(twice)
      expect_identical(twice, once_plus_attr)
    }
    forall(
      gen_df(6, 7, minrow = 1L, mincol = 1L),
      removes_added_non_parent_with_non_maximum_nrow
    )
  })
  it("returns a subset", {
    reduced_to_subset <- function(df) {
      database <- autodb(df, ensure_lossless = FALSE)
      reduced <- reduce(database)
      expect_identical(name(reduced), name(database))
      expect_true(all(reduced %in% database))
      expect_true(all(relationships(reduced) %in% relationships(database)))
    }
    forall(gen_df(6, 7, minrow = 1L), reduced_to_subset)
  })
  it("returns a database where non-parent relations have the same maximal number of rows", {
    all_non_parents_in_reduction_have_same_nrow <- function(df) {
      database <- autodb(df, ensure_lossless = FALSE)
      if (length(database) == 0)
        succeed()
      else{
        reduced <- reduce(database)
        non_parents <- setdiff(
          names(reduced),
          vapply(relationships(reduced), `[[`, character(1), 3)
        )
        non_parent_nrows <- vapply(
          reduced[non_parents],
          \(table) nrow(table$df),
          integer(1)
        )
        max_table_nrow <- max(vapply(database, \(table) nrow(table$df), integer(1)))
        expect_true(all(non_parent_nrows == max_table_nrow))
      }
    }
    forall(
      gen_df(6, 7, minrow = 1L, mincol = 1L),
      all_non_parents_in_reduction_have_same_nrow
    )
  })
  it("returns at least one relation with maximal number of records, and any parents", {
    contains_maximal_row_relation_and_parents <- function(df) {
      db <- autodb(df, ensure_lossless = TRUE)
      reduced <- reduce(db)
      nrows <- vapply(reduced, \(r) nrow(r$df), integer(1))
      expect_identical(max(nrows), nrow(df))
      base <- names(reduced)[which.max(nrows)]
      parents <- relationships(db) |>
        Filter(f = \(r) r[[1]] == base) |>
        vapply(\(r) r[[3]], character(1))
      expect_true(all(is.element(parents, names(reduced))))
    }
    forall(
      gen_df(6, 7, minrow = 1L, mincol = 1L, remove_dup_rows = TRUE),
      contains_maximal_row_relation_and_parents
    )
  })
})

describe("reduce.database_schema", {
  it("is idempotent", {
    has_idempotent_reduction <- function(df) {
      database_schema <- discover(as.data.frame(df), 1) |>
        normalise(ensure_lossless = TRUE)
      once_schema <- reduce(database_schema, names(database_schema)[[1L]])
      twice_schema <- reduce(once_schema, names(database_schema)[[1L]])
      expect_identical(twice_schema, once_schema)
    }
    forall(gen_df(6, 7), has_idempotent_reduction)
  })
  it("removes added relations with less rows than existing non-parent relations", {
    removes_added_non_parent_with_non_maximum_nrow <- function(df) {
      ds <- discover(df, 1) |>
        normalise(ensure_lossless = TRUE)
      once <- reduce(ds, names(ds)[[1L]])

      once_plus_small <- relation_schema(
        Map(
          list,
          c(attrs(once), list("extra_attr")),
          c(keys(once), list(list("extra_attr"))
          )
        ),
        c(attrs_order(once), "extra_attr")
      ) |>
        database_schema(relationships = relationships(once))

      twice <- reduce(once_plus_small, names(ds)[1L])
      twice_minus_small_attr <- relation_schema(
        Map(list, attrs(twice), keys(twice)),
        setdiff(attrs_order(twice), "extra_attr")
      ) |>
        database_schema(relationships = relationships(twice))
      expect_identical(twice_minus_small_attr, once)
    }

    df <- data.frame(
      a = c(F, F, T, T, NA, NA),
      b = c(1L, 1L, NA, 1L, NA, NA),
      c = c(0L, 0L, 1L, NA, NA, 1L),
      d = c(NA, NA, 0L, 0L, NA, 0L),
      e = c(0L, 1L, 1L, NA, NA, NA)
    )
    removes_added_non_parent_with_non_maximum_nrow(df)

    forall(
      gen_df(6, 7, minrow = 1L, mincol = 1L),
      removes_added_non_parent_with_non_maximum_nrow
    )
  })
  it("returns a subset", {
    reduced_to_subset <- function(df) {
      database_schema <- discover(df, 1) |>
        normalise(ensure_lossless = TRUE)
      reduced <- reduce(database_schema, names(database_schema)[[1L]])
      kept <- match(names(reduced), names(database_schema))
      expect_true(all(!is.na(kept)))
      expect_true(!anyDuplicated(kept))
      expect_identical(attrs(reduced), attrs(database_schema)[kept])
      expect_identical(keys(reduced), keys(database_schema)[kept])
      expect_identical(
        relationships(reduced),
        Filter(
          \(r) all(is.element(c(r[[1]], r[[3]]), names(reduced))),
          relationships(database_schema)
        )
      )
      expect_identical(attrs_order(reduced), attrs_order(database_schema))
    }
    forall(gen_df(6, 7, minrow = 1L, mincol = 1L), reduced_to_subset)
  })
  it("returns a schema with named subschema, and any parents", {
    contains_named_relation_and_parents <- function(df) {
      ds <- discover(df, 1) |>
        normalise(ensure_lossless = TRUE)
      base <- names(ds)[[1]]
      reduced <- reduce(ds, base)
      expect_identical(base, names(ds)[[1]])
      parents <- relationships(ds) |>
        Filter(f = \(r) r[[1]] == base) |>
        vapply(\(r) r[[3]], character(1))
      expect_true(all(is.element(parents, names(reduced))))
    }
    forall(
      gen_df(6, 7, minrow = 1L, mincol = 1L, remove_dup_rows = TRUE),
      contains_named_relation_and_parents
    )
  })
})
