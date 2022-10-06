library(hedgehog)

describe("reduce", {
  it("is idempotent", {
    forall(
      gen_df(6, 7),
      function(df) {
        es <- autonorm(as.data.frame(df), 1, ensure_lossless = FALSE)
        once <- reduce(es)
        twice <- reduce(once)
        expect_identical(twice, once)
      }
    )
  })
  it("does nothing to a lossless database", {
    forall(
      gen_df(6, 7),
      function(df) {
        es <- autonorm(as.data.frame(df), 1, ensure_lossless = TRUE)
        once <- reduce(es)
        expect_identical(once, es)
      }
    )
  })
  it("removes added tables with less rows than existing non-parent tables", {
    forall(
      gen_df(6, 7, nonempty = TRUE),
      function(df) {
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
    )
  })
  it("returns a subset", {
    forall(
      gen_df(6, 7, nonempty = TRUE),
      function(df) {
        es <- autonorm(df, 1, ensure_lossless = FALSE)
        reduced <- reduce(es)
        expect_identical(reduced$name, es$name)
        expect_true(all(reduced$tables %in% es$tables))
        expect_true(all(reduced$relationships %in% es$relationships))
      }
    )
  })
  it("returns a database where non-parent tables have the same maximal number of rows", {
    forall(
      gen_df(6, 7, nonempty = TRUE),
      function(df) {
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
    )
  })
})
