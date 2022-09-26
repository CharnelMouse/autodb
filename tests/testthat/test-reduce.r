library(hedgehog)

describe("reduce", {
  gen_ncol_inc <- gen.int(7)
  gen_len_inc <- gen.int(6)
  gen_lst <- generate(
    for (n_col_inc in gen_ncol_inc) {
      generate(
        for (len_inc in gen_len_inc) {
          rep(
            list(gen.sample(c(FALSE, TRUE), len_inc - 1, replace = TRUE)),
            n_col_inc - 1
          ) |>
            setNames(make.unique(rep_len(LETTERS, n_col_inc - 1)))
        }
      )
    }
  )
  gen_nonempty_lst <- generate(
    for (n_col_inc in gen_ncol_inc) {
      generate(
        for (len_inc in gen_len_inc) {
          rep(
            list(gen.sample(c(FALSE, TRUE), len_inc, replace = TRUE)),
            n_col_inc
          ) |>
            setNames(make.unique(rep_len(LETTERS, n_col_inc)))
        }
      )
    }
  )
  gen_df <- generate(for (lst in gen_lst) as.data.frame(lst))
  gen_nonempty_df <- generate(for (lst in gen_nonempty_lst) as.data.frame(lst))
  it("is idempotent", {
    forall(
      gen_df,
      function(df) {
        es <- autonorm(as.data.frame(df), 1)
        once <- reduce(es)
        twice <- reduce(once)
        expect_identical(twice, once)
      }
    )
  })
  it("removes added tables with less rows than existing non-parent tables", {
    forall(
      gen_nonempty_df,
      function(df) {
        es <- autonorm(df, 1)
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
      gen_nonempty_df,
      function(df) {
        es <- autonorm(df, 1)
        reduced <- reduce(es)
        expect_identical(reduced$name, es$name)
        expect_true(all(reduced$tables %in% es$tables))
        expect_true(all(reduced$relationships %in% es$relationships))
      }
    )
  })
  it("returns a database with a single non-parent_table", {
    forall(
      gen_nonempty_df,
      function(df) {
        es <- autonorm(df, 1)
        reduced <- reduce(es)
        non_parents <- setdiff(
          names(reduced$tables),
          vapply(reduced$relationships, `[`, character(1), 3)
        )
        expect_length(non_parents, 1L)
      }
    )
  })
})
