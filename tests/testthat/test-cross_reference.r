library(hedgehog)

describe("cross_reference", {
  gen_ncol_inc <- gen.int(7)
  gen_len_inc <- gen.int(6)
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
  gen_nonempty_df <- generate(for (lst in gen_nonempty_lst) as.data.frame(lst))
  it("returns relationships", {
    df <- data.frame(a = integer(), b = integer(), c = integer())
    norm_deps <- list(
      attrs = list(
        c("a", "b"),
        c("b", "c")
      ),
      keys = list(
        list("a"),
        list("b")
      )
    )
    es <- cross_reference(norm_deps)
    expected_parents = list(2L, integer())
    expected_relations <- list(
      list(c(1L, 2L), "b")
    )
    expect_identical(es$parents, expected_parents)
    expect_identical(es$relationships, expected_relations)
  })
  it("only links children to parents by exactly one parent key", {
    forall(
      gen_nonempty_df,
      function(df) {
        df <- unique(df)
        deps <- dfd(df, 1)
        scheme <- normalise(flatten(deps), check_key = TRUE)
        if (length(scheme$keys) == 1)
          succeed()
        else{
          linked <- cross_reference(scheme)
          relationship_tables <- lapply(linked$relationships, `[[`, 1)
          relationship_attrs <- vapply(linked$relationships, `[[`, character(1), 2)
          link_sets <- tapply(
            relationship_attrs,
            as.data.frame(do.call(rbind, relationship_tables)),
            \(as) sort(unique(as))
          )
          for (column in seq_len(ncol(link_sets))) {
            parent <- strtoi(colnames(link_sets)[column])
            attribute_sets <- link_sets[, column]
            expect_lte(length(setdiff(attribute_sets, linked$keys[[parent]])), 1)
          }
        }
      }
    )
  })
})
