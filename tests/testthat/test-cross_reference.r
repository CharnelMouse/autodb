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
  gen_df <- generate(for (lst in gen_lst) as.data.frame(lst))

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
        scheme <- normalise(flatten(deps))
        if (length(scheme$keys) <= 1)
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
            attribute_sets <- attribute_sets[!vapply(attribute_sets, is.null, logical(1))]
            expect_lte(length(setdiff(attribute_sets, linked$keys[[parent]])), 1)
          }
        }
      }
    )
  })
  it("leaves a single ultimate child if given a lossless decomposition", {
    forall(
      gen_nonempty_df,
      function(df) {
        df <- unique(df)
        deps <- dfd(df, 1)
        scheme <- normalise(flatten(deps))
        linked <- cross_reference(scheme, ensure_lossless = TRUE)
        if (length(linked$keys) == 1)
          succeed()
        else{
          relationship_tables <- lapply(linked$relationships, `[[`, 1)
          parents <- vapply(relationship_tables, `[`, integer(1), 2)
          children <- vapply(relationship_tables, `[`, integer(1), 1)
          non_parents <- setdiff(children, parents)
          expect_length(non_parents, 1)
        }
      }
    )
  })
  it("leaves no stranded tables if given a lossless decomposition", {
    forall(
      gen_nonempty_df,
      function(df) {
        df <- unique(df)
        deps <- dfd(df, 1)
        scheme <- normalise(flatten(deps))
        linked <- cross_reference(scheme, ensure_lossless = TRUE)
        if (length(linked$keys) == 1)
          succeed()
        else{
          relationship_tables <- lapply(linked$relationships, `[[`, 1)
          stranded <- setdiff(
            seq_along(linked$keys),
            unlist(relationship_tables)
          )
          expect_length(stranded, 0)
        }
      }
    )
  })
  it("reintroduces attributes not in dependencies if ensuring lossless", {
    forall(
      gen_df,
      function(df) {
        df <- unique(df)
        deps <- dfd(df, 1)
        lone_attr <- LETTERS[length(deps$attrs) + 1]
        deps$dependencies <- c(deps$dependencies, setNames(list(list()), lone_attr))
        deps$attrs <- c(deps$attrs, lone_attr)
        scheme <- normalise(flatten(deps))
        linked <- cross_reference(scheme, ensure_lossless = TRUE)
        expect_true(lone_attr %in% unlist(linked$attrs))
      }
    )
  })
  it("returns relations that return themselves if normalised again", {
    gen.keysize <- gen.sample.int(10)
    gen.key <- generate(
      for (keysize in gen.keysize) {
        letters[1:10][sort(sample(1:10, keysize))]
      }
    )
    gen.relation <- generate(
      for (key in gen.key) {
        nonkey <- setdiff(letters[1:10], key)
        structure(
          list(
            attrs = list(c(key, nonkey)),
            keys = list(list(key)),
            parents = list(integer()),
            relationships = list()
          ),
          class = c("database_scheme", "list")
        )
      }
    )
    forall(
      gen.relation,
      function(relation) {
        nonkey <- setdiff(unlist(relation$attrs), unlist(relation$keys))
        deps <- flatten(list(
          dependencies = setNames(
            lapply(
              nonkey,
              \(x) relation$keys[[1]]
            ),
            nonkey
          ),
          attrs = relation$attrs[[1]]
        ))
        redo <- cross_reference(normalise(deps), ensure_lossless = TRUE)
        expect_identical(redo, relation)
        expect_length(redo$attrs, 1)
        expect_identical(redo$attrs[[1]], relation$attrs[[1]])
        expect_identical(redo$keys[[1]], relation$keys[[1]])
      }
    )
  })
})
