library(hedgehog)

describe("cross_reference", {
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
    links_by_exactly_one_parent_key <- function(df) {
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
    forall(
      gen_df(6, 7, nonempty = TRUE, remove_dup_rows = TRUE),
      links_by_exactly_one_parent_key
    )
  })
  it("leaves a single ultimate child if given a lossless decomposition", {
    single_ultimate_child_if_lossless_decomposition <- function(df) {
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
    forall(
      gen_df(6, 7, nonempty = TRUE, remove_dup_rows = TRUE),
      single_ultimate_child_if_lossless_decomposition
    )
  })
  it("leaves no stranded tables if given a lossless decomposition", {
    no_stranded_if_lossless_decomposition <- function(df) {
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
    forall(
      gen_df(6, 7, nonempty = FALSE, remove_dup_rows = TRUE),
      no_stranded_if_lossless_decomposition
    )
  })
  it("reintroduces attributes not in dependencies if ensuring lossless", {
    reintroduces_missing_attrs_if_lossless <- function(df) {
      deps <- dfd(df, 1)
      lone_attr <- LETTERS[length(deps$attrs) + 1]
      deps$dependencies <- c(deps$dependencies, setNames(list(list()), lone_attr))
      deps$attrs <- c(deps$attrs, lone_attr)
      scheme <- normalise(flatten(deps))
      linked <- cross_reference(scheme, ensure_lossless = TRUE)
      expect_true(lone_attr %in% unlist(linked$attrs))
    }
    forall(
      gen_df(6, 7, remove_dup_rows = TRUE),
      reintroduces_missing_attrs_if_lossless
    )
  })
  it("only return non-extraneous table relationships", {
    only_returns_non_extraneous_relationships <- function(df) {
      scheme <- normalise(flatten(dfd(df, 1)))
      linked <- cross_reference(scheme, ensure_lossless = TRUE)
      table_relationships <- unique(lapply(linked$relationships, `[[`, 1))
      table_relationships <- list(
        determinant_sets = lapply(table_relationships, `[`, 1),
        dependents = vapply(table_relationships, `[`, integer(1), 2)
      )
      expect_identical(
        remove_extraneous_dependencies(table_relationships),
        table_relationships
      )
    }
    forall(
      gen_df(6, 7, nonempty = FALSE, remove_dup_rows = TRUE),
      only_returns_non_extraneous_relationships
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
    returns_itself <- function(relation) {
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
    forall(gen.relation, returns_itself)
  })
})
