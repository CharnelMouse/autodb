describe("normalise", {
  it("gives valid schemas with same-attributes reference pairs", {
    # table_dum and table_dee
    empty_fds <- functional_dependency(list(), attrs_order = character())
    empty_schema <- normalise(empty_fds)
    is_valid_database_schema(
      empty_schema,
      same_attr_name = TRUE,
      single_key_pairs = FALSE
    )

    forall(
      list(
        gen_flat_deps(7, 20, to = 20L),
        gen.element(c(FALSE, TRUE)),
        gen.element(c(FALSE, TRUE)),
        gen.element(c(FALSE, TRUE))
      ),
      \(fds, sng, el, ra) {
        normalise(fds, single_ref = sng, ensure_lossless = el, remove_avoidable = ra) |>
          is_valid_database_schema(
            same_attr_name = TRUE,
            single_key_pairs = sng
          )
      },
      curry = TRUE
    )
  })
  it("is the same as synthesise >> autoref", {
    forall(
      gen_flat_deps(7, 6, to = 6L),
      expect_biidentical(
        normalise,
        synthesise %>>% autoref
      )
    )
  })
  it("adds table with key with attributes in original order", {
    adds_ordered_primary_keys <- function(fds) {
      schema <- normalise(fds, ensure_lossless = TRUE)
      all_keys <- unlist(keys(schema), recursive = FALSE)
      key_indices <- lapply(all_keys, match, attrs_order(schema))
      expect_false(any(vapply(key_indices, is.unsorted, logical(1))))
    }
    forall(
      gen_flat_deps(7, 20, to = 20L),
      adds_ordered_primary_keys
    )
  })
  it("is equivalent to removing extraneous attributes separately", {
    forall(
      gen_flat_deps(7, 20, to = 20),
      expect_biidentical(
        normalise,
        remove_extraneous_attributes %>>%
          with_args(normalise, reduce_attributes = FALSE)
      )
    )
  })

  it("returns relations that return themselves if normalised again, if lossless", {
    gen.database_schema_single_lossless <- function(attr_names) {
      gen.choice(
        gen.pure(attr_names),
        gen.pure(attr_names[FALSE]),
        gen.subsequence(attr_names)
      ) |>
        gen.with(function(key) {
          nonkey <- setdiff(attr_names, key)
          attrs <- list(c(key, nonkey))
          keys <- list(list(key))
          nm <- if (length(key) == 0L)
            "constants"
          else
            paste(key, collapse = "_")
          database_schema(
            relation_schema(
              setNames(
                Map(list, attrs, keys),
                nm
              ),
              attrs_order = attr_names
            ),
            list()
          )
        })
    }
    returns_itself <- function(relation) {
      deps <- functional_dependency(
        unlist(synthesised_fds(attrs(relation), keys(relation)), recursive = FALSE),
        attrs_order(relation)
      )
      redo <- normalise(deps)
      expect_identical(redo, relation)
    }
    forall(
      gen.database_schema_single_lossless(letters[1:10]),
      returns_itself
    )
  })
})
