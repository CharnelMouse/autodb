library(hedgehog)

describe("relation_schema", {
  it("expects valid input: schemas is a named list", {
    expect_error(relation_schema(1L, character()))
    expect_error(
      relation_schema(list(), character()),
      "^relation schemas must be named$"
    )
  })
  it("expects valid input: schema elements correct lengths", {
    expect_error(
      relation_schema(list(a = NULL), character()),
      "^schema elements must have length two$"
    )
  })
  it("expects valid input: schema elements contain characters of valid lengths", {
    expect_error(
      relation_schema(list(NULL), character()),
      "^schema elements must have length two$"
    )
    expect_error(
      relation_schema(list(list(integer(), "a")), "a"),
      "^schema attribute sets must be characters$"
    )
    expect_error(
      relation_schema(list(list(character(), 1L)), "1"),
      "^schema key sets must be lists$"
    )
    expect_error(
      relation_schema(list(list(character(), list())), "1"),
      "^schema key sets must have at least one element$"
    )
    expect_error(
      relation_schema(list(list(character(), list(1L))), "1"),
      "^schema key sets must have character elements$"
    )
    expect_error(
      relation_schema(list(list(character(), list(character()))), 1L),
      "^expected character attrs_order$"
    )
  })
  it("expects valid input: unique schema names", {
    expect_error(
      relation_schema(
        list(
          a = list(character(), list(character())),
          a = list(character(), list(character()))
        ),
        character()
      ),
      "^relation schema names must be unique$"
    )
  })
  it("expects valid input: non-empty schema names", {
    expect_error(
      relation_schema(
        setNames(
          list(
            a = list(character(), list(character())),
            b = list(character(), list(character()))
          ),
          c("", "b")
        ),
        character()
      ),
      "^relation schema names must be non-empty"
    )
  })
  it("expects valid input: no duplicate attrs", {
    expect_error(
      relation_schema(list(a = list(c("a", "a"), list("a"))), "a"),
      "^relation attributes must be unique$"
    )
  })
  it("expects valid input: no duplicate attrs in keys", {
    expect_error(
      relation_schema(list(a = list("a", list(c("a", "a")))), "a"),
      "^relation key attributes must be unique$"
    )
  })
  it("expects valid input: no duplicate attrs_order", {
    expect_error(
      relation_schema(list(a = list("a", list("a"))), c("a", "a")),
      "^attrs_order must be unique$"
    )
  })
  it("expects valid input: all attributes given in attrs_order", {
    expect_error(
      relation_schema(list(a = list("a", list("a"))), "b"),
      "^attributes in schema must be present in attrs_order$"
    )
  })
  it("expects valid input: key attributes are in relation", {
    expect_error(
      relation_schema(list(a = list("a", list("b"))), c("a", "b")),
      "^attributes in keys must be present in relation$"
    )
  })

  it("returns a valid relation schema", {
    forall(
      gen.relation_schema(letters[1:2], 2, 20),
      is_valid_relation_schema
    )
  })
  it("orders key attributes with respect to order in attrs_order", {
    key_attributes_ordered <- function(rs) {
      keys_matches <- vapply(
        keys(rs),
        with_args(
          vapply,
          with_args(match, table = attrs_order(rs)) %>>%
            (Negate(is.unsorted)),
          logical(1)
        ) %>>%
          all,
        logical(1)
      )
      expect_true(all(keys_matches))
    }
    forall(gen.relation_schema(letters[1:6], 1, 8), key_attributes_ordered)
  })
  it("orders attributes with respect to appearance in keys, then by attrs_order", {
    attributes_ordered <- function(rs) {
      attr_matches <- vapply(
        seq_along(rs),
        \(n) {
          as <- attrs(rs)[[n]]
          ks <- keys(rs)[[n]]
          key_given <- unique(unlist(ks))
          rest <- setdiff(as, key_given)
          identical(
            as,
            c(
              key_given,
              rest[order(match(rest, attrs_order(rs)))]
            )
          )
        },
        logical(1)
      )
      expect_true(all(attr_matches))
    }
    forall(gen.relation_schema(letters[1:6], 1, 8), attributes_ordered)
  })

  it("is subsetted to a valid relation schema, follows usual subsetting rules", {
    forall(
      gen.relation_schema(letters[1:6], 0, 8) |>
        gen.and_then(\(rs) list(
          gen.pure(rs),
          gen.sample_resampleable(c(FALSE, TRUE), of = length(rs))
        )),
      \(rs, i) {
        is_valid_relation_schema(rs[i])

        inum <- which(i)
        is_valid_relation_schema(rs[inum])
        expect_identical(rs[i], rs[inum])

        ineg <- -setdiff(seq_along(rs), inum)
        if (!all(i)) {
          is_valid_relation_schema(rs[ineg])
          expect_identical(rs[i], rs[ineg])
        }

        is_valid_relation_schema(rs[names(rs)[i]])
        expect_identical(rs[i], rs[names(rs)[i]])

        expect_length(rs[i], sum(i))

        ints <- stats::setNames(seq_along(rs), names(rs))
        expect_identical(rs[i], rs[ints[i]])
        expect_identical(rs[ineg], rs[ints[ineg]])
        expect_identical(rs[names(rs)[i]], rs[names(rs)[ints[i]]])
      },
      curry = TRUE
    )
    forall(
      gen.relation_schema(letters[1:6], 1, 8) |>
        gen.and_then(\(rs) list(
          gen.pure(rs),
          gen.element(seq_along(rs))
        )),
      \(rs, inum) {
        is_valid_relation_schema(rs[[inum]])
        expect_identical(rs[inum], rs[[inum]])

        ineg <- -setdiff(seq_along(rs), inum)
        if (length(ineg) == 1) {
          is_valid_relation_schema(rs[[ineg]])
          expect_identical(rs[inum], rs[[ineg]])
        }

        is_valid_relation_schema(rs[[names(rs)[[inum]]]])
        expect_identical(rs[inum], rs[[names(rs)[[inum]]]])

        is_valid_relation_schema(eval(rlang::expr(`$`(rs, !!names(rs)[[inum]]))))
        expect_identical(rs[inum], eval(rlang::expr(`$`(rs, !!names(rs)[[inum]]))))

        ints <- stats::setNames(seq_along(rs), names(rs))
        expect_identical(rs[[inum]], rs[[ints[[inum]]]])
        expect_identical(
          tryCatch(rs[[ineg]], error = function(e) e$message),
          tryCatch(rs[[ints[[ineg]]]], error = function(e) e$message)
        )
        expect_identical(rs[[names(rs)[[inum]]]], rs[[names(rs)[[ints[[inum]]]]]])
      },
      curry = TRUE
    )
    forall(
      gen.relation_schema(letters[1:6], 1, 8),
      \(rs) {
        expect_identical(rs[[TRUE]], rs[[1]])
      }
    )
    forall(
      gen.relation_schema(letters[1:6], 1, 8) |>
        gen.and_then(\(rs) list(
          rs = gen.pure(rs),
          indices = gen.sample_resampleable(
            seq_along(rs),
            from = 2,
            to = 2*length(rs)
          )
        )),
      \(rs, indices) {
        is_valid_relation_schema(rs[indices])
      },
      curry = TRUE
    )
  })
  it("can be subsetted while preserving attributes", {
    x <- relation_schema(list(a = list(c("a", "b"), list("a"))), letters[1:5])
    expect_identical(x[TRUE], x)
    expect_identical(
      x[FALSE],
      relation_schema(setNames(list(), character()), letters[1:5])
    )
    expect_identical(x[[1]], x)
    expect_error(x[[integer()]])
    expect_error(x[[c(1, 1)]])
  })

  it("is made unique to a valid relation schema", {
    forall(
      gen.relation_schema(letters[1:6], 0, 8),
      unique %>>% is_valid_relation_schema
    )
  })
  it("is made unique with no duplicate schemas", {
    forall(
      gen.relation_schema(letters[1:6], 1, 8),
      \(rs) {
        rs2 <- c(rs, rs)
        expect_false(Negate(anyDuplicated)(rs2))
        expect_true(Negate(anyDuplicated)(unique(rs2)))
      }
    )
  })

  it("concatenates to a valid relation schema", {
    forall(
      gen.relation_schema(letters[1:6], from = 0, to = 4) |>
        gen.list(from = 1, to = 3),
      c %>>% is_valid_relation_schema,
      curry = TRUE
    )
  })
  it("concatenates with duplicates preserved", {
    forall(
      gen.relation_schema(letters[1:6], 1, 8) |>
        gen.with(\(rs) list(rs, rs)),
      \(lst) {
        expect_length(do.call(c, lst), sum(lengths(lst)))
      }
    )
  })
  it("concatenates without losing attributes", {
    concatenate_lossless_for_attrs_order <- function(...) {
      lst <- list(...)
      res <- c(...)
      for (l in lst) {
        expect_true(all(is.element(attrs_order(l), attrs_order(res))))
      }
    }
    forall(
      gen.relation_schema(letters[1:6], from = 0, to = 8) |>
        gen.list(from = 1, to = 10),
      concatenate_lossless_for_attrs_order,
      curry = TRUE
    )
  })
  it("concatenates without losing attribute orderings, if consistent", {
    concatenate_keeps_attribute_order <- function(...) {
      lst <- list(...)
      expect_silent(res <- c(...))
      for (index in seq_along(lst)) {
        expect_identical(
          attrs_order(lst[[!!index]]),
          intersect(attrs_order(res), attrs_order(lst[[!!index]]))
        )
      }
    }

    forall(
      gen.sample(letters[1:8], gen.element(1:3)) |>
        gen.with(
          sort %>>%
            with_args(relation_schema, schema = setNames(list(), character()))
        ) |>
        gen.list(from = 2, to = 5),
      concatenate_keeps_attribute_order,
      curry = TRUE
    )

    # example where attributes aren't consistent, but are pairwise
    schemas <- list(
      relation_schema(setNames(list(), character()), c("a", "b")),
      relation_schema(setNames(list(), character()), c("b", "c")),
      relation_schema(setNames(list(), character()), c("c", "a"))
    )
    expect_failure(do.call(concatenate_keeps_attribute_order, schemas))

    forall(
      gen.subsequence(letters[1:6]) |>
        gen.with(\(attrs) relation_schema(setNames(list(), character()), attrs)) |>
        gen.list(from = 2, to = 10),
      concatenate_keeps_attribute_order,
      curry = TRUE
    )
  })
  it("concatenates without losing schemas", {
    concatenate_lossless_for_schemas <- function(...) {
      lst <- list(...)
      res <- c(...)
      for (l in lst) {
        sorted <- l
        # sort attrs to keep test independent from that for
        # attribute orderings
        attrs_order(sorted) <- attrs_order(res)
        expect_true(all(is.element(
          sorted,
          res
        )))
      }
    }
    forall(
      gen.relation_schema(letters[1:6], from = 0, to = 8) |>
        gen.list(from = 1, to = 10),
      concatenate_lossless_for_schemas,
      curry = TRUE
    )
  })

  it("can have empty-key schemas merged", {
    up_to_one_empty_key <- function(rs) {
      if (sum(vapply(keys(rs), identical, logical(1), list(character()))) <= 1)
        discard()
      res <- merge_empty_keys(rs)
      is_valid_relation_schema(rs)
      expect_lte(
        sum(vapply(keys(res), identical, logical(1), list(character()))),
        1L
      )
    }
    forall(
      gen.relation_schema_empty_keys(letters[1:6], 1, 8, min_empty = 1),
      up_to_one_empty_key
    )
  })
  it("is composed of its attrs(), keys(), names(), and attrs_order()", {
    forall(
      gen.relation_schema(letters[1:6], 0, 8),
      \(rs) expect_identical(
        rs,
        relation_schema(
          setNames(Map(list, attrs(rs), keys(rs)), names(rs)),
          attrs_order = attrs_order(rs)
        )
      )
    )
  })

  it("is created with logical attribute classes", {
    forall(
      gen.relation_schema(letters[1:6], 1, 8),
      \(rs) {
        r <- create(rs)
        classes <- unlist(
          unname(lapply(records(r), \(df) lapply(df, class))),
          recursive = FALSE
        )
        if (is.null(names(classes)))
          names(classes) <- character()
        expect_identical(
          unname(classes),
          rep(list("logical"), length(classes))
        )
      }
    )
  })

  it("can have its attributes renamed", {
    forall(
      gen.relation_schema(letters[1:6], 1, 8),
      function(rs) {
        rs2 <- rename_attrs(rs, toupper(attrs_order(rs)))
        expect_identical(
          rs2,
          relation_schema(
            setNames(
              Map(
                list,
                lapply(attrs(rs), toupper),
                lapply(keys(rs), lapply, toupper)
              ),
              names(rs)
            ),
            attrs_order = toupper(attrs_order(rs))
          )
        )
      }
    )
  })

  it("prints", {
    expect_output(
      print(relation_schema(setNames(list(), character()), character())),
      "\\A0 relation schemas\\n0 attributes\\Z",
      perl = TRUE
    )
    expect_output(
      print(relation_schema(
        list(a = list(c("a", "b"), list("a"))),
        c("a", "b")
      )),
      paste0(
        "\\A",
        "1 relation schema",
        "\\n",
        "2 attributes: a, b",
        "\\n",
        "schema a: a, b\\n  key 1: a",
        "\\Z"
      ),
      perl = TRUE
    )
  })
})
