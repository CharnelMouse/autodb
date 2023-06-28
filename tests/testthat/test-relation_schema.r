library(hedgehog)

describe("relation_schema", {
  gen.rs_inputs <- function(x, from, to) {
    gen.sample(x, gen.sample(0:length(x), 1)) |>
      gen.and_then(\(attrs) {
        list(
          attrs,
          gen.sample(attrs, gen.sample(0:length(attrs), 1)) |>
            gen.list(to = 3) |>
            gen.with(\(keys) {
              uniq <- unique(keys)
              superset <- outer(
                uniq,
                uniq,
                Vectorize(\(sup, sub) {
                  all(is.element(sub, sup)) && !all(is.element(sup, sub))
                })
              )
              rem <- uniq[!apply(superset, 1, any)]
              rem[keys_order(lapply(rem, match, attrs))]
            })
        )
      }) |>
      gen.list(from = from, to = to) |>
      gen.with(\(lst) {
        # only one schema can have an empty key
        rels_with_empty_keys <- which(vapply(
          lst,
          \(schema) any(lengths(schema[[2]]) == 0L),
          logical(1)
        ))
        if (length(rels_with_empty_keys) > 1L)
          lst <- lst[-rels_with_empty_keys[-1]]

        nms <- make.names(
          vapply(lst, \(rel) name_dataframe(rel[[2]][[1]]), character(1)),
          unique = TRUE
        )
        list(setNames(lst, nms), x)
      })
  }
  gen.rs <- function(x, from, to, unique = TRUE) {
    gen.rs_inputs(x, from, to) |>
      gen.with(\(lst) do.call(relation_schema, c(lst, list(unique = unique))))
  }
  it("generates valid relation schemas", {
    forall(gen.rs(letters[1:6], 0, 8), is_valid_relation_schema)
  })
  it("expects valid input: schema elements correct lengths, contain characters of valid lengths", {
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
  it("expect valid input: named schemas", {
    expect_error(
      relation_schema(list(), character()),
      "^relations must be named$"
    )
  })
  it("expect valid input: unique schema names", {
    expect_error(
      relation_schema(
        list(
          a = list(character(), list(character())),
          a = list(character(), list(character()))
        ),
        character()
      ),
      "^relation names must be unique$"
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
  it("returns a set, i.e. no duplicated relations, when unique = TRUE (default)", {
    forall(
      gen.rs_inputs(letters[1:2], 2, 20),
      (\(lst) do.call(relation_schema, lst)) %>>%
        Negate(anyDuplicated) %>>%
        expect_true
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
    forall(gen.rs(letters[1:6], 1, 8), key_attributes_ordered)
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
    forall(gen.rs(letters[1:6], 1, 8), attributes_ordered)
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
  it("can be made unique within class", {
    forall(
      gen.rs(letters[1:6], 0, 8, unique = FALSE),
      unique %>>% class %>>% with_args(expect_identical, "relation_schema")
    )
  })
  it("concatenates within class", {
    concatenate_within_class <- function(...) {
      expect_identical(class(c(...)), class(..1))
    }
    forall(
      gen.rs(letters[1:6], 0, 8) |> gen.list(from = 1, to = 10),
      concatenate_within_class,
      curry = TRUE
    )
  })
  it("concatenates with duplicates removed", {
    concatenate_unique <- function(...) {
      expect_true(!anyDuplicated(c(...)))
    }
    forall(
      gen.rs(letters[1:6], 0, 8) |> gen.list(from = 1, to = 10),
      concatenate_unique,
      curry = TRUE
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
      gen.rs(letters[1:6], 0, 8) |> gen.list(from = 1, to = 10),
      concatenate_lossless_for_attrs_order,
      curry = TRUE
    )
  })
  it("concatenates without losing attribute orderings, if consistent", {
    remove_inconsistent <- function(lst) {
      pairwise <- outer(
        lst,
        lst,
        Vectorize(\(fd1, fd2) {
          as1 <- attrs(fd1)
          as2 <- attrs(fd2)
          one_in_two <- match(as1, as2)
          two_in_one <- match(as2, as1)
          !is.unsorted(one_in_two, na.rm = TRUE) &&
            !is.unsorted(two_in_one, na.rm = TRUE)
        })
      )
      # remove if ordering inconsistent with earlier ones
      lst[apply(pairwise | upper.tri(pairwise), 1, all)]
    }
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
      gen.sample(letters[1:8], gen.sample(1:3)) |>
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
      sorted_joined <- lapply(
        unclass(res),
        \(schema) list(sort(schema[[1]]), lapply(schema[[2]], sort))
      )
      for (l in lst) {
        sorted <- lapply(unclass(l), \(schema) list(sort(schema[[1]]), schema[[2]]))
        expect_true(all(is.element(
          # sort attrs to keep test independent from that for
          # attribute orderings
          sorted,
          sorted_joined
        )))
      }
    }
    forall(
      gen.rs(letters[1:6], 0, 8) |> gen.list(from = 1, to = 10),
      concatenate_lossless_for_schemas,
      curry = TRUE
    )
  })
  it("is composed of its attrs(), keys(), names() and attrs_order()", {
    forall(
      gen.rs(letters[1:6], 0, 8),
      \(rs) expect_identical(
        rs,
        relation_schema(
          setNames(Map(list, attrs(rs), keys(rs)), names(rs)),
          attrs_order = attrs_order(rs)
        )
      )
    )
  })
})
