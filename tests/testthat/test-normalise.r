library(hedgehog)

describe("normalise", {
  it("doesn't change relation attribute order if dependencies are reordered", {
    df <- data.frame(
      a = rep(1:2, each = 2),
      b = rep(1:2, each = 2),
      c = rep(1:2, each = 2),
      d = rep(1:2, each = 2),
      e = rep(1:2, each = 2),
      f = rep(1:2, each = 2)
    )
    deps <- list(
      dependencies = list(
        a = as.list(letters[1:6][-1]),
        b = as.list(letters[1:6][-2]),
        c = as.list(letters[1:6][-3]),
        d = as.list(letters[1:6][-4]),
        e = as.list(letters[1:6][-5]),
        f = as.list(letters[1:6][-6])
      ),
      attrs = letters[1:6]
    )
    deps <- flatten(deps)
    nds <- normalise(deps)
    forall(
      gen.sample(deps$dependencies, length(deps$dependencies)),
      function(perm) {
        new_deps <- deps
        new_deps$dependencies <- perm
        new_nds <- normalise(new_deps)
        expect_identical(nds, new_nds)
      }
    )
  })
  it("removes extraneous attributes", {
    dependencies <- list(
      dependencies = list(
        list("a", "b"),
        list(c("a", "b"), "c")
      ),
      attrs = c("a", "b", "c")
    )
    norm.dependencies <- normalise(dependencies)
    expect_identical(
      norm.dependencies,
      list(
        attrs = list(c("a", "b", "c")),
        keys = list(list("a"))
      )
    )
  })
  it("removes extraneous dependencies", {
    dependencies <- list(
      dependencies = list(
        list("a", "b"),
        list("a", "c"),
        list("b", "c")
      ),
      attrs = c("a", "b", "c")
    )
    norm.dependencies <- normalise(dependencies)
    expect_identical(
      norm.dependencies,
      list(
        attrs = list(c("a", "b"), c("b", "c")),
        keys = list(list("a"), list("b"))
      )
    )
  })
  it("merges equivalent keys", {
    dependencies <- list(
      dependencies = list(
        list("d", "a"),
        list("d", "b"),
        list("a", "d"),
        list(c("b", "c"), "d")
      ),
      attrs = c("a", "b", "c", "d")
    )
    norm.dependencies <- normalise(dependencies)
    expect_identical(
      norm.dependencies,
      list(
        attrs = list(c("a", "d", "b"), c("b", "c", "a")),
        keys = list(list("a", "d"), list(c("b", "c")))
      )
    )
  })
  it("can handle basic bijections", {
    # A -> B, B -> A, A -> C, B -> C, A -> D, B -> F, D -> E, F -> E
    # => A <-> B, A -> CDF, D -> E, F -> E
    dependencies <- list(
      dependencies = list(
        list("a", "b"),
        list("b", "a"),
        list("a", "c"),
        list("b", "c"),
        list("a", "d"),
        list("b", "f"),
        list("d", "e"),
        list("f", "e")
      ),
      attrs = c("a", "b", "c", "d", "e", "f")
    )
    norm.dependencies <- normalise(dependencies)
    expect_identical(
      norm.dependencies,
      list(
        attrs = list(c("a", "b", "c", "d", "f"), c("d", "e"), c("f", "e")),
        keys = list(list("a", "b"), list("d"), list("f"))
      )
    )
  })
  it("removes transient dependencies after-merging keys (DFD fig. 3)", {
    dependencies <- list(
      dependencies = list(
        list(c("x1", "x2"), "a"),
        list(c("x1", "x2"), "d"),
        list(c("c", "d"), "x1"),
        list(c("c", "d"), "x2"),
        list(c("a", "x1"), "b"),
        list(c("b", "x2"), "c"),
        list("c", "a")
      ),
      attrs = c("x1", "x2", "a", "b", "c", "d")
    )
    norm.dep <- normalise(dependencies)
    expected_attrs <- list(
      c("x1", "x2", "c", "d"), # contains a if trans_deps not removed
      c("a", "x1", "b"),
      c("b", "x2", "c"),
      c("c", "a")
    )
    expected_keys <- list(
      list(c("x1", "x2"), c("c", "d")),
      list(c("a", "x1")),
      list(c("b", "x2")),
      list("c")
    )
    expect_setequal(norm.dep$attrs, expected_attrs)
    expect_setequal(norm.dep$keys, expected_keys)
    expect_identical(
      match(norm.dep$attrs, expected_attrs),
      match(norm.dep$keys, expected_keys)
    )
  })
  it("replaces keys / non-key attributes with their bijection set's chosen index", {
    dependencies <- list(
      dependencies = list(
        list(c("A", "B"), "C"),
        list("C", "A"),
        list("C", "B"),
        list("C", "D"),
        list(c("A", "B", "E"), "F")
      ),
      attrs = c("A", "B", "C", "D", "E", "F")
    )
    norm.dep <- normalise(dependencies)
    expect_identical(
      norm.dep,
      list(
        attrs = list(c("C", "A", "B", "D"), c("C", "E", "F")),
        keys = list(list("C", c("A", "B")), list(c("C", "E")))
      )
    )
  })
  it("adds a key table if one not present, to keep original table reproducible", {
    deps <- list(
      dependencies = list(
        a = list("b"),
        b = list("a"),
        c = list(),
        d = list()
      ),
      attrs = letters[1:4]
    )
    deps <- flatten(deps)
    nds <- normalise(deps)
    expect_setequal(nds$attrs, list(c("a", "b"), c("a", "c", "d")))

    deps <- list(
      dependencies = list(
        a = list(),
        b = list()
      ),
      attrs = letters[1:2]
    )
    deps <- flatten(deps)
    nds <- normalise(deps)
    expect_identical(
      nds,
      list(
        attrs = list(c("a", "b")),
        keys = list(list(c("a", "b")))
      )
    )

    deps <- list(
      dependencies = list(
        a = list(),
        b = list("a", c("d", "e")),
        c = list("a", c("d", "e")),
        d = list("c"),
        e = list()
      ),
      attrs = letters[1:5]
    )
    deps <- flatten(deps)
    nds <- normalise(deps)
    expect_setequal(
      nds$attrs,
      list(c("a", "b", "c"), c("d", "e", "b", "c"), c("c", "d"), c("a", "e"))
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
        list(attrs = list(c(key, nonkey)), keys = list(list(key)))
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
        redo <- normalise(deps)
        expect_length(redo$attrs, 1)
        expect_identical(redo$attrs[[1]], relation$attrs[[1]])
        expect_identical(redo$keys[[1]], relation$keys[[1]])
      }
    )
  })
})

test_that("drop_primary_dups", {
  df <- data.frame(
    city = c(
      'honolulu', 'boston', 'honolulu', 'dallas', 'seattle',
      'honolulu', 'boston', 'honolulu', 'seattle', 'boston'
    ),
    state = c(
      'HI', 'MA', 'HI', 'TX', 'WA',
      'AL', 'MA', 'HI', 'WA', 'NA'
    ),
    is_liberal = c(
      TRUE, TRUE, TRUE, FALSE, TRUE,
      TRUE, TRUE, TRUE, TRUE, FALSE
    )
  )
  new_df <- drop_primary_dups(df, "city")
  df_new_dic <- data.frame(
    city = c("boston", "dallas", "honolulu", "seattle"),
    state = c("MA", "TX", "HI", "WA"),
    is_liberal = c(TRUE, FALSE, TRUE, TRUE)
  )
  expect_identical(new_df, df_new_dic)

  df <- data.frame(
    requires_light = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
    is_dark = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
    light_on = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  new_df <- drop_primary_dups(df, c('requires_light', 'is_dark'))
  expect_false(new_df$light_on[new_df$requires_light & !new_df$is_dark])
  expect_false(new_df$light_on[!new_df$requires_light & new_df$is_dark])
  expect_true(new_df$light_on[new_df$requires_light & new_df$is_dark])
})

describe("keys_order", {
  it("works like order() for single-length elements", {
    forall(
      gen.sample.int(100, 10),
      function(ints) {
        expect_identical(order(ints), keys_order(ints))
      }
    )
  })
  it("gives a sorted list if applied as subsetter", {
    forall(
      gen.sample.int(100, 10),
      function(ints) {
        ord <- keys_order(ints)
        expect_identical(seq_along(ints), keys_order(ints[ord]))
      }
    )
  })
  it("orders by length first", {
    gen_el <- generate(for (n in gen.int(4)) {
      generate(for (start in gen.int(10)) {
        as.integer(start - 1L + seq_len(n))
      })
    })
    gen_lst <- gen.list(gen_el, to = 10)
    forall(
      gen_lst,
      function(lst) {
        ord <- keys_order(lst)
        ord_sorted_within_lengths <- ord |>
          tapply(lengths(lst)[ord], sort, simplify = FALSE) |>
          unlist(use.names = FALSE)
        expect_identical(order(lengths(lst)), ord_sorted_within_lengths)
      }
    )
  })
  it("orders by values, in given order, within lengths", {
    same_length_sorted <- function(lst) {
      len <- length(lst[[1]])
      stopifnot(all(lengths(lst) == len))
      if (len == 0)
        return(TRUE)
      firsts <- vapply(lst, `[`, integer(1), 1)
      if (is.unsorted(firsts))
        return(FALSE)
      rest <- lapply(lst, `[`, -1)
      all(tapply(rest, firsts, \(x) length(x) <= 1 || same_length_sorted(x)))
    }
    gen_el <- generate(for (n in gen.int(5)) {
      gen.sample.int(2, n, replace = TRUE)
    })
    gen_lst <- gen.list(gen_el, to = 10)
    forall(
      gen_lst,
      function(lst) {
        ord <- keys_order(lst)
        sorted_keys <- lst[ord]
        sorted_lengths <- lengths(lst)[ord]
        expect_true(all(tapply(sorted_keys, sorted_lengths, same_length_sorted)))
      }
    )
  })
})
