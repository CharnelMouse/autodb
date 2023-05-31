library(hedgehog)

describe("functional_dependency", {
  gen.fd <- gen.sample(letters, 1) |>
    gen.and_then(\(dependent) {
      list(
        gen.subsequence(setdiff(sample(letters), dependent)),
        dependent
      )
    }) |>
    gen.and_then(\(lst) {
      c(lst, list(gen.sample(unlist(lst))))
    }) |>
    gen.with(\(lst) {
      functional_dependency(list(lst[1:2]), lst[[3]])
    })
  it("expects valid input: FD elements correct lengths, contain characters of valid lengths", {
    expect_error(
      functional_dependency(list(NULL), character()),
      "^FDs elements must have length two$"
    )
    expect_error(
      functional_dependency(list(list(integer(), "a")), character()),
      "^FDs determinant sets must be characters$"
    )
    expect_error(
      functional_dependency(list(list(character(), 1L)), character()),
      "^FDs dependents must be length-one characters$"
    )
    expect_error(
      functional_dependency(list(list(character(), character())), character()),
      "^FDs dependents must be length-one characters$"
    )
  })
  it("expects valid input: all attributes given in attrs", {
    expect_error(
      functional_dependency(list(list(character(), "a")), "b"),
      "^attributes in FDs must be present in attrs$"
    )
  })
  it("orders attributes in each determinant set with respect to order in attrs", {
    detset_attributes_ordered <- function(fds) {
      for (fd in fds) {
        expect_false(is.unsorted(match(fd[[1]], fds$attrs)))
      }
    }
    forall(gen.fd, detset_attributes_ordered, curry = TRUE)
  })
  it("prints", {
    expect_output(
      print(functional_dependency(list(), character())),
      "\\A0 functional dependencies\\n0 attributes\\Z",
      perl = TRUE
    )
    expect_output(
      print(functional_dependency(list(list("a", "b")), c("a", "b"))),
      "\\A1 functional dependency\\n2 attributes: a, b\\na -> b\\Z",
      perl = TRUE
    )
  })
  it("can be subsetted while preserving attributes", {
    x <- functional_dependency(list(list("a", "b")), letters[1:5])
    y <- x[TRUE]
    expect_identical(x, y)
  })
  it("concatenates within class", {
    concatenate_within_class <- function(...) {
      expect_identical(class(c(...)), class(..1))
    }
    forall(gen.list(gen.fd, to = 10), concatenate_within_class, curry = TRUE)
  })
  it("concatenates with duplicates removed", {
    concatenate_unique <- function(...) {
      expect_true(!anyDuplicated(c(...)))
    }
    forall(gen.list(gen.fd, to = 10), concatenate_unique, curry = TRUE)
  })
  it("concatenates without losing attributes", {
    concatenate_lossless_for_attributes <- function(...) {
      lst <- list(...)
      res <- c(...)
      for (l in lst) {
        expect_true(all(is.element(attr(l, "attrs"), attr(res, "attrs"))))
      }
    }
    forall(
      gen.list(gen.fd, to = 10),
      concatenate_lossless_for_attributes,
      curry = TRUE
    )
  })
  it("concatenates without losing attribute orderings, if consistent", {
    remove_inconsistent <- function(lst) {
      pairwise <- outer(
        lst,
        lst,
        Vectorize(\(fd1, fd2) {
          as1 <- attr(fd1, "attrs")
          as2 <- attr(fd2, "attrs")
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
      res <- c(...)
      for (index in seq_along(lst)) {
        expect_identical(
          attr(lst[[!!index]], "attrs"),
          intersect(attr(res, "attrs"), attr(lst[[!!index]], "attrs"))
        )
      }
    }

    forall(
      gen.sample(letters[1:8], gen.sample(1:3)) |>
        gen.and_then(sort %>>% with_args(functional_dependency, FDs = list())) |>
        gen.list(from = 2, to = 5),
      concatenate_keeps_attribute_order,
      curry = TRUE
    )

    forall(
      gen.list(gen.fd, from = 1, to = 10) |>
        gen.and_then(remove_inconsistent),
      concatenate_keeps_attribute_order,
      curry = TRUE
    )
  })
  it("concatenates without losing FDs", {
    concatenate_lossless_for_FDs <- function(...) {
      lst <- list(...)
      res <- c(...)
      for (l in lst) {
        expect_true(all(is.element(
          # sort determinant sets to keep test independent from that for
          # attribute orderings
          lapply(l, \(fd) list(sort(fd[[1]]), fd[[2]])),
          lapply(res, \(fd) list(sort(fd[[1]]), fd[[2]]))
        )))
      }
    }
    forall(
      gen.list(gen.fd, to = 10),
      concatenate_lossless_for_FDs,
      curry = TRUE
    )
  })
})
