library(hedgehog)

describe("functional_dependency", {
  gen.fd <- function(x, from, to, unique = TRUE) {
    gen.sample(x, 1) |>
      gen.and_then(\(dependent) {
        list(
          gen.subsequence(setdiff(sample(x), dependent)),
          dependent
        )
      }) |>
      gen.list(from = from, to = to) |>
      gen.and_then(\(lst) {
        c(list(lst), list(gen.sample(unique(unlist(lst)))))
      }) |>
      gen.with(\(lst) {
        functional_dependency(lst[[1]], lst[[2]], unique = unique)
      })
  }
  it("expects valid input: FD elements correct lengths, contain characters of valid lengths", {
    expect_error(
      functional_dependency(list(NULL), character()),
      "^FDs elements must have length two$"
    )
    expect_error(
      functional_dependency(list(list(integer(), "a")), "a"),
      "^FD determinant sets must be characters$"
    )
    expect_error(
      functional_dependency(list(list(character(), 1L)), "1"),
      "^FDs dependents must be length-one characters$"
    )
    expect_error(
      functional_dependency(list(list(character(), character())), character()),
      "^FDs dependents must be length-one characters$"
    )
  })
  it("expects valid input: no duplicate determinants", {
    expect_error(
      functional_dependency(list(list(c("a", "a"), "b")), c("a", "b")),
      "^attributes in determinant sets must be unique$"
    )
  })
  it("expects valid input: all attributes given in attrs", {
    expect_error(
      functional_dependency(list(list(character(), "a")), "b"),
      "^attributes in FDs must be present in attrs$"
    )
  })
  it("returns a set, i.e. no duplicated FD elements", {
    forall(
      gen.fd(letters[1:2], 2, 6),
      Negate(anyDuplicated) %>>% expect_true
    )
  })
  it("orders attributes in each determinant set with respect to order in attrs", {
    detset_attributes_ordered <- function(fds) {
      matches <- vapply(
        fds,
        with_args(`[[`, i = 1L) %>>%
          with_args(match, table = attrs(fds)) %>>%
          (Negate(is.unsorted)),
        logical(1)
      )
      expect_true(all(matches))
    }
    forall(gen.fd(letters[1:6], 0, 8), detset_attributes_ordered)
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
  it("is subsetted to a valid functional dependency object", {
    forall(
      gen.fd(letters[1:6], 0, 8) |>
        gen.and_then(\(fd) list(
          gen.pure(fd),
          gen.sample(c(FALSE, TRUE), length(fd), replace = TRUE)
        )),
      \(fd, i) {
        is_valid_functional_dependency(fd[i])
        is_valid_functional_dependency(fd[which(i)])
        expect_identical(fd[i], fd[which(i)])
        expect_length(fd[i], sum(i))
      },
      curry = TRUE
    )
  })
  it("can be subsetted while preserving attributes", {
    x <- functional_dependency(list(list("a", "b")), letters[1:5])
    expect_identical(x[TRUE], x)
    expect_identical(x[[1]], x)
    expect_error(x[[integer()]])
    expect_error(x[[c(1, 1)]])
  })
  it("can be made unique within class", {
    forall(
      gen.fd(letters[1:6], 0, 8, unique = FALSE),
      unique %>>% class %>>% with_args(expect_identical, "functional_dependency")
    )
  })
  it("concatenates within class", {
    concatenate_within_class <- function(...) {
      expect_identical(class(c(...)), class(..1))
    }
    forall(
      gen.fd(letters[1:6], 0, 8) |> gen.list(from = 1, to = 10),
      concatenate_within_class,
      curry = TRUE
    )
  })
  it("concatenates with duplicates removed", {
    concatenate_unique <- function(...) {
      expect_true(!anyDuplicated(c(...)))
    }
    forall(
      gen.fd(letters[1:6], 0, 8) |> gen.list(from = 1, to = 10),
      concatenate_unique,
      curry = TRUE
    )
  })
  it("concatenates without losing attributes", {
    concatenate_lossless_for_attributes <- function(...) {
      lst <- list(...)
      res <- c(...)
      for (l in lst) {
        expect_true(all(is.element(attrs(l), attrs(res))))
      }
    }
    forall(
      gen.fd(letters[1:6], 0, 8) |> gen.list(from = 1, to = 10),
      concatenate_lossless_for_attributes,
      curry = TRUE
    )
  })
  it("concatenates without losing attribute orderings, if consistent", {
    concatenate_keeps_attribute_order <- function(...) {
      lst <- list(...)
      expect_silent(res <- c(...))
      for (index in seq_along(lst)) {
        expect_identical(
          attrs(lst[[!!index]]),
          intersect(attrs(res), attrs(lst[[!!index]]))
        )
      }
    }

    forall(
      gen.sample(letters[1:8], gen.sample(1:3)) |>
        gen.with(sort %>>% with_args(functional_dependency, FDs = list())) |>
        gen.list(from = 2, to = 5),
      concatenate_keeps_attribute_order,
      curry = TRUE
    )

    # example where attributes aren't consistent, but are pairwise
    deps <- list(
      functional_dependency(list(), c("a", "b")),
      functional_dependency(list(), c("b", "c")),
      functional_dependency(list(), c("c", "a"))
    )
    expect_failure(do.call(concatenate_keeps_attribute_order, deps))

    forall(
      gen.subsequence(letters[1:6]) |>
        gen.with(\(attrs) functional_dependency(list(), attrs)) |>
        gen.list(from = 2, to = 10),
      concatenate_keeps_attribute_order,
      curry = TRUE,
      discard.limit = 10
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
          lapply(unclass(l), \(fd) list(sort(fd[[1]]), fd[[2]])),
          lapply(unclass(res), \(fd) list(sort(fd[[1]]), fd[[2]]))
        )))
      }
    }
    forall(
      gen.fd(letters[1:6], 0, 8) |> gen.list(from = 1, to = 10),
      concatenate_lossless_for_FDs,
      curry = TRUE
    )
  })
  it("is composed of its detset() and dependent() outputs, with attrs() attribute", {
    forall(
      gen.fd(letters[1:6], 0, 8),
      \(fd) expect_identical(
        fd,
        functional_dependency(
          Map(list, detset(fd), dependent(fd)),
          attrs = attrs(fd)
        )
      )
    )
  })
})
