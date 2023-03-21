library(hedgehog)

describe("functional_dependency", {
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
    gen.fd <- gen.sample(letters, 1) |>
      gen.and_then(\(dependent) {
        list(
          gen.subsequence(setdiff(letters, dependent)),
          dependent
        )
      }) |>
      gen.and_then(\(lst) {
        c(lst, list(gen.sample(unlist(lst))))
      }) |>
      gen.with(\(lst) {
        functional_dependency(list(lst[1:2]), lst[[3]])
      })
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
})

describe("flatten", {
  it("returns a valid functional_dependency", {
    flattens_then <- function(f) {
      function(deps) {
        f(flatten(deps))
      }
    }
    expect_valid_functional_dependency <- function(functional_dependency) {
      expect_s3_class(functional_dependency, "functional_dependency")
      attrs <- attr(functional_dependency, "attrs", exact = TRUE)
      expect_true(all(is.element(unlist(functional_dependency), attrs)))
      determinant_sets <- unlist(
        lapply(functional_dependency, `[[`, 1L),
        recursive = FALSE
      )
      determinant_indices <- lapply(determinant_sets, match, attrs)
      expect_true(all(vapply(
        determinant_indices, Negate(is.unsorted), logical(1)
      )))
    }
    forall(
      gen_df(6, 7) |> gen.with(\(df) dfd(df, 1)),
      flattens_then(expect_valid_functional_dependency)
    )
  })
})
