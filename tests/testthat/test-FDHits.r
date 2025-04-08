difference_sets <- function(lookup) {
  if (nrow(lookup) == 0)
    return(list())
  lapply(
    seq_len(nrow(lookup) - 1),
    \(n) {
      lapply(
        setdiff(seq_len(nrow(lookup)), seq_len(n)),
        \(m) names(lookup)[as.logical(lookup[n, ] != lookup[m, ])]
      )
    }
  ) |>
    Reduce(f = c, init = list()) |>
    unique()
}

describe("treeSearchJoint", {
  it("works as an algorithm with complete difference sets and pre-defined validation", {
    treeSearchJoint_works <- function(x) {
      fds <- discover(x, 1)
      expected <- Map(
        list,
        unique(detset(fds)),
        unname(split(dependant(fds), detset(fds) |> (\(x) match(x, x))()))
      )
      D <- try(difference_sets(lookup_table(x)), silent = TRUE)
      if (class(D)[[1]] == "try-error")
        return(fail(attr(D, "condition")$message))

      observed <- try(treeSearchJoint(names(x), D, expected), silent = TRUE)
      if (class(observed)[[1]] == "try-error")
        return(fail(attr(observed, "condition")$message))
      observed_fds <- observed |>
        lapply(\(x) lapply(x[[2]], \(y) list(x[[1]], y))) |>
        Reduce(f = c, init = list()) |>
        functional_dependency(names(x))
      expect_setequal(observed_fds, fds)
    }

    # example from original paper
    treeSearchJoint_works(data.frame(
      Room_Nr = c(101L, 101L, 102L, 101L),
      Time = c("Wed 10:00 am", "Wed 02:00 pm", "Fri 02:00 pm", "Fri 02:00 pm"),
      Course = c("Programming", "Databases"),
      Lecturer = c("Miako", "Daniel", "Miako", "Saurabh")
    ))

    forall(gen_df(6, 7, remove_dup_rows = FALSE), treeSearchJoint_works)
  })
})

describe("sample_diffsets", {
  it("samples distinct pairs of rows that agree on a given attribute", {
    sample_diffsets_works <- function(x) {
      lookup <- lookup_table(x)
      # PLIs are just single-attribute (stripped) partitions
      plis <- lapply(
        lookup,
        \(indices) {
          split(seq_len(nrow(lookup)), indices) |>
            (\(vec) vec[lengths(vec) > 1])() |>
            unname()
        }
      )
      all_diff <- difference_sets(lookup)
      expect_true(all(sapply(
        names(plis),
        \(nm) all(is.element(
          sample_diffsets(plis[[nm]], lookup),
          Filter(\(d) !is.element(nm, d), all_diff)
        ))
      )))
      # epsilon = TRUE => samples all pairs => finds all difference sets
      expect_true(all(sapply(
        names(plis),
        \(nm) setequal(
          sample_diffsets(plis[[nm]], lookup, 1),
          Filter(\(d) !is.element(nm, d), all_diff)
        )
      )))
    }
    forall(gen_df(6, 7), sample_diffsets_works)
  })
})
