describe("treeSearchJoint", {
  it("works as an algorithm with complete difference sets and pre-defined validation", {
    difference_sets <- function(x) {
      if (nrow(x) == 0)
        return(list())
      lapply(
        seq_len(nrow(x) - 1),
        \(n) {
          lapply(
            setdiff(seq_len(nrow(x)), seq_len(n)),
            \(m) names(x)[as.logical(mapply(Negate(identical), x[n, ], x[m, ]))]
          )
        }
      ) |>
        Reduce(f = c, init = list()) |>
        unique()
    }

    treeSearchJoint_works <- function(x) {
      fds <- discover(x, 1)
      expected <- Map(
        list,
        unique(detset(fds)),
        unname(split(dependant(fds), detset(fds) |> (\(x) match(x, x))()))
      )
      D <- try(difference_sets(x), silent = TRUE)
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
