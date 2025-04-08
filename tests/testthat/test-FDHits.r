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
      lookup <- lookup_table(x)
      fds <- discover(x, 1)
      expected <- Map(
        list,
        unique(detset(fds)),
        unname(split(dependant(fds), detset(fds) |> (\(x) match(x, x))()))
      )
      D <- try(difference_sets(lookup_table(x)), silent = TRUE)
      if (class(D)[[1]] == "try-error")
        return(fail(attr(D, "condition")$message))

      observed <- try(treeSearchJoint(x, D), silent = TRUE)
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
      plis <- lapply(lookup, pli)
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

describe("refine_partition", {
  # There's some verbiage in Bleifuss et al. 2024 about using the lookup table
  # to add an attribute to a partition, without any detail. We attempt this with
  # refine_partition below.
  gen.input <- gen_df(6, 7, mincol = 2) |>
    gen.and_then(\(x) {
      gen.element(names(x)) |>
        gen.and_then(\(attr) {
          list(
            gen.pure(lookup_table(x)),
            gen.sample_resampleable(setdiff(names(x), attr), to = ncol(x) - 1),
            gen.pure(attr)
          )
        })
    })
  refine_partition_works <- function(lookup, start_attrs, attr) {
    partition <- unname(split(
      seq_len(nrow(lookup)),
      lookup[, start_attrs, drop = FALSE]
    )) |>
      (\(x) x[lengths(x) > 1])()
    expected <- unname(split(
      seq_len(nrow(lookup)),
      lookup[, c(start_attrs, attr), drop = FALSE]
    )) |>
      (\(x) x[lengths(x) > 1])()
    observed <- try(refine_partition(partition, attr, lookup))
    if (class(observed)[[1]] == "try-error")
      return(fail)
    expect_setequal(observed, expected)
  }
  forall(gen.input, refine_partition_works, curry = TRUE)
})

describe("new_diffset", {
  it("deterministically finds a difference set overlapping with W, not hit by S", {
    gen.violated_fd <- function(lookup) {
      fds <- discover(lookup, 1)
      if (length(fds) == 0) {
        gen.sample_resampleable(names(lookup), to = ncol(lookup)) |>
          gen.and_then(\(nms) {
            list(
              gen.pure(setdiff(names(lookup), nms)),
              gen.pure(intersect(names(lookup), nms))
            )
          })
      }else{
        if (all(lengths(detset(fds)) == 0)) {
          nonconstants <- setdiff(names(lookup), dependant(fds))
          if (length(nonconstants) == 0)
            return(list(gen.pure(NULL), gen.pure(NULL)))
          list(
            gen.pure(character()),
            gen.pure(nonconstants)
          )
        }else{
          gen.element(fds[lengths(detset(fds)) > 0]) |>
            gen.and_then(\(fd) {
              ds <- detset(fd)[[1]]
              gen.sample_resampleable(ds, to = length(ds)) |>
                gen.with(\(remove) {
                  S <- setdiff(ds, remove)
                  list(
                    S,
                    setdiff(names(lookup), S)
                  )
                })
            })
        }
      }
    }
    gen.input <- gen_df(6, 7, minrow = 2, mincol = 1, remove_dup_rows = TRUE) |>
      gen.with(lookup_table) |>
      gen.and_then(\(lookup)
        gen.violated_fd(lookup) |>
          gen.with(\(lst) c(lst, list(lookup)))
      )
    forall(
      gen.input,
      if_discard_else(
        \(S, W, lookup) is.null(S),
        expect_biidentical(new_diffset, new_diffset)
      ),
      curry = TRUE
    )
  })
})
