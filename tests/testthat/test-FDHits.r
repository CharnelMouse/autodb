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

describe("FDHits", {
  it("gives a deterministic result, except for per-dependant dependency order", {
    n_copies <- function(n, fn) {
      function(df) {
        do.call(fn, replicate(n, df, simplify = FALSE))
      }
    }
    all_terminate_then <- function(fn) {
      function(...) {
        lst <- list(...)
        for (n in seq_along(lst)) {
          lst[[n]] <- R.utils::withTimeout(
            FDHits(lst[[n]]),
            timeout = 5,
            onTimeout = "silent"
          )
          if (is.null(lst[[n]]))
            return(fail(paste("execution timed out after", timeout, "seconds")))
        }
        do.call(fn, lst)
      }
    }
    expect_all_equiv_deps <- function(...) {
      lst <- list(...)
      attrs_orders <- lapply(lst, attrs_order)
      if (!all(vapply(attrs_orders, identical, logical(1), attrs_orders[[1]])))
        return(fail(paste(
          "attrs_order inconsistent:",
          paste(
            vapply(
              unique(attrs_orders),
              \(x) paste0("{", toString(x), "}"),
              character(1)
            ),
            collapse = ", "
          )
        )))
      if (!all(vapply(lst, setequal, logical(1), lst[[1]])))
        return(fail(paste0(
          "FDs inconsistent:\n",
          paste(
            vapply(
              unique(lst),
              \(x) paste0("{", paste(as.character(x), collapse = "; "), "}"),
              character(1)
            ),
            collapse = "\n"
          )
        )))
      succeed()
    }
    forall(
      gen_df(4, 6),
      n_copies(100, all_terminate_then(expect_all_equiv_deps))
    )
  })
  it("gives the same results as DFD", {
    FDHits_works <- function(x, method) {
      lookup <- lookup_table(x)
      fds <- discover(x, 1)
      expected <- Map(
        list,
        unique(detset(fds)),
        unname(split(dependant(fds), detset(fds) |> (\(x) match(x, x))()))
      )

      observed <- try(FDHits(x, method = method), silent = TRUE)
      if (class(observed)[[1]] == "try-error")
        return(fail(attr(observed, "condition")$message))
      expect_setequal(observed, fds)
    }

    # example from original paper
    FDHits_works(
      data.frame(
        Room_Nr = c(101L, 101L, 102L, 101L),
        Time = c("Wed 10:00 am", "Wed 02:00 pm", "Fri 02:00 pm", "Fri 02:00 pm"),
        Course = c("Programming", "Databases"),
        Lecturer = c("Miako", "Daniel", "Miako", "Saurabh")
      ),
      method = "Sep"
    )
    FDHits_works(
      data.frame(
        Room_Nr = c(101L, 101L, 102L, 101L),
        Time = c("Wed 10:00 am", "Wed 02:00 pm", "Fri 02:00 pm", "Fri 02:00 pm"),
        Course = c("Programming", "Databases"),
        Lecturer = c("Miako", "Daniel", "Miako", "Saurabh")
      ),
      method = "Joint"
    )

    forall(
      list(
        gen_df(6, 7, remove_dup_rows = FALSE),
        gen.element(c("Sep", "Joint"))
      ),
      FDHits_works
    )
  })
})

describe("sample_diffsets", {
  it("samples distinct pairs of rows that agree on a given attribute", {
    sample_diffsets_works <- function(x) {
      lookup <- lookup_table(x)
      # PLIs are just single-attribute (stripped) partitions
      plis <- lapply(lookup, pli)
      all_diff <- difference_sets(lookup) |>
        (\(x) x[lengths(x) > 0])()
      expect_true(all(sapply(
        names(plis),
        \(nm) all(is.element(
          sample_diffsets(plis[[nm]], lookup),
          Filter(\(d) !is.element(nm, d), all_diff) |>
            lapply(match, names(lookup))
        ))
      )))
      # epsilon = TRUE => samples all pairs => finds all difference sets
      expect_true(all(sapply(
        names(plis),
        \(nm) setequal(
          sample_diffsets(plis[[nm]], lookup, 1),
          Filter(\(d) !is.element(nm, d), all_diff) |>
            lapply(match, names(lookup))
        )
      )))
    }
    forall(gen_df(6, 7), sample_diffsets_works)
  })
})

describe("refine_partition", {
  gen.input <- gen_df(6, 7, mincol = 2) |>
    gen.and_then(\(x) {
      gen.element(names(x)) |>
        gen.and_then(\(attr) {
          lookup <- lookup_table(x)
          list(
            gen.pure(lookup),
            gen.sample_resampleable(setdiff(names(x), attr), to = ncol(x) - 1),
            gen.pure(match(attr, names(lookup)))
          )
        })
    })
  refine_partition_works <- function(lookup, start_attrs, attr) {
    indices <- lookup[[attr]]
    partition <- unname(split(
      seq_len(nrow(lookup)),
      lookup[, start_attrs, drop = FALSE]
    )) |>
      (\(x) x[lengths(x) > 1])()
    expected <- unname(split(
      seq_len(nrow(lookup)),
      lookup[, c(start_attrs, names(lookup)[attr]), drop = FALSE]
    )) |>
      (\(x) x[lengths(x) > 1])()
    observed <- try(refine_partition(partition, indices))
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
              gen.pure(match(intersect(names(lookup), nms), names(lookup)))
            )
          })
      }else{
        if (all(lengths(detset(fds)) == 0)) {
          nonconstants <- setdiff(names(lookup), dependant(fds))
          if (length(nonconstants) == 0)
            return(list(gen.pure(NULL), gen.pure(NULL)))
          list(
            gen.pure(character()),
            gen.pure(match(nonconstants, names(lookup)))
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
                    match(setdiff(names(lookup), S), names(lookup))
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
          gen.with(\(lst) {
            S <- lst[[1]]
            W <- lst[[2]]
            Spli <- if (length(S) == 0) {
              list(seq_len(nrow(lookup))) |>
                (\(x) x[lengths(x) > 1])()
            }else{
              unname(fsplit(
                seq_len(nrow(lookup)),
                lookup[, S, drop = FALSE]
              )) |>
                (\(x) x[lengths(x) > 1])()
            }
            refined_partitions <- lapply(
              W,
              \(attr) refine_partition_old(Spli, attr, lookup) |>
                # sort to avoid using is.element or setequal
                (\(x) x[order(vapply(x, `[`, integer(1),1))])()
            )
            c(
              list(Spli),
              list(refined_partitions),
              lst[1],
              list(lookup)
            )
          })
      )
    forall(
      gen.input,
      if_discard_else(
        \(Spli, new_clusters, S, lookup) is.null(S),
        \(Spli, new_clusters, S, lookup) expect_biidentical(new_diffset, new_diffset)(Spli, new_clusters, lookup)
      ),
      curry = TRUE
    )
  })
})
