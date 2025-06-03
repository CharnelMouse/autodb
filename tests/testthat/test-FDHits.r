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
      gen_df(4, 6) |>
        gen.with(lookup_table),
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

      observed <- try(FDHits(lookup, method = method), silent = TRUE)
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
      # epsilon = 1 needn't sample every relevant difference set,
      # since sampling can have duplicates
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
    observed <- try(refine_partition_by_lookup(partition, indices))
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
              \(attr) refine_partition_by_lookup(Spli, lookup[[attr]]) |>
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

describe("critical", {
  ints_to_bitset <- function(x, bitlen) {
    bools <- rep(FALSE, bitlen)
    bools[x] <- TRUE
    packBits(bools)
  }
  it(
    paste(
      "for C in S:",
      "- returns indices of diffsets that contain A, and only C from S",
      "- by above, returns nothing if A is in S and C != A",
      sep = "\n"
    ),
    {
      forall(
        list(
          gen.int(10),
          gen.sample(seq_len(10), gen.int(10), replace = FALSE),
          gen.sample(seq_len(10), gen.element(0:10), replace = FALSE) |>
            gen.list(from = 0, to = 10)
        ) |>
          gen.with(\(lst) {
            A <- lst[[1]]
            S <- lst[[2]]
            diffsets <- lst[[3]]
            list(
              ints_to_bitset(A, 16),
              ints_to_bitset(S, 16),
              ints_to_bitset(S[[1]], 16),
              lapply(diffsets, ints_to_bitset, 16)
            )
          }),
        function(A, S, S_element, diffsets) {
          crits <- critical(S_element, A, S, diffsets)
          if (all((A & S) == A) && !all(A == S_element) && length(crits) > 0)
            return(fail(paste(
              "A is in S, but critical edges for C aren't empty:",
              toString(crits)
            )))
          has_A <- vapply(diffsets, \(ds) all((A & ds) == A), logical(1))
          has_only_el <- vapply(diffsets, \(ds) all((S & ds) == S_element), logical(1))
          expect_identical(crits, which(has_A & has_only_el))
        },
        curry = TRUE
      )
    }
  )
  it(
    paste(
      "adding C to S:",
      "- removes all diffsets containing C as critical for original elements",
      "- has edges critical for C that can't include those previously critical elsewhere",
      "- more strongly, edges critical for C are those previously uncovered for A that contain C",
      sep = "\n"
    ),
    {
      forall(
        gen.sample(seq_len(10), gen.element(2:10), replace = FALSE) |>
          gen.list(from = 1, to = 10) |>
          gen.with(\(lst) {
            ints <- lst[[1]]
            list(
              ints_to_bitset(ints[[1]], 16),
              ints_to_bitset(ints[-(1:2)], 16),
              lapply(ints[-(1:2)], ints_to_bitset, 16),
              ints_to_bitset(ints[[2]], 16),
              lapply(lst[-1], ints_to_bitset, 16)
            )
          }),
        function(A, S, S_elements, new_S_element, diffsets) {
          old_criticals <- lapply(
            S_elements,
            \(S_element) critical(S_element, A, S, diffsets)
          )
          remaining_diffsets <- setdiff(
            seq_along(diffsets),
            unlist(old_criticals)
          )

          new_criticals <- lapply(
            S_elements,
            \(S_element) critical(S_element, A, S | new_S_element, diffsets)
          )
          new_critical <- critical(new_S_element, A, S | new_S_element, diffsets)

          old_criticals_reduced <- mapply(
            function(old, new) {
              filtered_old <- old[vapply(
                diffsets[old],
                \(edge) all((edge & new_S_element) == 0),
                logical(1)
              )]
              identical(new, filtered_old)
            },
            old_criticals,
            new_criticals
          )
          if (!all(old_criticals_reduced))
            return(fail("some critical edge sets weren't reduced"))
          new_criticals_from_remaining <- all(is.element(
            new_critical,
            remaining_diffsets
          ))
          if (!new_criticals_from_remaining)
            return(fail("new element's critical edges not from non-critical"))
          new_criticals_from_uncovered <- setequal(
            new_critical,
            which(
              uncov(S, A, diffsets) &
                vapply(
                  diffsets,
                  \(ds) all((ds & new_S_element) == new_S_element),
                  logical(1)
                )
            )
          )
          if (!new_criticals_from_uncovered)
            return(fail("new element's critical edges not from uncovered_A"))
          succeed()
        },
        curry = TRUE
      )
    }
  )
})
