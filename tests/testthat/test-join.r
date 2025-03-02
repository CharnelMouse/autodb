describe("df_join", {
  it("returns a table with unique rows if it joins it with itself (maybe re-ordered)", {
    forall(
      gen_df(4, 6, remove_dup_rows = TRUE),
      expect_bi(df_equiv, identity, dup %>>% uncurry(df_join))
    )
  })
  it("merges simple tables with matching names/classes in the same way as merge()", {
    forall(
      gen.element(0:5) |>
        gen.and_then(\(n) {
          list(
            gen.element(c("logical", "integer", "numeric", "character", "factor")) |>
              gen.c(of = n),
            gen_attr_names(n, 9)
          )
        }) |>
        gen.and_then(uncurry(\(classes, nms) {
          list(
            classes = gen.pure(classes),
            nms = gen.pure(nms),
            n_records = gen.element(0:6),
            remove_dup_rows = FALSE
          ) |>
            gen.and_then(uncurry(gen.df_fixed_ranges)) |>
            gen.list(of = 2)
        })),
      expect_bi(df_equiv, uncurry(df_join), uncurry(merge))
    )
  })
})
