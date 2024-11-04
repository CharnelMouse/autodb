library(hedgehog)

describe("df_join", {
  it("returns a table if it joins it with itself (maybe re-ordered)", {
    forall(
      gen_df(4, 6, remove_dup_rows = TRUE) |>
        gen.with(\(x) `rownames<-`(x, NULL)),
      expect_bi(df_equiv, identity, dup %>>% uncurry(df_join))
    )
  })
})
