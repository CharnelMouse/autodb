test_that("simple tables pass through the whole pipeline", {
  df <- data.frame(a = 1:4, b = 1:2)
  es <- autonorm(df, 1)
  es2 <- dfd(df, 1) |>
    flatten() |>
    normalise() |>
    cross_reference() |>
    decompose(df = df)
  expect_identical(es, es2)
  expect_silent(plot_tables(es))
})
