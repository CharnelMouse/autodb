test_that("simple tables pass through the whole pipeline", {
  df <- data.frame(a = 1:4, b = 1:2)
  database <- autodb(df, 1)
  database2 <- dfd(df, 1) |>
    flatten() |>
    normalise() |>
    cross_reference() |>
    decompose(df = df)
  expect_identical(database, database2)
  expect_silent(gv(database))
})
