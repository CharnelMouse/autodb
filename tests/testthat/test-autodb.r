library(hedgehog)

describe("autodb", {
  it("returns valid databases", {
    forall(
      gen_df(6, 7),
      apply_both(
        autodb %>>% is_valid_database,
        with_args(autodb, remove_avoidable = TRUE) %>>% is_valid_database
      )
    )
  })
  it("is the same as discover >> normalise >> decompose", {
    df <- data.frame(a = 1:4, b = 1:2)
    database <- autodb(df)
    database2 <- discover(df, 1) |>
      normalise() |>
      decompose(df = df)
    expect_identical(database, database2)
    expect_silent(gv(database))

    forall(
      list(
        gen_df(6, 7),
        ensure_lossless = gen.element(c(FALSE, TRUE)),
        remove_avoidable = gen.element(c(FALSE, TRUE))
      ),
      expect_biidentical(
        uncurry(autodb),
        biapply(
          with_args("[[", 1),
          (\(x) c(list(discover(x[[1]], 1)), x[-1])) %>>%
            (uncurry(normalise))
        ) %>>%
          (uncurry(decompose))
      ),
      curry = FALSE
    )
  })
  it("runs DFD and normalises the given data.frame", {
    df <- data.frame(
      Title = rep(
        c(
          "Beginning MySQL Database Design and Optimization",
          "The Relational Model for Database Management: Version 2"
        ),
        each = 2
      ),
      Format = c("Hardcover", "E-book", "E-book", "Paperback"),
      Author = rep(c("Chad Russell", "E.F. Codd"), each = 2),
      Author_Nationality = rep(c("American", "British"), each = 2),
      Price = c(4999L, 2234L, 1388L, 3999L),
      Thickness = "Thick",
      Genre_ID = rep(1:2, each = 2),
      Genre_Name = rep(c("Tutorial", "Popular science"), each = 2),
      Publisher_ID = rep(1:2, each = 2)
    )
    database <- autodb(df)
    expect_true(!anyDuplicated(database))
    expect_identical(length(database), 3L)
    expect_setequal(names(database), c("Price", "Title", "constants"))
    expect_setequal(
      relationships(database),
      list(list("Price", "Title", "Title", "Title"))
    )
  })
  it("doesn't choose keys containing attributes with types in exclude_class", {
    df <- data.frame(
      Title = rep(
        c(
          "Beginning MySQL Database Design and Optimization",
          "The Relational Model for Database Management: Version 2"
        ),
        each = 2
      ),
      Format = c("Hardcover", "E-book", "E-book", "Paperback"),
      Author = rep(c("Chad Russell", "E.F. Codd"), each = 2),
      Author_Nationality = rep(c("American", "British"), each = 2),
      Price = c(4999, 2234, 1388, 3999),
      Thickness = "Thick",
      Genre_ID = rep(1:2, each = 2),
      Genre_Name = rep(c("Tutorial", "Popular science"), each = 2),
      Publisher_ID = rep(1:2, each = 2)
    )
    database_nonfiltered <- autodb(df)
    expect_setequal(
      names(database_nonfiltered),
      c("Price", "Title", "constants")
    )
    expect_identical(
      keys(database_nonfiltered)$Price,
      list("Price", c("Title", "Format"))
    )
    database_filtered <- autodb(df, exclude_class = "numeric")
    expect_setequal(
      names(database_filtered),
      c("Title_Format", "Title", "constants")
    )
    expect_identical(
      keys(database_filtered)$Title_Format,
      list(c("Title", "Format"))
    )
  })
  it("removes keys containing attributes named in exclude", {
    df <- data.frame(
      Title = rep(
        c(
          "Beginning MySQL Database Design and Optimization",
          "The Relational Model for Database Management: Version 2"
        ),
        each = 2
      ),
      Format = c("Hardcover", "E-book", "E-book", "Paperback"),
      Author = rep(c("Chad Russell", "E.F. Codd"), each = 2),
      Author_Nationality = rep(c("American", "British"), each = 2),
      Price = c(4999L, 2234L, 1388L, 3999L),
      Thickness = "Thick",
      Genre_ID = rep(1:2, each = 2),
      Genre_Name = rep(c("Tutorial", "Popular science"), each = 2),
      Publisher_ID = rep(1:2, each = 2)
    )
    database_nonfiltered <- autodb(df)
    expect_setequal(
      names(database_nonfiltered),
      c("Price", "Title", "constants")
    )
    expect_identical(
      keys(database_nonfiltered)$Price,
      list("Price", c("Title", "Format"))
    )
    database_filtered <- autodb(df, exclude = "Price")
    expect_setequal(
      names(database_filtered),
      c("Title_Format", "Title", "constants")
    )
    expect_identical(
      keys(database_filtered)$Title_Format,
      list(c("Title", "Format"))
    )
  })
  it("doesn't choose keys with incorrect types as the index if filter = TRUE", {
    df <- data.frame(
      Title = rep(
        c(
          "Beginning MySQL Database Design and Optimization",
          "The Relational Model for Database Management: Version 2"
        ),
        each = 2
      ),
      Format = c("Hardcover", "E-book", "E-book", "Paperback"),
      Author = rep(c("Chad Russell", "E.F. Codd"), each = 2),
      Author_Nationality = rep(c("American", "British"), each = 2),
      Price = c(4999L, 2234L, 1388L, 3999L),
      Thickness = "Thick",
      Genre_ID = rep(1:2, each = 2),
      Genre_Name = rep(c("Tutorial", "Popular science"), each = 2),
      Publisher_ID = rep(1:2, each = 2)
    )
    database_nonfiltered <- autodb(df)
    expect_setequal(
      names(database_nonfiltered),
      c("Price", "Title", "constants")
    )
    expect_identical(
      keys(database_nonfiltered)$Price,
      list("Price", c("Title", "Format"))
    )
    database_filtered <- autodb(df, exclude_class = "integer")
    expect_setequal(
      names(database_filtered),
      c("Title_Format", "Title", "constants")
    )
    expect_identical(
      keys(database_filtered)$Title_Format,
      list(c("Title", "Format"))
    )
  })
  it("correctly handles attributes with non-df-standard names", {
    df <- data.frame(1:3, c(1, 1, 2), c(1, 2, 2)) |>
      stats::setNames(c("A 1", "B 2", "C 3"))
    database <- autodb(df)
    expect_identical(names(database[[1]]$df), c("A 1", "B 2", "C 3"))
  })
  it("adds a key table if none given in normalisation", {
    df <- data.frame(
      a = c(1L, 2L, 1L, 2L),
      b = c(1L, 2L, 1L, 2L),
      c = c(1L, 1L, 2L, 2L)
    )
    database <- autodb(df)
    expect_identical(
      database$a_c,
      list(
        df = data.frame(a = 1:2, c = rep(1:2, each = 2), row.names = 1:4),
        keys = list(c("a", "c"))
      )
    )
  })
  it("decomposes zero-column data frames correctly into TABLE_DUM or TABLE_DEE", {
    table_dum <- data.frame()
    table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
    table_deux <- data.frame(a = 1:2)[, -1, drop = FALSE]
    db_dum <- autodb(table_dum)
    db_dee <- autodb(table_dee)
    db_deux <- autodb(table_deux)
    expect_length(db_dum, 1L)
    expect_length(db_dee, 1L)
    expect_identical(nrow(db_dum[[1]]$df), 0L)
    expect_identical(nrow(db_dee[[1]]$df), 1L)
    expect_identical(db_deux, db_dee)
  })
})
