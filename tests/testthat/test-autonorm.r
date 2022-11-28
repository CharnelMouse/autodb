describe("autonorm", {
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
    database <- autonorm(df, 1)
    expect_true(!anyDuplicated(database))
    expect_identical(length(database$relations), 3L)
    expect_setequal(names(database$relations), c("Price", "Title", "constants"))
    expect_setequal(
      database$relationships,
      list(c("Price", "Title", "Title", "Title"))
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
    database_nonfiltered <- autonorm(df, 1)
    expect_setequal(
      names(database_nonfiltered$relations),
      c("Price", "Title", "constants")
    )
    expect_identical(
      database_nonfiltered$relations$Price$keys,
      list("Price", c("Title", "Format"))
    )
    database_filtered <- autonorm(df, 1, exclude_class = "numeric")
    expect_setequal(
      names(database_filtered$relations),
      c("Title_Format", "Title", "constants")
    )
    expect_identical(
      database_filtered$relations$Title_Format$keys,
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
    database_nonfiltered <- autonorm(df, 1)
    expect_setequal(
      names(database_nonfiltered$relations),
      c("Price", "Title", "constants")
    )
    expect_identical(
      database_nonfiltered$relations$Price$keys,
      list("Price", c("Title", "Format"))
    )
    database_filtered <- autonorm(df, 1, exclude = "Price")
    expect_setequal(
      names(database_filtered$relations),
      c("Title_Format", "Title", "constants")
    )
    expect_identical(
      database_filtered$relations$Title_Format$keys,
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
    database_nonfiltered <- autonorm(df, 1)
    expect_setequal(
      names(database_nonfiltered$relations),
      c("Price", "Title", "constants")
    )
    expect_identical(
      database_nonfiltered$relations$Price$keys,
      list("Price", c("Title", "Format"))
    )
    database_filtered <- autonorm(df, 1, exclude_class = "integer")
    expect_setequal(
      names(database_filtered$relations),
      c("Title_Format", "Title", "constants")
    )
    expect_identical(
      database_filtered$relations$Title_Format$keys,
      list(c("Title", "Format"))
    )
  })
  it("correctly handles attributes with non-df-standard names", {
    df <- data.frame(1:3, c(1, 1, 2), c(1, 2, 2)) |>
      stats::setNames(c("A 1", "B 2", "C 3"))
    database <- autonorm(df, 1)
    expect_identical(names(database$relations[[1]]$df), c("A 1", "B 2", "C 3"))
  })
  it("adds a key table if none given in normalisation", {
    df <- data.frame(
      a = c(1L, 2L, 1L, 2L),
      b = c(1L, 2L, 1L, 2L),
      c = c(1L, 1L, 2L, 2L)
    )
    database <- autonorm(df, 1)
    expect_identical(
      database$relations$a_c,
      list(
        df = data.frame(a = 1:2, c = rep(1:2, each = 2), row.names = 1:4),
        keys = list(c("a", "c")),
        parents = "a"
      )
    )
  })
})
