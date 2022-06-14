describe("auto_entityset", {
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
    es <- auto_entityset(df, 1)
    skip("non-deterministic, fix next")
    expect_setequal(
      names(es$dataframes),
      c("Publisher_ID", "Price", "Format")
    )
    expect_identical(
      es$relationships,
      list(
        c("Price", "Publisher_ID", "Publisher_ID", "Publisher_ID"),
        c("Price", "Format", "Format", "Format")
      )
    )
  })
  it("doesn't choose keys with incorrect types as the index", {
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
    es <- auto_entityset(df, 1)
    skip("wait on key filtering")
  })
})
