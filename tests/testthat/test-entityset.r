describe("plot_string", {
  describe("entityset", {
    it("creates a Graphviz record expression for the data.frame", {
      es <- list(
        name = "Book",
        dataframes = list(
          Book = list(
            df = data.frame(
              Title = c(
                "Beginning MySQL Database Design and Optimization",
                "The Relational Model for Database Management: Version 2"
              ),
              Author = c(
                "Chad Russel",
                "EF Codd"
              ),
              Pages = c(520L, 538L),
              Thickness = "Thick",
              Genre_ID = 1:2,
              Publisher_ID = 1:2
            ),
            keys = list("Title"),
            index = "Title",
            children = c("Format_Price", "Author", "Genre")
          ),
          Format_Price = list(
            df = data.frame(
              Title = c(
                "Beginning MySQL Database Design and Optimization",
                "Beginning MySQL Database Design and Optimization",
                "The Relational Model for Database Management: Version 2",
                "The Relational Model for Database Management: Version 2"
              ),
              Format = c("Hardcover", "E-book", "E-book", "Paperback"),
              Price = c(4999L, 2234L, 1388L, 3999L)
            ),
            keys = list(c("Title", "Format")),
            index = c("Title", "Format"),
            children = character()
          ),
          Author = list(
            df = data.frame(
              Author = c("Chad Russell", "EF Codd"),
              Author_Nationality = c("American", "British")
            ),
            keys = list("Author"),
            index = "Author",
            children = character()
          ),
          Genre = list(
            df = data.frame(
              Genre_ID = 1:2,
              Genre_Name = c("Tutorial", "Popular science")
            ),
            keys = list("Genre_ID"),
            index = "Genre_ID",
            children = character()
          )
        ),
        relationships = list(
          c("Book", "Title", "Format_Price", "Title"),
          c("Book", "Author", "Author", "Author"),
          c("Book", "Genre_ID", "Genre", "Genre_ID")
        )
      )
      expected_string <- paste(
        "digraph Book {",
        "  rankdir = \"LR\"",
        "  node [shape=record];",
        paste(
          "  Book [label = \"Book (2 rows)",
          "<Title> Title : character",
          "<Author> Author : character",
          "<Pages> Pages : integer",
          "<Thickness> Thickness : character",
          "<Genre_ID> Genre_ID : integer",
          "<Publisher_ID> Publisher_ID : integer\"];",
          sep = "|"
        ),
        paste(
          "  Format_Price [label = \"Format_Price (4 rows)",
          "<Title> Title : character",
          "<Format> Format : character",
          "<Price> Price : integer\"];",
          sep = "|"
        ),
        paste(
          "  Author [label = \"Author (2 rows)",
          "<Author> Author : character",
          "<Author_Nationality> Author_Nationality : character\"];",
          sep = "|"
        ),
        paste(
          "  Genre [label = \"Genre (2 rows)",
          "<Genre_ID> Genre_ID : integer",
          "<Genre_Name> Genre_Name : character\"];",
          sep = "|"
        ),
        "",
        "  Book:Title -> Format_Price:Title;",
        "  Book:Author -> Author:Author;",
        "  Book:Genre_ID -> Genre:Genre_ID;",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        plot_string_entityset(es),
        expected_string
      )
    })
  })
  describe("data.frame", {
    it("creates a Graphviz record expression for the data.frame", {
      df <- data.frame(
        a = 1:2, b = letters[1:2]
      )
      expect_identical(
        plot_string_df(df, "table"),
        paste(
          "digraph table {",
          "  rankdir = \"LR\"",
          "  node [shape=record];",
          "  table [label = \"table (2 rows)|<a> a : integer|<b> b : character\"];",
          "}",
          "",
          sep = "\n"
        )
      )
    })
    it("replaces spaces in table names with underscores", {
      df <- data.frame(
        a = 1:2, b = letters[1:2]
      )
      expect_identical(
        plot_string_df(df, "table test"),
        paste(
          "digraph table_test {",
          "  rankdir = \"LR\"",
          "  node [shape=record];",
          "  table_test [label = \"table_test (2 rows)|<a> a : integer|<b> b : character\"];",
          "}",
          "",
          sep = "\n"
        )
      )
    })
  })
})
