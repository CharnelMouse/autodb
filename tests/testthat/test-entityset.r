describe("EntitySet", {
  it("returns relations", {
    df <- data.frame(a = integer(), b = integer(), c = integer())
    deps <- list(
      dependencies = list(
        list("a", "b"),
        list("a", "c"),
        list("b", "c")
      ),
      attrs = c("a", "b", "c")
    )
    es <- EntitySet(df, deps)
    expected_relations <- list(
      c("a", "b", "b", "b")
    )
    expect_identical(es$relationships, expected_relations)
  })
})

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
            ) |>
              stats::setNames(c(
                "Title",
                "Author",
                "Pages",
                "Thickness",
                "Genre ID",
                "Publisher ID"
              )),
            keys = list("Title"),
            index = "Title",
            parents = c("Format Price", "Author", "Genre")
          ),
          `Format Price` = list(
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
            parents = character()
          ),
          Author = list(
            df = data.frame(
              Author = c("Chad Russell", "EF Codd"),
              Author_Nationality = c("American", "British")
            ) |>
              stats::setNames(c("Author", "Author Nationality")),
            keys = list("Author"),
            index = "Author",
            parents = character()
          ),
          Genre = list(
            df = data.frame(
              Genre_ID = 1:2,
              Genre_Name = c("Tutorial", "Popular science")
            ) |>
              stats::setNames(c("Genre ID", "Genre Name")),
            keys = list("Genre ID"),
            index = "Genre ID",
            parents = character()
          )
        ),
        relationships = list(
          c("Book", "Title", "Format Price", "Title"),
          c("Book", "Author", "Author", "Author"),
          c("Book", "Genre ID", "Genre", "Genre ID")
        )
      )
      expected_string <- paste(
        "digraph book {",
        "  rankdir = \"LR\"",
        "  node [shape=record];",
        paste(
          "  book [label = \"Book (2 rows)",
          "<title> Title : character, prime",
          "<author> Author : character",
          "<pages> Pages : integer",
          "<thickness> Thickness : character",
          "<genre_id> Genre ID : integer",
          "<publisher_id> Publisher ID : integer\"];",
          sep = "|"
        ),
        paste(
          "  format_price [label = \"Format Price (4 rows)",
          "<title> Title : character, prime",
          "<format> Format : character, prime",
          "<price> Price : integer\"];",
          sep = "|"
        ),
        paste(
          "  author [label = \"Author (2 rows)",
          "<author> Author : character, prime",
          "<author_nationality> Author Nationality : character\"];",
          sep = "|"
        ),
        paste(
          "  genre [label = \"Genre (2 rows)",
          "<genre_id> Genre ID : integer, prime",
          "<genre_name> Genre Name : character\"];",
          sep = "|"
        ),
        "",
        "  book:title -> format_price:title;",
        "  book:author -> author:author;",
        "  book:genre_id -> genre:genre_id;",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        plot_string_entityset(es),
        expected_string
      )
    })
    it("converts attribute/df names to snake case for labels (inc. spaces, periods)", {
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
            ) |>
              stats::setNames(c(
                "Title",
                "Author",
                "Pages",
                "Thickness",
                "Genre ID",
                "Publisher ID"
              )),
            keys = list("Title"),
            index = "Title",
            parents = c("Format Price", "Author", "Genre")
          ),
          `Format Price` = list(
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
            parents = character()
          ),
          Author = list(
            df = data.frame(
              Author = c("Chad Russell", "EF Codd"),
              Author_Nationality = c("American", "British")
            ) |>
              stats::setNames(c("Author", "Author Nationality")),
            keys = list("Author"),
            index = "Author",
            parents = character()
          ),
          Genre = list(
            df = data.frame(
              Genre_ID = 1:2,
              Genre_Name = c("Tutorial", "Popular science")
            ) |>
              stats::setNames(c("Genre ID", "Genre Name")),
            keys = list("Genre ID"),
            index = "Genre ID",
            parents = character()
          )
        ),
        relationships = list(
          c("Book", "Title", "Format Price", "Title"),
          c("Book", "Author", "Author", "Author"),
          c("Book", "Genre ID", "Genre", "Genre ID")
        )
      )
      expected_string <- paste(
        "digraph book {",
        "  rankdir = \"LR\"",
        "  node [shape=record];",
        paste(
          "  book [label = \"Book (2 rows)",
          "<title> Title : character, prime",
          "<author> Author : character",
          "<pages> Pages : integer",
          "<thickness> Thickness : character",
          "<genre_id> Genre ID : integer",
          "<publisher_id> Publisher ID : integer\"];",
          sep = "|"
        ),
        paste(
          "  format_price [label = \"Format Price (4 rows)",
          "<title> Title : character, prime",
          "<format> Format : character, prime",
          "<price> Price : integer\"];",
          sep = "|"
        ),
        paste(
          "  author [label = \"Author (2 rows)",
          "<author> Author : character, prime",
          "<author_nationality> Author Nationality : character\"];",
          sep = "|"
        ),
        paste(
          "  genre [label = \"Genre (2 rows)",
          "<genre_id> Genre ID : integer, prime",
          "<genre_name> Genre Name : character\"];",
          sep = "|"
        ),
        "",
        "  book:title -> format_price:title;",
        "  book:author -> author:author;",
        "  book:genre_id -> genre:genre_id;",
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
    it("converts attribute/df names to snake case for labels (inc. spaces, periods)", {
      df <- data.frame(
        a = 1:2, b = letters[1:2]
      ) |>
        stats::setNames(c("A 1", "b.2"))
      expect_identical(
        plot_string_df(df, "Table Test"),
        paste(
          "digraph table_test {",
          "  rankdir = \"LR\"",
          "  node [shape=record];",
          "  table_test [label = \"Table Test (2 rows)|<a_1> A 1 : integer|<b_2> b.2 : character\"];",
          "}",
          "",
          sep = "\n"
        )
      )
    })
  })
})
