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
    norm_deps <- normalise(deps)
    tables <- decompose(df, norm_deps)
    es <- EntitySet(tables, norm_deps)
    expected_relations <- list(
      c("a", "b", "b", "b")
    )
    expect_identical(es$relationships, expected_relations)
  })
})

describe("plot_string", {
  test_df_strings <- function(
    df_name,
    df_label,
    nrows,
    attr_names,
    attr_labels,
    attr_chars
  ) {
    paste(
      paste0("  ", df_label, " [label = <"),
      "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
      paste0("    <TR><TD>", df_name, " (", nrows, " rows)</TD></TR>"),
      paste(
        "    <TR><TD PORT=\"",
        attr_labels,
        "\">",
        attr_names,
        " : ",
        attr_chars,
        "</TD></TR>",
        sep = "",
        collapse = "\n"
      ),
      "    </TABLE>>];",
      sep = "\n"
    )
  }

  describe("entityset", {
    it("creates a Graphviz HTML-like expression for the data.frame", {
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
        "  node [shape=plaintext];",

        "",

        test_df_strings(
          "Book",
          "book",
          2,
          c("Title", "Author", "Pages", "Thickness", "Genre ID", "Publisher ID"),
          c("title", "author", "pages", "thickness", "genre_id", "publisher_id"),
          c("character, prime", "character", "integer", "character", "integer", "integer")
        ),

        test_df_strings(
          "Format Price",
          "format_price",
          4,
          c("Title", "Format", "Price"),
          c("title", "format", "price"),
          c("character, prime", "character, prime", "integer")
        ),

        test_df_strings(
          "Author",
          "author",
          2,
          c("Author", "Author Nationality"),
          c("author", "author_nationality"),
          c("character, prime", "character")
        ),

        test_df_strings(
          "Genre",
          "genre",
          2,
          c("Genre ID", "Genre Name"),
          c("genre_id", "genre_name"),
          c("integer, prime", "character")
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
        "  node [shape=plaintext];",

        "",

        test_df_strings(
          "Book",
          "book",
          2,
          c("Title", "Author", "Pages", "Thickness", "Genre ID", "Publisher ID"),
          c("title", "author", "pages", "thickness", "genre_id", "publisher_id"),
          c("character, prime", "character", "integer", "character", "integer", "integer")
        ),

        test_df_strings(
          "Format Price",
          "format_price",
          4,
          c("Title", "Format", "Price"),
          c("title", "format", "price"),
          c("character, prime", "character, prime", "integer")
        ),

        test_df_strings(
          "Author",
          "author",
          2,
          c("Author", "Author Nationality"),
          c("author", "author_nationality"),
          c("character, prime", "character")
        ),

        test_df_strings(
          "Genre",
          "genre",
          2,
          c("Genre ID", "Genre Name"),
          c("genre_id", "genre_name"),
          c("integer, prime", "character")
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
    it("creates a Graphviz HTML-like expression for the data.frame", {
      df <- data.frame(
        a = 1:2, b = letters[1:2]
      )
      expect_identical(
        plot_string_df(df, "table"),
        paste(
          "digraph table {",
          "  rankdir = \"LR\"",
          "  node [shape=plaintext];",
          "",
          test_df_strings(
            "table",
            "table",
            2,
            c("a", "b"),
            c("a", "b"),
            c("integer", "character")
          ),
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
          "  node [shape=plaintext];",
          "",
          test_df_strings(
            "Table Test",
            "table_test",
            2,
            c("A 1", "b.2"),
            c("a_1", "b_2"),
            c("integer", "character")
          ),
          "}",
          "",
          sep = "\n"
        )
      )
    })
  })
})
