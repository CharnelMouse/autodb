describe("plot_string", {
  test_df_strings <- function(
    df_name,
    df_label,
    nrows,
    attr_names,
    attr_labels,
    attr_classes,
    key_memberships
  ) {
    ncols <- ncol(key_memberships) + 2L
    paste(
      paste0("  ", df_label, " [label = <"),
      "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
      paste0(
        "    <TR><TD COLSPAN=\"",
        ncols,
        "\">",
        df_name,
        " (",
        nrows,
        " rows)</TD></TR>"
      ),
      paste(
        "    <TR><TD PORT=\"TO_",
        attr_labels,
        "\">",
        attr_names,
        "</TD>",
        if (nrow(key_memberships) > 0)
          apply(
            key_memberships,
            1,
            \(ls) {
              paste0(
                "<TD",
                ifelse(ls, " BGCOLOR=\"black\"", ""),
                ">",
                "</TD>"
              )
            }
          ),
        "<TD PORT=\"FROM_",
        attr_labels,
        "\">",
        attr_classes,
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
          c("character", "character", "integer", "character", "integer", "integer"),
          matrix(
            c(
              TRUE,
              FALSE,
              FALSE,
              FALSE,
              FALSE,
              FALSE
            ),
            nrow = 6,
            byrow = TRUE
          )
        ),

        test_df_strings(
          "Format Price",
          "format_price",
          4,
          c("Title", "Format", "Price"),
          c("title", "format", "price"),
          c("character", "character", "integer"),
          matrix(
            c(
              TRUE,
              TRUE,
              FALSE
            ),
            nrow = 3,
            byrow = TRUE
          )
        ),

        test_df_strings(
          "Author",
          "author",
          2,
          c("Author", "Author Nationality"),
          c("author", "author_nationality"),
          c("character", "character"),
          matrix(
            c(
              TRUE,
              FALSE
            ),
            nrow = 2,
            byrow = TRUE
          )
        ),

        test_df_strings(
          "Genre",
          "genre",
          2,
          c("Genre ID", "Genre Name"),
          c("genre_id", "genre_name"),
          c("integer", "character"),
          matrix(
            c(
              TRUE,
              FALSE
            ),
            nrow = 2,
            byrow = TRUE
          )
        ),

        "",

        "  book:FROM_title -> format_price:TO_title;",
        "  book:FROM_author -> author:TO_author;",
        "  book:FROM_genre_id -> genre:TO_genre_id;",
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
          c("character", "character", "integer", "character", "integer", "integer"),
          matrix(
            c(
              TRUE,
              FALSE,
              FALSE,
              FALSE,
              FALSE,
              FALSE
            ),
            nrow = 6,
            byrow = TRUE
          )
        ),

        test_df_strings(
          "Format Price",
          "format_price",
          4,
          c("Title", "Format", "Price"),
          c("title", "format", "price"),
          c("character", "character", "integer"),
          matrix(
            c(
              TRUE,
              TRUE,
              FALSE
            ),
            nrow = 3,
            byrow = TRUE
          )
        ),

        test_df_strings(
          "Author",
          "author",
          2,
          c("Author", "Author Nationality"),
          c("author", "author_nationality"),
          c("character", "character"),
          matrix(
            c(
              TRUE,
              FALSE
            ),
            nrow = 2,
            byrow = TRUE
          )

        ),

        test_df_strings(
          "Genre",
          "genre",
          2,
          c("Genre ID", "Genre Name"),
          c("genre_id", "genre_name"),
          c("integer", "character"),
          matrix(
            c(
              TRUE,
              FALSE
            ),
            nrow = 2,
            byrow = TRUE
          )
        ),

        "",

        "  book:FROM_title -> format_price:TO_title;",
        "  book:FROM_author -> author:TO_author;",
        "  book:FROM_genre_id -> genre:TO_genre_id;",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        plot_string_entityset(es),
        expected_string
      )
    })
    it("doesn't give a graph ID if database name is missing", {
      es <- structure(
        list(
          name = NA_character_,
          dataframes = list(
            a = list(
              df = data.frame(a = 1:4, b = 1:2),
              keys = list("a"),
              index = "a",
              parents = character()
            )
          ),
          relationships = list()
        ),
        class = c("database", "list")
      )
      plot_string <- plot_string_entityset(es)
      expect_identical(substr(plot_string, 1, 9), "digraph {")
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
            c("integer", "character"),
            matrix(nrow = 0, ncol = 0)
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
            c("integer", "character"),
            matrix(nrow = 0, ncol = 0)
          ),
          "}",
          "",
          sep = "\n"
        )
      )
    })
  })
})
