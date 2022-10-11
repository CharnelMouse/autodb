describe("gv", {
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
        paste0(
          " (",
          nrows,
          " rows)"
        ),
        "</TD></TR>"
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
  test_nameless_relation_strings <- function(
    rel_name,
    rel_label,
    attr_names,
    attr_labels,
    attr_classes,
    key_memberships
  ) {
    ncols <- ncol(key_memberships) + 2L
    paste(
      paste0("  ", rel_label, " [label = <"),
      "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
      paste0(
        "    <TR><TD COLSPAN=\"",
        ncols,
        "\">",
        rel_name,
        "</TD></TR>"
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
      database <- structure(
        list(
          name = "Book",
          tables = list(
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
        ),
        class = c("database", "list")
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
        gv(database),
        expected_string
      )
    })
    it("converts attribute/df names to snake case for labels (inc. spaces, periods)", {
      database <- structure(
        list(
          name = "Book",
          tables = list(
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
        ),
        class = c("database", "list")
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
        gv(database),
        expected_string
      )
    })
    it("doesn't give a graph ID if database name is missing", {
      database <- structure(
        list(
          name = NA_character_,
          tables = list(
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
      plot_string <- gv(database)
      expect_identical(substr(plot_string, 1, 9), "digraph {")
    })
  })
  describe("database_scheme", {
    it("creates a Graphviz HTML-like expression for the data.frame", {
      database <- structure(
        list(
          attrs = list(
            c("Title", "Author", "Pages", "Thickness", "Genre_ID", "Publisher_ID"),
            c("Title", "Format", "Price"),
            c("Author", "Author_Nationality"),
            c("Genre_ID", "Genre_Name")
          ),
          keys = list(
            list("Title"),
            list(c("Title", "Format")),
            list("Author"),
            list("Genre_ID")
          ),
          parents = list(
            2:4,
            integer(),
            integer(),
            integer()
          ),
          relationships = list(
            list(c(1L, 2L), "Title"),
            list(c(1L, 3L), "Author"),
            list(c(1L, 4L), "Genre ID")
          )
        ),
        class = c("database_scheme", "list")
      )
      expected_string <- paste(
        "digraph book {",
        "  rankdir = \"LR\"",
        "  node [shape=plaintext];",
        "",
        "  1 [label = <",
        "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
        "    <TR><TD PORT=\"TO_title\">Title</TD><TD PORT =\"FROM_title\" BGCOLOR=\"black\"></TD></TR>",
        "    <TR><TD PORT=\"TO_author\">Author</TD><TD PORT =\"FROM_author\"></TD></TR>",
        "    <TR><TD PORT=\"TO_pages\">Pages</TD><TD PORT =\"FROM_pages\"></TD></TR>",
        "    <TR><TD PORT=\"TO_thickness\">Thickness</TD><TD PORT =\"FROM_thickness\"></TD></TR>",
        "    <TR><TD PORT=\"TO_genre_id\">Genre_ID</TD><TD PORT =\"FROM_genre_id\"></TD></TR>",
        "    <TR><TD PORT=\"TO_publisher_id\">Publisher_ID</TD><TD PORT =\"FROM_publisher_id\"></TD></TR>",
        "    </TABLE>>];",
        "  2 [label = <",
        "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
        "    <TR><TD PORT=\"TO_title\">Title</TD><TD PORT =\"FROM_title\" BGCOLOR=\"black\"></TD></TR>",
        "    <TR><TD PORT=\"TO_format\">Format</TD><TD PORT =\"FROM_format\" BGCOLOR=\"black\"></TD></TR>",
        "    <TR><TD PORT=\"TO_price\">Price</TD><TD PORT =\"FROM_price\"></TD></TR>",
        "    </TABLE>>];",
        "  3 [label = <",
        "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
        "    <TR><TD PORT=\"TO_author\">Author</TD><TD PORT =\"FROM_author\" BGCOLOR=\"black\"></TD></TR>",
        "    <TR><TD PORT=\"TO_author_nationality\">Author_Nationality</TD><TD PORT =\"FROM_author_nationality\"></TD></TR>",
        "    </TABLE>>];",
        "  4 [label = <",
        "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
        "    <TR><TD PORT=\"TO_genre_id\">Genre_ID</TD><TD PORT =\"FROM_genre_id\" BGCOLOR=\"black\"></TD></TR>",
        "    <TR><TD PORT=\"TO_genre_name\">Genre_Name</TD><TD PORT =\"FROM_genre_name\"></TD></TR>",
        "    </TABLE>>];",
        "",
        "  1:FROM_title -> 2:TO_title;",
        "  1:FROM_author -> 3:TO_author;",
        "  1:FROM_genre_id -> 4:TO_genre_id;",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        gv(database, "book"),
        expected_string
      )
    })
    it("converts attribute/df names to snake case for labels (inc. spaces, periods)", {
      database <- structure(
        list(
          attrs = list(
            c(
              "Title",
              "Author",
              "Pages",
              "Thickness",
              "Genre ID",
              "Publisher ID"
            ),
            c("Title", "Format", "Price"),
            c("Author", "Author Nationality"),
            c("Genre ID", "Genre Name")
          ),
          keys = list(
            list("Title"),
            list(c("Title", "Format")),
            list("Author"),
            list("Genre ID")
          ),
          parents = list(
            2:4,
            integer(),
            integer(),
            integer()
          ),
          relationships = list(
            list(c(1L, 2L), "Title"),
            list(c(1L, 3L), "Author"),
            list(c(1L, 4L), "Genre ID")
          )
        ),
        class = c("database_scheme", "list")
      )
      expected_string <- paste(
        "digraph book {",
        "  rankdir = \"LR\"",
        "  node [shape=plaintext];",
        "",
        "  1 [label = <",
        "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
        "    <TR><TD PORT=\"TO_title\">Title</TD><TD PORT =\"FROM_title\" BGCOLOR=\"black\"></TD></TR>",
        "    <TR><TD PORT=\"TO_author\">Author</TD><TD PORT =\"FROM_author\"></TD></TR>",
        "    <TR><TD PORT=\"TO_pages\">Pages</TD><TD PORT =\"FROM_pages\"></TD></TR>",
        "    <TR><TD PORT=\"TO_thickness\">Thickness</TD><TD PORT =\"FROM_thickness\"></TD></TR>",
        "    <TR><TD PORT=\"TO_genre_id\">Genre ID</TD><TD PORT =\"FROM_genre_id\"></TD></TR>",
        "    <TR><TD PORT=\"TO_publisher_id\">Publisher ID</TD><TD PORT =\"FROM_publisher_id\"></TD></TR>",
        "    </TABLE>>];",
        "  2 [label = <",
        "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
        "    <TR><TD PORT=\"TO_title\">Title</TD><TD PORT =\"FROM_title\" BGCOLOR=\"black\"></TD></TR>",
        "    <TR><TD PORT=\"TO_format\">Format</TD><TD PORT =\"FROM_format\" BGCOLOR=\"black\"></TD></TR>",
        "    <TR><TD PORT=\"TO_price\">Price</TD><TD PORT =\"FROM_price\"></TD></TR>",
        "    </TABLE>>];",
        "  3 [label = <",
        "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
        "    <TR><TD PORT=\"TO_author\">Author</TD><TD PORT =\"FROM_author\" BGCOLOR=\"black\"></TD></TR>",
        "    <TR><TD PORT=\"TO_author_nationality\">Author Nationality</TD><TD PORT =\"FROM_author_nationality\"></TD></TR>",
        "    </TABLE>>];",
        "  4 [label = <",
        "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
        "    <TR><TD PORT=\"TO_genre_id\">Genre ID</TD><TD PORT =\"FROM_genre_id\" BGCOLOR=\"black\"></TD></TR>",
        "    <TR><TD PORT=\"TO_genre_name\">Genre Name</TD><TD PORT =\"FROM_genre_name\"></TD></TR>",
        "    </TABLE>>];",
        "",
        "  1:FROM_title -> 2:TO_title;",
        "  1:FROM_author -> 3:TO_author;",
        "  1:FROM_genre_id -> 4:TO_genre_id;",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        gv(database, "book"),
        expected_string
      )
    })
    it("doesn't give a graph ID if database name is missing", {
      database <- structure(
        list(
          name = NA_character_,
          tables = list(
            a = list(
              df = data.frame(a = 1:4, b = 1:2),
              keys = list("a"),
              index = "a",
              parents = character()
            )
          ),
          relationships = list()
        ),
        class = c("database_scheme", "list")
      )
      plot_string <- gv(database)
      expect_identical(substr(plot_string, 1, 9), "digraph {")
    })
  })
  describe("data.frame", {
    it("creates a Graphviz HTML-like expression for the data.frame", {
      df <- data.frame(
        a = 1:2, b = letters[1:2]
      )
      expect_identical(
        gv(df, "table"),
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
        gv(df, "Table Test"),
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
