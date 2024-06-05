library(hedgehog)

describe("gv", {
  test_df_strings <- function(
    df_name,
    df_label,
    nrows,
    rows_name,
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
          " ",
          rows_name,
          ")"
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

  describe("database", {
    it("expects non-empty relation names", {
      forall(
        gen_df(6, 7),
        \(df) {
          db <- autodb(df)
          if (length(db) == 0)
            discard()
          attr(db, "names")[[1]] <- "" # skip name guard
          expect_error(
            gv(db),
            "^relation names can not be zero characters in length$"
          )
        }
      )
    })
    it("works for autodb output", {
      forall(
        gen_df(6, 7),
        autodb %>>% gv %>>% expect_no_error
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      db_dum <- autodb(table_dum)
      db_dee <- autodb(table_dee)
      expect_no_error(gv(db_dum))
      expect_no_error(gv(db_dee))
    })
    it("creates a Graphviz HTML-like expression for the data.frame", {
      db <- database(
        relation(
          list(
            Book = list(
              df = data.frame(
                Title = c(
                  "Beginning MySQL Database Design and Optimization",
                  "The Relational Model for Database Management: Version 2"
                ),
                Author = c(
                  "Chad Russell",
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
              keys = list("Title")
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
              keys = list(c("Title", "Format"))
            ),
            Author = list(
              df = data.frame(
                Author = c("Chad Russell", "EF Codd"),
                Author_Nationality = c("American", "British")
              ) |>
                stats::setNames(c("Author", "Author Nationality")),
              keys = list("Author")
            ),
            Genre = list(
              df = data.frame(
                Genre_ID = 1:2,
                Genre_Name = c("Tutorial", "Popular science")
              ) |>
                stats::setNames(c("Genre ID", "Genre Name")),
              keys = list("Genre ID")
            )
          ),
          attrs_order = c(
            "Title",
            "Author",
            "Author Nationality",
            "Format",
            "Price",
            "Subject",
            "Pages",
            "Thickness",
            "Genre ID",
            "Genre Name",
            "Publisher ID"
          )
        ),
        references = list(
          list("Book", "Title", "Format Price", "Title"),
          list("Book", "Author", "Author", "Author"),
          list("Book", "Genre ID", "Genre", "Genre ID")
        ),
        name = "Book"
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
          "records",
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
          "records",
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
          "records",
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
          "records",
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
        gv(db),
        expected_string
      )
    })
    it("converts attribute/df names to snake case for labels (inc. spaces, periods)", {
      db <- database(
        relation(
          list(
            Book = list(
              df = data.frame(
                Title = c(
                  "Beginning MySQL Database Design and Optimization",
                  "The Relational Model for Database Management: Version 2"
                ),
                Author = c(
                  "Chad Russell",
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
              keys = list("Title")
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
              keys = list(c("Title", "Format"))
            ),
            Author = list(
              df = data.frame(
                Author = c("Chad Russell", "EF Codd"),
                Author_Nationality = c("American", "British")
              ) |>
                stats::setNames(c("Author", "Author Nationality")),
              keys = list("Author")
            ),
            Genre = list(
              df = data.frame(
                Genre_ID = 1:2,
                Genre_Name = c("Tutorial", "Popular science")
              ) |>
                stats::setNames(c("Genre ID", "Genre Name")),
              keys = list("Genre ID")
            )
          ),
          attrs_order = c(
            "Title",
            "Author",
            "Author Nationality",
            "Format",
            "Price",
            "Subject",
            "Pages",
            "Thickness",
            "Genre ID",
            "Genre Name",
            "Publisher ID"
          )
        ),
        references = list(
          list("Book", "Title", "Format Price", "Title"),
          list("Book", "Author", "Author", "Author"),
          list("Book", "Genre ID", "Genre", "Genre ID")
        ),
        name = "Book"
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
          "records",
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
          "records",
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
          "records",
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
          "records",
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
        gv(db),
        expected_string
      )
    })
    it("doesn't give a graph ID if database name is missing", {
      db <- database(
        relation(
          list(
            a = list(
              df = data.frame(a = 1:4, b = 1:2),
              keys = list("a")
            )
          ),
          attrs_order = c("a", "b")
        ),
        references = list(),
        name = NA_character_
      )
      plot_string <- gv(db)
      expect_identical(substr(plot_string, 1, 9), "digraph {")
    })
    it("only gives each attribute pair in foreign key references once", {
      db <- database_schema(
        relation_schema(
          list(
            a_b = list(letters[1:3], list(c("a", "b"), c("b", "c"))),
            a_b2 = list(letters[1:3], list(c("a", "b"), c("b", "c")))
          ),
          letters[1:3]
        ),
        references = list(
          list("a_b", c("a", "b"), "a_b2", c("a", "b")),
          list("a_b", c("b", "c"), "a_b2", c("b", "c"))
        )
      ) |>
        create()
      plot_string <- gv(db)
      expect_length(
        gregexpr("\\n  a_b.FROM_b -> a_b_2.TO_b", plot_string)[[1]],
        1L
      )
    })
  })
  describe("relation", {
    it("expects non-empty relation names", {
      forall(
        gen_df(6, 7),
        \(df) {
          rl <- subrelations(autodb(df))
          if (length(rl) == 0)
            discard()
          attr(rl, "names")[[1]] <- "" # skip name guard
          expect_error(
            gv(rl),
            "^relation names can not be zero characters in length$"
          )
        }
      )
    })
    it("works for synthesise >> create outputs", {
      forall(
        gen_flat_deps(7, 20, to = 20L),
        synthesise %>>% create %>>% gv %>>% expect_no_error
      )
    })
    it("works for generated cases", {
      forall(
        gen.relation(letters[1:6], from = 0, to = 8),
        gv %>>% expect_no_error
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      rel_dum <- create(synthesise(discover(table_dum, 1)))
      rel_dee <- create(synthesise(discover(table_dee, 1)))
      expect_no_error(gv(rel_dum))
      expect_no_error(gv(rel_dee))
    })
  })
  describe("database_schema", {
    it("expects non-empty relation schema names", {
      forall(
        gen_df(6, 7),
        \(df) {
          ds <- normalise(discover(df, 1))
          if (length(ds) == 0)
            discard()
          attr(ds, "names")[[1]] <- "" # skip schema name guard
          expect_error(
            gv(ds),
            "^relation schema names can not be zero characters in length$"
          )
        }
      )
    })
    it("works for normalise/autoref outputs", {
      forall(
        gen_flat_deps(7, 20, to = 20L),
        normalise %>>% gv %>>% expect_no_error
      )
    })
    it("works for generated cases", {
      forall(
        gen.database_schema(letters[1:8], 0, 10, same_attr_name = FALSE),
        gv %>>% expect_no_error
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      schema_dum <- normalise(discover(table_dum, 1))
      schema_dee <- normalise(discover(table_dee, 1))
      expect_no_error(gv(schema_dum))
      expect_no_error(gv(schema_dee))
    })
    it("converts attribute/df names to snake case for labels (inc. spaces, periods)", {
      schema <- relation_schema(
        list(
          `Genre ID` = list(c("Genre ID", "Genre Name"), list("Genre ID"))
        ),
        attrs_order = c("Genre ID", "Genre Name")
      ) |>
        database_schema(references = list())
      expected_string <- paste(
        "digraph book {",
        "  rankdir = \"LR\"",
        "  node [shape=plaintext];",
        "",
        "  genre_id [label = <",
        "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
        "    <TR><TD COLSPAN=\"2\">Genre ID</TD></TR>",
        "    <TR><TD PORT=\"TO_genre_id\">Genre ID</TD><TD PORT=\"FROM_genre_id\" BGCOLOR=\"black\"></TD></TR>",
        "    <TR><TD PORT=\"TO_genre_name\">Genre Name</TD><TD PORT=\"FROM_genre_name\"></TD></TR>",
        "    </TABLE>>];",
        "",
        "",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        gv(schema, "book"),
        expected_string
      )
    })
    it("doesn't give a graph ID if name is missing", {
      schema <- relation_schema(
        list(a = list(c("a", "b"), list("a"))),
        attrs_order = c("a", "b")
      ) |>
        database_schema(references = list())
      plot_string <- gv(schema)
      expect_identical(substr(plot_string, 1, 9), "digraph {")
    })
    it("gives the foreign key references", {
      schema <- relation_schema(
        list(
          a = list(c("a", "b"), list("a")),
          b = list(c("b", "c"), list("b"))
        ),
        c("a", "b", "c")
      ) |>
        database_schema(references = list(list("a", "b", "b", "b")))
      plot_string <- gv(schema)
      expect_true(grepl("\\n  a.FROM_b -> b.TO_b", plot_string))

      schema <- relation_schema(
        list(
          a = list(c("a", "b"), list("a")),
          b = list(c("b", "c"), list("b"))
        ),
        c("a", "b", "c")
      ) |>
        database_schema(references = list(list("a", "a", "b", "b")))
      plot_string <- gv(schema)
      expect_true(grepl("\\n  a.FROM_a -> b.TO_b", plot_string))
    })
    it("only gives each attribute pair in foreign key references once", {
      schema <- database_schema(
        relation_schema(
          list(
            a_b = list(letters[1:3], list(c("a", "b"), c("b", "c"))),
            a_b2 = list(letters[1:3], list(c("a", "b"), c("b", "c")))
          ),
          letters[1:3]
        ),
        references = list(
          list("a_b", c("a", "b"), "a_b2", c("a", "b")),
          list("a_b", c("b", "c"), "a_b2", c("b", "c"))
        )
      )
      plot_string <- gv(schema)
      expect_length(
        gregexpr("\\n  a_b.FROM_b -> a_b_2.TO_b", plot_string)[[1]],
        1L
      )
    })
  })
  describe("relation_schema", {
    it("expects non-empty relation schema names", {
      forall(
        gen_df(6, 7),
        \(df) {
          rs <- subschemas(normalise(discover(df, 1)))
          if (length(rs) == 0)
            discard()
          attr(rs, "names")[[1]] <- "" # skip schema name guard
          expect_error(
            gv(rs),
            "^relation schema names can not be zero characters in length$"
          )
        }
      )
    })
    it("works for synthesise outputs", {
      forall(
        gen_flat_deps(7, 20, to = 20L),
        synthesise %>>% gv %>>% expect_no_error
      )
    })
    it("works for generated cases", {
      forall(
        gen.relation_schema(letters[1:8], 0, 10),
        gv %>>% expect_no_error
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      schema_dum <- synthesise(discover(table_dum, 1))
      schema_dee <- synthesise(discover(table_dee, 1))
      expect_no_error(gv(schema_dum))
      expect_no_error(gv(schema_dee))
    })
    it("converts attribute/df names to snake case for labels (inc. spaces, periods)", {
      schema <- relation_schema(
        list(
          `Genre ID` = list(
            c("Genre ID", "Genre Name"),
            list("Genre ID")
          )
        ),
        c("Genre ID", "Genre Name")
      )
      expected_string <- paste(
        "digraph book {",
        "  rankdir = \"LR\"",
        "  node [shape=plaintext];",
        "",
        "  genre_id [label = <",
        "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
        "    <TR><TD COLSPAN=\"2\">Genre ID</TD></TR>",
        "    <TR><TD PORT=\"TO_genre_id\">Genre ID</TD><TD PORT=\"FROM_genre_id\" BGCOLOR=\"black\"></TD></TR>",
        "    <TR><TD PORT=\"TO_genre_name\">Genre Name</TD><TD PORT=\"FROM_genre_name\"></TD></TR>",
        "    </TABLE>>];",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        gv(schema, "book"),
        expected_string
      )
    })
    it("doesn't give a graph ID if name is missing", {
      schema <- relation_schema(
        list(
          a = list(c("a", "b"), list("a"))
        ),
        c("a", "b")
      )
      plot_string <- gv(schema)
      expect_identical(substr(plot_string, 1, 9), "digraph {")
    })
  })
  describe("data.frame", {
    it("expects name to be non-empty", {
      df <- data.frame(a = 1:3)
      expect_error(gv(df, ""), "^name must be non-empty$")
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      expect_no_error(gv(table_dum, "table_dum"))
      expect_no_error(gv(table_dee, "table_dee"))
    })
    it("works for generated cases", {
      forall(
        list(gen_df(6, 7), gen_attr_name(5)),
        gv %>>% expect_no_error,
        curry = TRUE
      )
    })
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
            "rows",
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
            "rows",
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
