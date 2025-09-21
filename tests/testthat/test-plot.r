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
      paste0("  \"", df_label, "\" [label = <"),
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
    it("expects a length-one character name", {
      forall(
        gen.database(letters[1:6], 0, 4),
        \(db) expect_error(gv(db, c("a", "b")))
      )
    })
    it("works for autodb output", {
      forall(
        gen_df(6, 7),
        autodb %>>% gv %>>% expect_errorless
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      db_dum <- autodb(table_dum)
      db_dee <- autodb(table_dee)
      expect_errorless(gv(db_dum))
      expect_errorless(gv(db_dee))
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
            "Pages",
            "Thickness",
            "Genre ID",
            "Genre Name",
            "Publisher ID"
          )
        ),
        references = list(
          list("Format Price", "Title", "Book", "Title"),
          list("Book", "Author", "Author", "Author"),
          list("Book", "Genre ID", "Genre", "Genre ID")
        )
      )
      expected_string <- paste(
        "digraph \"Book\" {",
        "  rankdir = \"LR\"",
        "  node [shape=plaintext];",

        "",

        test_df_strings(
          "Book",
          "Book",
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
          "Format_Price",
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
          "Author",
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
          "Genre",
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

        "  \"Format_Price\":FROM_title -> \"Book\":TO_title;",
        "  \"Book\":FROM_author -> \"Author\":TO_author;",
        "  \"Book\":FROM_genre_id -> \"Genre\":TO_genre_id;",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        gv(db, name = "Book"),
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
            "Pages",
            "Thickness",
            "Genre ID",
            "Genre Name",
            "Publisher ID"
          )
        ),
        references = list(
          list("Format Price", "Title", "Book", "Title"),
          list("Book", "Author", "Author", "Author"),
          list("Book", "Genre ID", "Genre", "Genre ID")
        )
      )
      expected_string <- paste(
        "digraph \"Book\" {",
        "  rankdir = \"LR\"",
        "  node [shape=plaintext];",

        "",

        test_df_strings(
          "Book",
          "Book",
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
          "Format_Price",
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
          "Author",
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
          "Genre",
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

        "  \"Format_Price\":FROM_title -> \"Book\":TO_title;",
        "  \"Book\":FROM_author -> \"Author\":TO_author;",
        "  \"Book\":FROM_genre_id -> \"Genre\":TO_genre_id;",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        gv(db, name = "Book"),
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
        references = list()
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
    it("uses HTML escape sequences for &<>\" in main name and relation/attribute names", {
      rs <- relation_schema(
        list(
          `<rel&1>` = list(c("a<1 & \"b\">2", "d"), list("a<1 & \"b\">2")),
          `<rel&2>` = list(c("a<1 & \"b\">2", "e"), list(c("a<1 & \"b\">2", "e")))
        ),
        c("a<1 & \"b\">2", "d", "e")
      )
      ds <- database_schema(
        rs,
        list(list("<rel&2>", "a<1 & \"b\">2", "<rel&1>", "a<1 & \"b\">2"))
      )
      db <- create(ds)
      expect_identical(
        gv(db, "<Database & Test>"),
        paste(
          "digraph \"_Database___Test_\" {",
          "  rankdir = \"LR\"",
          "  node [shape=plaintext];",
          "",
          '  \"_rel_1_\" [label = <',
          '    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">',
          '    <TR><TD COLSPAN="3">&lt;rel&amp;1&gt; (0 records)</TD></TR>',
          '    <TR><TD PORT="TO_a_1____b__2">a&lt;1 &amp; &quot;b&quot;&gt;2</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_a_1____b__2">logical</TD></TR>',
          '    <TR><TD PORT="TO_d">d</TD><TD></TD><TD PORT="FROM_d">logical</TD></TR>',
          '    </TABLE>>];',
          '  \"_rel_2_\" [label = <',
          '    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">',
          '    <TR><TD COLSPAN="3">&lt;rel&amp;2&gt; (0 records)</TD></TR>',
          '    <TR><TD PORT="TO_a_1____b__2">a&lt;1 &amp; &quot;b&quot;&gt;2</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_a_1____b__2">logical</TD></TR>',
          '    <TR><TD PORT="TO_e">e</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_e">logical</TD></TR>',
          '    </TABLE>>];',
          "",
          "  \"_rel_2_\":FROM_a_1____b__2 -> \"_rel_1_\":TO_a_1____b__2;",
          "}",
          "",
          sep = "\n"
        )
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
    it("expects a length-one character name", {
      forall(
        gen.relation(letters[1:6], 0, 4),
        \(rel) expect_error(gv(rel, c("a", "b")))
      )
    })
    it("works for synthesise >> create outputs", {
      forall(
        gen_flat_deps(7, 20, to = 20L),
        synthesise %>>% create %>>% gv %>>% expect_errorless
      )
    })
    it("works for generated cases", {
      forall(
        gen.relation(letters[1:6], from = 0, to = 8),
        gv %>>% expect_errorless
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      rel_dum <- create(synthesise(discover(table_dum)))
      rel_dee <- create(synthesise(discover(table_dee)))
      expect_errorless(gv(rel_dum))
      expect_errorless(gv(rel_dee))
    })
    it("uses HTML escape sequences for &<>\" in main name and relation/attribute names", {
      rs <- relation_schema(
        list(`<rel&1>` = list(c("a<1 & b>2", "d"), list("a<1 & b>2"))),
        c("a<1 & b>2", "d")
      )
      rel <- create(rs)
      expect_identical(
        gv(rel, "<Relation & Schema | Test>"),
        paste(
          "digraph \"_Relation___Schema___Test_\" {",
          "  rankdir = \"LR\"",
          "  node [shape=plaintext];",
          "",
          '  \"_rel_1_\" [label = <',
          '    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">',
          '    <TR><TD COLSPAN="3">&lt;rel&amp;1&gt; (0 records)</TD></TR>',
          '    <TR><TD PORT="TO_a_1___b_2">a&lt;1 &amp; b&gt;2</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_a_1___b_2">logical</TD></TR>',
          '    <TR><TD PORT="TO_d">d</TD><TD></TD><TD PORT="FROM_d">logical</TD></TR>',
          '    </TABLE>>];',
          "}",
          "",
          sep = "\n"
        )
      )
    })
  })
  describe("database_schema", {
    it("expects non-empty relation schema names", {
      forall(
        gen_df(6, 7),
        \(df) {
          ds <- normalise(discover(df))
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
    it("expects a length-one character name", {
      forall(
        gen.database_schema(letters[1:6], 0, 4),
        \(ds) expect_error(gv(ds, c("a", "b")))
      )
    })
    it("works for normalise/autoref outputs", {
      forall(
        gen_flat_deps(7, 20, to = 20L),
        normalise %>>% gv %>>% expect_errorless
      )
    })
    it("works for generated cases", {
      forall(
        gen.database_schema(letters[1:8], 0, 10, same_attr_name = FALSE),
        gv %>>% expect_errorless
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      schema_dum <- normalise(discover(table_dum))
      schema_dee <- normalise(discover(table_dee))
      expect_errorless(gv(schema_dum))
      expect_errorless(gv(schema_dee))
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
        "digraph \"book\" {",
        "  rankdir = \"LR\"",
        "  node [shape=plaintext];",
        "",
        "  \"Genre_ID\" [label = <",
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
      expect_true(grepl("\n  \"a\":FROM_b -> \"b\":TO_b", plot_string, fixed = TRUE))

      schema <- relation_schema(
        list(
          a = list(c("a", "b"), list("a")),
          b = list(c("b", "c"), list("b"))
        ),
        c("a", "b", "c")
      ) |>
        database_schema(references = list(list("a", "a", "b", "b")))
      plot_string <- gv(schema)
      expect_true(grepl("\n  \"a\":FROM_a -> \"b\":TO_b", plot_string, fixed = TRUE))
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
    it("uses HTML escape sequences for &<>\" in main name and relation/attribute names", {
      rs <- relation_schema(
        list(
          `<rel&1>` = list(c("a<1 & b>2", "d"), list("a<1 & b>2")),
          `<rel&2>` = list(c("a<1 & b>2", "e"), list(c("a<1 & b>2", "e")))
        ),
        c("a<1 & b>2", "d", "e")
      )
      ds <- database_schema(
        rs,
        list(list("<rel&2>", "a<1 & b>2", "<rel&1>", "a<1 & b>2"))
      )
      expect_identical(
        gv(ds, "<Database & Schema | Test>"),
        paste(
          "digraph \"_Database___Schema___Test_\" {",
          "  rankdir = \"LR\"",
          "  node [shape=plaintext];",
          "",
          '  \"_rel_1_\" [label = <',
          '    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">',
          '    <TR><TD COLSPAN="2">&lt;rel&amp;1&gt;</TD></TR>',
          '    <TR><TD PORT="TO_a_1___b_2">a&lt;1 &amp; b&gt;2</TD><TD PORT="FROM_a_1___b_2" BGCOLOR="black"></TD></TR>',
          '    <TR><TD PORT="TO_d">d</TD><TD PORT="FROM_d"></TD></TR>',
          '    </TABLE>>];',
          '  \"_rel_2_\" [label = <',
          '    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">',
          '    <TR><TD COLSPAN="2">&lt;rel&amp;2&gt;</TD></TR>',
          '    <TR><TD PORT="TO_a_1___b_2">a&lt;1 &amp; b&gt;2</TD><TD PORT="FROM_a_1___b_2" BGCOLOR="black"></TD></TR>',
          '    <TR><TD PORT="TO_e">e</TD><TD PORT="FROM_e" BGCOLOR="black"></TD></TR>',
          '    </TABLE>>];',
          "",
          "  \"_rel_2_\":FROM_a_1___b_2 -> \"_rel_1_\":TO_a_1___b_2;",
          "}",
          "",
          sep = "\n"
        )
      )
    })
  })
  describe("relation_schema", {
    it("expects non-empty relation schema names", {
      forall(
        gen_df(6, 7),
        \(df) {
          rs <- subschemas(normalise(discover(df)))
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
    it("expects a length-one character name", {
      forall(
        list(
          gen.relation_schema(letters[1:6], 0, 4),
          gen.choice(
            gen.element(c(0L, 2:6)) |>
              gen.and_then(\(n) gen.sample_resampleable(letters[1:6], of = n)),
            gen.int(6)
          )
        ),
        gv %>>% expect_error,
        curry = TRUE
      )
    })
    it("works for synthesise outputs", {
      forall(
        gen_flat_deps(7, 20, to = 20L),
        synthesise %>>% gv %>>% expect_errorless
      )
    })
    it("works for generated cases", {
      forall(
        gen.relation_schema(letters[1:8], 0, 10),
        gv %>>% expect_errorless
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      schema_dum <- synthesise(discover(table_dum))
      schema_dee <- synthesise(discover(table_dee))
      expect_errorless(gv(schema_dum))
      expect_errorless(gv(schema_dee))
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
        "digraph \"book\" {",
        "  rankdir = \"LR\"",
        "  node [shape=plaintext];",
        "",
        "  \"Genre_ID\" [label = <",
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
    it("uses HTML escape sequences for &<>\" in main name and relation/attribute names", {
      rs <- relation_schema(
        list(`<rel&1>` = list(c("a<1 & b>2", "d"), list("a<1 & b>2"))),
        c("a<1 & b>2", "d")
      )
      expect_identical(
        gv(rs, "<Relation & Schema | Test>"),
        paste(
          "digraph \"_Relation___Schema___Test_\" {",
          "  rankdir = \"LR\"",
          "  node [shape=plaintext];",
          "",
          '  \"_rel_1_\" [label = <',
          '    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">',
          '    <TR><TD COLSPAN="2">&lt;rel&amp;1&gt;</TD></TR>',
          '    <TR><TD PORT="TO_a_1___b_2">a&lt;1 &amp; b&gt;2</TD><TD PORT="FROM_a_1___b_2" BGCOLOR="black"></TD></TR>',
          '    <TR><TD PORT="TO_d">d</TD><TD PORT="FROM_d"></TD></TR>',
          '    </TABLE>>];',
          "}",
          "",
          sep = "\n"
        )
      )
    })
  })
  describe("data.frame", {
    it("expects name to be non-empty", {
      df <- data.frame(a = 1:3)
      expect_error(gv(df, ""), "^name must be non-empty$")
    })
    it("expects a length-one character name", {
      forall(
        gen_df(4, 6),
        \(df) expect_error(gv(df, c("a", "b")))
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      expect_errorless(gv(table_dum, "table_dum"))
      expect_errorless(gv(table_dee, "table_dee"))
    })
    it("works for generated cases", {
      forall(
        list(gen_df(6, 7), gen_attr_name(5)),
        gv %>>% expect_errorless,
        curry = TRUE
      )
      forall(
        gen_df(6, 7),
        gv %>>% expect_errorless
      )
    })
    it("generates a name if not given one", {
      df <- data.frame(a = 1:3)
      g <- strsplit(gv(df), "\n", fixed = TRUE)[[1]]
      expect_identical(g[[1]], "digraph \"data\" {")
      expect_identical(
        g[[7]],
        "    <TR><TD COLSPAN=\"2\">data (3 rows)</TD></TR>"
      )
    })
    it("creates a Graphviz HTML-like expression for the data.frame", {
      df <- data.frame(
        a = 1:2, b = letters[1:2]
      )
      expect_identical(
        gv(df, "table"),
        paste(
          "digraph \"table\" {",
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
          "digraph \"Table_Test\" {",
          "  rankdir = \"LR\"",
          "  node [shape=plaintext];",
          "",
          test_df_strings(
            "Table Test",
            "Table_Test",
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
    it("uses HTML escape sequences for &<>\" in main name and attribute names", {
      df <- data.frame(
        a = 1:2, b = letters[1:2]
      ) |>
        stats::setNames(c("a", "b<2 & c>3"))
      expect_identical(
        gv(df, "<Table & Test>"),
        paste(
          "digraph \"_Table___Test_\" {",
          "  rankdir = \"LR\"",
          "  node [shape=plaintext];",
          "",
          test_df_strings(
            "&lt;Table &amp; Test&gt;",
            "_Table___Test_",
            2,
            "rows",
            c("a", "b&lt;2 &amp; c&gt;3"),
            c("a", "b_2___c_3"),
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

describe("d2", {
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
            d2(db),
            "^relation names can not be zero characters in length$"
          )
        }
      )
    })
    it("expects a length-one character name", {
      forall(
        gen.database(letters[1:6], 0, 4),
        \(db) expect_error(d2(db, name = c("a", "b")))
      )
    })
    it("uses non-missing name as a cluster", {
      rs <- relation_schema(
        list(
          Measurement = list(c("Chick", "Time", "weight"), list(c("Chick", "Time"))),
          Chick = list(c("Chick", "Diet"), list("Chick"))
        ),
        c("Chick", "Diet", "Time", "weight")
      )
      ds <- database_schema(
        rs,
        list(list("Measurement", "Chick", "Chick", "Chick"))
      )
      db <- create(ds)
      base_text <- c(
        "\"Measurement\": \"Measurement (0 records)\" {",
        "  shape: sql_table",
        "  \"Chick\": logical {constraint: [PK; FK1]}",
        "  \"Time\": logical {constraint: [PK]}",
        "  \"weight\": logical",
        "}",
        "\"Chick\": \"Chick (0 records)\" {",
        "  shape: sql_table",
        "  \"Chick\": logical {constraint: [PK]}",
        "  \"Diet\": logical",
        "}"
      )
      tableref_text <- "\"Measurement\" -> \"Chick\""
      attrref_text <- "\"Measurement\".\"Chick\" -> \"Chick\".\"Chick\""
      expect_identical(
        d2(db, reference = "relation"),
        paste(c(base_text, "", tableref_text, ""), collapse = "\n")
      )
      expect_identical(
        d2(db, reference = "attr"),
        paste(c(base_text, "", attrref_text, ""), collapse = "\n")
      )
      expect_identical(
        d2(db, name = "ChickWeight", reference = "relation"),
        paste(
          c(
            "\"ChickWeight\" {",
            paste0("  ", base_text),
            "",
            paste0("  ", tableref_text),
            "}",
            ""
          ),
          collapse = "\n"
        )
      )
      expect_identical(
        d2(db, name = "ChickWeight", reference = "attr"),
        paste(
          c(
            "\"ChickWeight\" {",
            paste0("  ", base_text),
            "",
            paste0("  ", attrref_text),
            "}",
            ""
          ),
          collapse = "\n"
        )
      )
    })
    it("works for normalise >> create output", {
      forall(
        gen_flat_deps(7, 20, to = 20L),
        normalise %>>% create %>>% d2 %>>% expect_errorless
      )
    })
    it("works for generated cases", {
      forall(
        gen.database(letters[1:6], from = 0, to = 8),
        d2 %>>% expect_errorless
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      db_dum <- autodb(table_dum)
      db_dee <- autodb(table_dee)
      expect_errorless(d2(db_dum))
      expect_errorless(d2(db_dee))
    })
    it("leaves attribute/df names as-is (i.e. no snake case conversion)", {
      schema <- relation_schema(
        list(
          `Genre ID` = list(
            c("Genre ID", "Genre Name"),
            list("Genre ID")
          )
        ),
        c("Genre ID", "Genre Name")
      )
      ds <- database_schema(schema, list())
      db <- create(ds)
      expected_string <- paste(
        "\"Genre ID\": \"Genre ID (0 records)\" {",
        "  shape: sql_table",
        "  \"Genre ID\": logical {constraint: [PK]}",
        "  \"Genre Name\": logical",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        d2(db),
        expected_string
      )
    })
    it("leaves relation/attribute names as-is (no HTML escape sequences for &<>\")", {
      rs <- relation_schema(
        list(`<rel&1>` = list(c("a<1 & b>2", "d"), list("a<1 & b>2"))),
        c("a<1 & b>2", "d")
      )
      ds <- database_schema(rs, list())
      db <- create(ds)
      expect_identical(
        d2(db),
        paste(
          '"<rel&1>": "<rel&1> (0 records)" {',
          "  shape: sql_table",
          '  "a<1 & b>2": logical {constraint: [PK]}',
          '  "d": logical',
          "}",
          "",
          sep = "\n"
        )
      )
    })
    it("gives keys as PK and UNQ{number} constraints", {
      rs <- relation_schema(
        list(
          `a + b = c` = list(
            letters[1:3],
            list(letters[1:2], letters[c(1, 3)], letters[2:3])
          )
        ),
        letters[1:3]
      )
      ds <- database_schema(rs, list())
      db <- create(ds)
      text <- c(
        "\"a + b = c\": \"a + b = c (0 records)\" {",
        "  shape: sql_table",
        "  \"a\": logical {constraint: [PK; UNQ1]}",
        "  \"b\": logical {constraint: [PK; UNQ2]}",
        "  \"c\": logical {constraint: [UNQ1; UNQ2]}",
        "}",
        ""
      )
      expect_identical(
        d2(db),
        paste(text, collapse = "\n")
      )
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
      plot_string <- d2(db)
      expect_length(
        gregexpr("\\n  a_b.FROM_b -> a_b_2.TO_b", plot_string)[[1]],
        1L
      )
    })
    it("gives keys as PK and UNQ{number} constraints", {
      rs <- relation_schema(
        list(
          `a + b = c` = list(
            letters[1:3],
            list(letters[1:2], letters[c(1, 3)], letters[2:3])
          )
        ),
        letters[1:3]
      )
      ds <- database_schema(rs, references = list())
      text <- c(
        "\"a + b = c\": \"a + b = c (0 records)\" {",
        "  shape: sql_table",
        "  \"a\": logical {constraint: [PK; UNQ1]}",
        "  \"b\": logical {constraint: [PK; UNQ2]}",
        "  \"c\": logical {constraint: [UNQ1; UNQ2]}",
        "}",
        ""
      )
      expect_identical(
        d2(create(ds)),
        paste(text, collapse = "\n")
      )
    })
    it("only gives table references once each", {
      rs <- relation_schema(
        list(
          Measurement = list(c("Chick", "Time", "weight"), list(c("Chick", "Time"))),
          Diet = list(c("Chick", "Time", "Diet"), list("Diet", c("Chick", "Time")))
        ),
        c("Chick", "Diet", "Time", "weight")
      )
      ds <- database_schema(
        rs,
        references = list(
          list("Measurement", c("Chick", "Time"), "Diet", c("Chick", "Time"))
        )
      )
      db <- create(ds)
      main_text <- c(
        "\"Measurement\": \"Measurement (0 records)\" {",
        "  shape: sql_table",
        "  \"Chick\": logical {constraint: [PK; FK1]}",
        "  \"Time\": logical {constraint: [PK; FK1]}",
        "  \"weight\": logical",
        "}",
        "\"Diet\": \"Diet (0 records)\" {",
        "  shape: sql_table",
        "  \"Diet\": logical {constraint: [PK]}",
        "  \"Chick\": logical {constraint: [UNQ1]}",
        "  \"Time\": logical {constraint: [UNQ1]}",
        "}"
      )
      tableref_text <- c(
        "",
        "\"Measurement\" -> \"Diet\""
      )
      attrref_text <- c(
        "",
        "\"Measurement\".\"Chick\" -> \"Diet\".\"Chick\"",
        "\"Measurement\".\"Time\" -> \"Diet\".\"Time\""
      )
      expect_identical(
        d2(db, reference = "relation"),
        paste(c(main_text, tableref_text, ""), collapse = "\n")
      )
      expect_identical(
        d2(db, reference = "attr"),
        paste(c(main_text, attrref_text, ""), collapse = "\n")
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
            d2(rl),
            "^relation names can not be zero characters in length$"
          )
        }
      )
    })
    it("expects a length-one character name", {
      forall(
        gen.relation(letters[1:6], 0, 4),
        \(rel) expect_error(d2(rel, name = c("a", "b")))
      )
    })
    it("uses non-missing name as a cluster", {
      rs <- relation_schema(
        list(
          Measurement = list(c("Chick", "Time", "weight"), list(c("Chick", "Time"))),
          Chick = list(c("Chick", "Diet"), list("Chick"))
        ),
        c("Chick", "Diet", "Time", "weight")
      )
      rel <- create(rs)
      base_text <- c(
        "\"Measurement\": \"Measurement (0 records)\" {",
        "  shape: sql_table",
        "  \"Chick\": logical {constraint: [PK]}",
        "  \"Time\": logical {constraint: [PK]}",
        "  \"weight\": logical",
        "}",
        "\"Chick\": \"Chick (0 records)\" {",
        "  shape: sql_table",
        "  \"Chick\": logical {constraint: [PK]}",
        "  \"Diet\": logical",
        "}"
      )
      expect_identical(
        d2(rel),
        paste(c(base_text, ""), collapse = "\n")
      )
      expect_identical(
        d2(rel, name = "ChickWeight"),
        paste(
          c(
            "\"ChickWeight\" {",
            paste0("  ", base_text),
            "}",
            ""
          ),
          collapse = "\n"
        )
      )
    })
    it("works for synthesise >> create outputs", {
      forall(
        gen_flat_deps(7, 20, to = 20L),
        synthesise %>>% create %>>% d2 %>>% expect_errorless
      )
    })
    it("works for generated cases", {
      forall(
        gen.relation(letters[1:6], from = 0, to = 8),
        d2 %>>% expect_errorless
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      rel_dum <- create(synthesise(discover(table_dum)))
      rel_dee <- create(synthesise(discover(table_dee)))
      expect_errorless(d2(rel_dum))
      expect_errorless(d2(rel_dee))
    })
    it("leaves attribute/df names as-is (i.e. no snake case conversion)", {
      schema <- relation_schema(
        list(
          `Genre ID` = list(
            c("Genre ID", "Genre Name"),
            list("Genre ID")
          )
        ),
        c("Genre ID", "Genre Name")
      )
      rel <- create(schema)
      expected_string <- paste(
        "\"Genre ID\": \"Genre ID (0 records)\" {",
        "  shape: sql_table",
        "  \"Genre ID\": logical {constraint: [PK]}",
        "  \"Genre Name\": logical",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        d2(rel),
        expected_string
      )
    })
    it("leaves relation/attribute names as-is (no HTML escape sequences for &<>\")", {
      rs <- relation_schema(
        list(`<rel&1>` = list(c("a<1 & b>2", "d"), list("a<1 & b>2"))),
        c("a<1 & b>2", "d")
      )
      rel <- create(rs)
      expect_identical(
        d2(rel),
        paste(
          '"<rel&1>": "<rel&1> (0 records)" {',
          "  shape: sql_table",
          '  "a<1 & b>2": logical {constraint: [PK]}',
          '  "d": logical',
          "}",
          "",
          sep = "\n"
        )
      )
    })
    it("gives keys as PK and UNQ{number} constraints", {
      rs <- relation_schema(
        list(
          `a + b = c` = list(
            letters[1:3],
            list(letters[1:2], letters[c(1, 3)], letters[2:3])
          )
        ),
        letters[1:3]
      )
      rel <- create(rs)
      text <- c(
        "\"a + b = c\": \"a + b = c (0 records)\" {",
        "  shape: sql_table",
        "  \"a\": logical {constraint: [PK; UNQ1]}",
        "  \"b\": logical {constraint: [PK; UNQ2]}",
        "  \"c\": logical {constraint: [UNQ1; UNQ2]}",
        "}",
        ""
      )
      expect_identical(
        d2(rel),
        paste(text, collapse = "\n")
      )
    })
  })
  describe("database_schema", {
    it("expects non-empty relation schema names", {
      forall(
        gen_df(6, 7),
        \(df) {
          rs <- subschemas(normalise(discover(df)))
          if (length(rs) == 0)
            discard()
          attr(rs, "names")[[1]] <- "" # skip schema name guard
          expect_error(
            d2(rs),
            "^relation schema names can not be zero characters in length$"
          )
        }
      )
    })
    it("expects a length-one character name", {
      forall(
        list(
          gen.database_schema(letters[1:6], 0, 4, same_attr_name = FALSE),
          gen.choice(
            gen.element(c(0L, 2:6)) |>
              gen.and_then(\(n) gen.sample_resampleable(letters[1:6], of = n)),
            gen.int(6)
          )
        ),
        d2 %>>% expect_error,
        curry = TRUE
      )
    })
    it("uses non-missing name as a cluster", {
      rs <- relation_schema(
        list(
          Measurement = list(c("Chick", "Time", "weight"), list(c("Chick", "Time"))),
          Chick = list(c("Chick", "Diet"), list("Chick"))
        ),
        c("Chick", "Diet", "Time", "weight")
      )
      ds <- database_schema(rs, list(list("Measurement", "Chick", "Chick", "Chick")))
      base_text <- c(
        "\"Measurement\": {",
        "  shape: sql_table",
        "  \"Chick\": {constraint: [PK; FK1]}",
        "  \"Time\": {constraint: [PK]}",
        "  \"weight\"",
        "}",
        "\"Chick\": {",
        "  shape: sql_table",
        "  \"Chick\": {constraint: [PK]}",
        "  \"Diet\"",
        "}"
      )
      tableref_text <- "\"Measurement\" -> \"Chick\""
      attrref_text <- "\"Measurement\".\"Chick\" -> \"Chick\".\"Chick\""
      expect_identical(
        d2(ds, reference = "relation"),
        paste(c(base_text, "", tableref_text, ""), collapse = "\n")
      )
      expect_identical(
        d2(ds, reference = "attr"),
        paste(c(base_text, "", attrref_text, ""), collapse = "\n")
      )
      expect_identical(
        d2(ds, name = "ChickWeight", reference = "relation"),
        paste(
          c(
            "\"ChickWeight\" {",
            paste0("  ", base_text),
            "",
            paste0("  ", tableref_text),
            "}",
            ""
          ),
          collapse = "\n"
        )
      )
      expect_identical(
        d2(ds, name = "ChickWeight", reference = "attr"),
        paste(
          c(
            "\"ChickWeight\" {",
            paste0("  ", base_text),
            "",
            paste0("  ", attrref_text),
            "}",
            ""
          ),
          collapse = "\n"
        )
      )
    })
    it("works for normalise/autoref outputs", {
      forall(
        gen_flat_deps(7, 20, to = 20L),
        normalise %>>% d2 %>>% expect_errorless
      )
    })
    it("works for generated cases", {
      forall(
        gen.database_schema(letters[1:8], 0, 10, same_attr_name = FALSE),
        d2 %>>% expect_errorless
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      schema_dum <- normalise(discover(table_dum))
      schema_dee <- normalise(discover(table_dee))
      expect_errorless(d2(schema_dum))
      expect_errorless(d2(schema_dee))
    })
    it("leaves attribute/df names as-is (i.e. no snake case conversion)", {
      schema <- relation_schema(
        list(
          `Genre ID` = list(
            c("Genre ID", "Genre Name"),
            list("Genre ID")
          )
        ),
        c("Genre ID", "Genre Name")
      ) |>
        database_schema(references = list())
      expected_string <- paste(
        "\"Genre ID\": {",
        "  shape: sql_table",
        "  \"Genre ID\": {constraint: [PK]}",
        "  \"Genre Name\"",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        d2(schema),
        expected_string
      )
    })
    it("leaves relation/attribute names as-is (no HTML escape sequences for &<>\")", {
      rs <- relation_schema(
        list(`<rel&1>` = list(c("a<1 & b>2", "d"), list("a<1 & b>2"))),
        c("a<1 & b>2", "d")
      )
      ds <- database_schema(rs, references = list())
      expect_identical(
        d2(rs),
        paste(
          '"<rel&1>": {',
          "  shape: sql_table",
          '  "a<1 & b>2": {constraint: [PK]}',
          '  "d"',
          "}",
          "",
          sep = "\n"
        )
      )
    })
    it("gives keys as PK and UNQ{number} constraints", {
      rs <- relation_schema(
        list(
          `a + b = c` = list(
            letters[1:3],
            list(letters[1:2], letters[c(1, 3)], letters[2:3])
          )
        ),
        letters[1:3]
      )
      ds <- database_schema(rs, references = list())
      text <- c(
        "\"a + b = c\": {",
        "  shape: sql_table",
        "  \"a\": {constraint: [PK; UNQ1]}",
        "  \"b\": {constraint: [PK; UNQ2]}",
        "  \"c\": {constraint: [UNQ1; UNQ2]}",
        "}",
        ""
      )
      expect_identical(
        d2(ds),
        paste(text, collapse = "\n")
      )
    })
    it("only gives table references once each", {
      rs <- relation_schema(
        list(
          Measurement = list(c("Chick", "Time", "weight"), list(c("Chick", "Time"))),
          Diet = list(c("Chick", "Time", "Diet"), list("Diet", c("Chick", "Time")))
        ),
        c("Chick", "Diet", "Time", "weight")
      )
      ds <- database_schema(
        rs,
        references = list(
          list("Measurement", c("Chick", "Time"), "Diet", c("Chick", "Time"))
        )
      )
      main_text <- c(
        "\"Measurement\": {",
        "  shape: sql_table",
        "  \"Chick\": {constraint: [PK; FK1]}",
        "  \"Time\": {constraint: [PK; FK1]}",
        "  \"weight\"",
        "}",
        "\"Diet\": {",
        "  shape: sql_table",
        "  \"Diet\": {constraint: [PK]}",
        "  \"Chick\": {constraint: [UNQ1]}",
        "  \"Time\": {constraint: [UNQ1]}",
        "}"
      )
      tableref_text <- c(
        "",
        "\"Measurement\" -> \"Diet\""
      )
      attrref_text <- c(
        "",
        "\"Measurement\".\"Chick\" -> \"Diet\".\"Chick\"",
        "\"Measurement\".\"Time\" -> \"Diet\".\"Time\""
      )
      expect_identical(
        d2(ds, reference = "relation"),
        paste(c(main_text, tableref_text, ""), collapse = "\n")
      )
      expect_identical(
        d2(ds, reference = "attr"),
        paste(c(main_text, attrref_text, ""), collapse = "\n")
      )
    })
  })
  describe("relation_schema", {
    it("expects non-empty relation schema names", {
      forall(
        gen_df(6, 7),
        \(df) {
          rs <- subschemas(normalise(discover(df)))
          if (length(rs) == 0)
            discard()
          attr(rs, "names")[[1]] <- "" # skip schema name guard
          expect_error(
            d2(rs),
            "^relation schema names can not be zero characters in length$"
          )
        }
      )
    })
    it("expects a length-one character name", {
      forall(
        list(
          gen.relation_schema(letters[1:6], 0, 4),
          gen.choice(
            gen.element(c(0L, 2:6)) |>
              gen.and_then(\(n) gen.sample_resampleable(letters[1:6], of = n)),
            gen.int(6)
          )
        ),
        d2 %>>% expect_error,
        curry = TRUE
      )
    })
    it("uses non-missing name as a cluster", {
      rs <- relation_schema(
        list(
          Measurement = list(c("Chick", "Time", "weight"), list(c("Chick", "Time"))),
          Chick = list(c("Chick", "Diet"), list("Chick"))
        ),
        c("Chick", "Diet", "Time", "weight")
      )
      base_text <- c(
        "\"Measurement\": {",
        "  shape: sql_table",
        "  \"Chick\": {constraint: [PK]}",
        "  \"Time\": {constraint: [PK]}",
        "  \"weight\"",
        "}",
        "\"Chick\": {",
        "  shape: sql_table",
        "  \"Chick\": {constraint: [PK]}",
        "  \"Diet\"",
        "}"
      )
      expect_identical(
        d2(rs),
        paste(c(base_text, ""), collapse = "\n")
      )
      expect_identical(
        d2(rs, name = "ChickWeight"),
        paste(
          c(
            "\"ChickWeight\" {",
            paste0("  ", base_text),
            "}",
            ""
          ),
          collapse = "\n"
        )
      )
    })
    it("works for synthesise outputs", {
      forall(
        gen_flat_deps(7, 20, to = 20L),
        synthesise %>>% d2 %>>% expect_errorless
      )
    })
    it("works for generated cases", {
      forall(
        gen.relation_schema(letters[1:8], 0, 10),
        d2 %>>% expect_errorless
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      schema_dum <- synthesise(discover(table_dum))
      schema_dee <- synthesise(discover(table_dee))
      expect_errorless(d2(schema_dum))
      expect_errorless(d2(schema_dee))
    })
    it("leaves attribute/df names as-is (i.e. no snake case conversion)", {
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
        "\"Genre ID\": {",
        "  shape: sql_table",
        "  \"Genre ID\": {constraint: [PK]}",
        "  \"Genre Name\"",
        "}",
        "",
        sep = "\n"
      )
      expect_identical(
        d2(schema),
        expected_string
      )
    })
    it("leaves relation/attribute names as-is (no HTML escape sequences for &<>\")", {
      rs <- relation_schema(
        list(`<rel&1>` = list(c("a<1 & b>2", "d"), list("a<1 & b>2"))),
        c("a<1 & b>2", "d")
      )
      expect_identical(
        d2(rs),
        paste(
          '"<rel&1>": {',
          "  shape: sql_table",
          '  "a<1 & b>2": {constraint: [PK]}',
          '  "d"',
          "}",
          "",
          sep = "\n"
        )
      )
    })
    it("gives keys as PK and UNQ{number} constraints", {
      rs <- relation_schema(
        list(
          `a + b = c` = list(
            letters[1:3],
            list(letters[1:2], letters[c(1, 3)], letters[2:3])
          )
        ),
        letters[1:3]
      )
      text <- c(
        "\"a + b = c\": {",
        "  shape: sql_table",
        "  \"a\": {constraint: [PK; UNQ1]}",
        "  \"b\": {constraint: [PK; UNQ2]}",
        "  \"c\": {constraint: [UNQ1; UNQ2]}",
        "}",
        ""
      )
      expect_identical(
        d2(rs),
        paste(text, collapse = "\n")
      )
    })
  })
  describe("data.frame", {
    it("expects name to be non-empty", {
      df <- data.frame(a = 1:3)
      expect_error(d2(df, ""), "^name must be non-empty$")
    })
    it("expects a length-one character name", {
      forall(
        gen_df(4, 6),
        \(df) expect_error(d2(df, c("a", "b")))
      )
    })
    it("works for degenerate cases", {
      table_dum <- data.frame()
      table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
      expect_errorless(d2(table_dum, "table_dum"))
      expect_errorless(d2(table_dee, "table_dee"))
    })
    it("works for generated cases", {
      forall(
        list(gen_df(6, 7), gen_attr_name(5)),
        d2 %>>% expect_errorless,
        curry = TRUE
      )
      forall(
        gen_df(6, 7),
        d2 %>>% expect_errorless
      )
    })
    it("generates a name if not given one", {
      df <- data.frame(a = 1:3)
      g <- strsplit(d2(df), "\n", fixed = TRUE)[[1]]
      expect_identical(g[[1]], "\"data\": \"data (3 rows)\" {")
    })
    it("creates a d2 expression for the data.frame", {
      df <- data.frame(
        a = 1:2, b = letters[1:2]
      )
      expect_identical(
        d2(df, "table"),
        paste(
          "\"table\": \"table (2 rows)\" {",
          "  shape: sql_table",
          "  \"a\": integer",
          "  \"b\": character",
          "}",
          "",
          sep = "\n"
        )
      )
    })
    it("uses attribute/df names as-is (no snake case, no HTML escape sequences for &<>\", etc)", {
      df <- data.frame(
        a = 1:2, b = letters[1:2]
      ) |>
        stats::setNames(c("A 1", "b.2"))
      expect_identical(
        d2(df, "Table Test"),
        paste(
          "\"Table Test\": \"Table Test (2 rows)\" {",
          "  shape: sql_table",
          "  \"A 1\": integer",
          "  \"b.2\": character",
          "}",
          "",
          sep = "\n"
        )
      )
      df <- data.frame(
        a = 1:2, b = letters[1:2]
      ) |>
        stats::setNames(c("a", "b<2 & c>3"))
      expect_identical(
        d2(df, "Table Test"),
        paste(
          "\"Table Test\": \"Table Test (2 rows)\" {",
          "  shape: sql_table",
          "  \"a\": integer",
          "  \"b<2 & c>3\": character",
          "}",
          "",
          sep = "\n"
        )
      )
    })
  })
})
