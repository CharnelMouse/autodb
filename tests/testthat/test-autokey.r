describe("autokey", {
  it("returns valid single relations", {
    forall(
      gen_df(6, 7),
      autokey %>>% is_valid_relation
    )
    forall(
      gen_df(6, 7),
      autokey %>>% with_args(expect_length, 1)
    )
  })
  it("is the same as biapply(df_unique, discover_keys) >> relation", {
    df <- data.frame(a = 1:4, b = 1:2)
    relation <- autokey(df)
    relation2 <- discover_keys(df) |>
      (\(keys) list(data = list(df = df_unique(df), keys = keys)))() |>
      relation(attrs_order = names(df))
    expect_identical(relation, relation2)
    expect_silent(gv(relation))

    forall(
      list(
        gen_df(6, 7),
        digits = gen.element(c(7:1, NA_integer_))
      ),
      \(df, digits, ensure_lossless, remove_avoidable) {
        expect_identical(
          autokey(
            df,
            digits = digits
          ),
          relation_schema(
            list(data = list(names(df), discover_keys(df, digits = digits))),
            attrs_order = names(df)
          ) |>
            create_insert(df = df, digits = digits)
        )
      },
      curry = TRUE
    )
  })
  it("adds keys to the given data frame", {
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
    relation <- autokey(df)
    expect_named(relation, "data")
    expect_length(relation, 1)
    expect_true(!anyDuplicated(records(relation)))
    expect_true(is.element("Price", keys(relation)$data))
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
    relation_nonfiltered <- autokey(df)
    relation_filtered <- autokey(df, exclude_class = "numeric")
    expect_setequal(
      keys(relation_filtered)$data,
      Filter(
        \(key) all(vapply(df[key], Negate(inherits), logical(1), "numeric")),
        keys(relation_nonfiltered)$data
      )
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
    relation_nonfiltered <- autokey(df)
    relation_filtered <- autokey(df, exclude = "Price")
    expect_setequal(
      keys(relation_filtered)$data,
      Filter(
        \(ks) !is.element("Price", ks),
        keys(relation_nonfiltered)$data
      )
    )
  })
  it("correctly handles attributes with non-df-standard names", {
    df <- data.frame(1:3, c(1, 1, 2), c(1, 2, 2)) |>
      stats::setNames(c("A 1", "B 2", "C 3"))
    relation <- autokey(df)
    expect_identical(attrs(relation)[[1]], c("A 1", "B 2", "C 3"))
  })
  it("decomposes zero-column data frames correctly into TABLE_DUM or TABLE_DEE", {
    table_dum <- data.frame()
    table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
    table_deux <- data.frame(a = 1:2)[, -1, drop = FALSE]
    rel_dum <- autokey(table_dum)
    rel_dee <- autokey(table_dee)
    rel_deux <- autokey(table_deux)
    expect_length(rel_dum, 1L)
    expect_length(rel_dee, 1L)
    expect_identical(keys(rel_dum)[[1]], list(character()))
    expect_identical(keys(rel_dee)[[1]], list(character()))
    expect_identical(nrow(records(rel_dum)[[1]]), 0L)
    expect_identical(nrow(records(rel_dee)[[1]]), 1L)
    expect_identical(rel_deux, rel_dee)
  })
  it("adds row names to database if added in keep_rownames", {
    x <- data.frame(
      a = c(1, 1, 1, 2, 2, 3, 3, 3, 4),
      b = c(1, 1, 1, 1, 1, 2, 2, 2, 3),
      row.names = letters[1:9]
    )
    rel <- autokey(x, keep_rownames = TRUE)
    expect_identical(
      rel,
      relation(
        list(
          data = list(
            df = cbind(data.frame(row = letters[1:9]), x),
            keys = list("row")
          )
        ),
        c("row", "a", "b")
      )
    )
  })
  it("can take columns that are lists, matrices, or data frames (e.g. jsonlite output)", {
    x <- data.frame(a = 1:4)
    x$b <- list(1:2, 3:5, 6:9, 1:2)
    x$c <- matrix(1:3, nrow = 4, ncol = 3)
    x$d <- data.frame(b = rep(1:2, 2), c = letters[c(1:3, 1)])
    expect_no_error(autokey(x))
  })
})
