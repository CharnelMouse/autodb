#' Generate Graphviz input text to plot databases
#'
#' Produces text input for Graphviz to make an HTML diagram of a given database.
#'
#' Each table is presented as a record-like shape, with the following elements:
#' \itemize{
#'   \item A header with the table's name, and the number of (unique) rows.
#'   \item A set of rows, one for each attribute in the table. These rows have the following contents:
#'   \itemize{
#'     \item On the left, the attribute names.
#'     \item In the middle, a depiction of the table's (candidate) keys. Each
#'     column represents a key, and a filled cell indicates that the attribute
#'     in that row is in that key. The keys are given in lexical order, with
#'     precedence given to keys with fewer attributes, and keys with attributes
#'     that appear earlier in the original table's attribute order. Default
#'     output from other package functions will thus have the primary key given
#'     first. In the future, this will be changed to always give the primary key
#'     first.
#'     \item On the right, the attribute types: specifically, the first element
#'     when passing the attribute's values into \code{\link{class}}.
#'   }
#' }
#'
#' Any foreign key references are represented by arrows
#' between the attribute pairs.
#'
#' If the database has a name, this name is attached to the resulting graph in
#' Graphviz. This is to allow easier combination of several such graphs into a
#' single image, if a user wishes to do so.
#'
#' @param db a database, as returned by \code{\link{cross_reference}} or
#'   \code{\link{autonorm}}.
#'
#' @return A scalar character, containing text input for Graphviz or the
#'   \code{DiagrammeR} package.
#' @export
gv_database <- function(db) {
  setup_string <- gv_setup_string(db$name)
  df_strings <- mapply(
    df_string,
    db$tables,
    names(db$tables)
  ) |>
    paste(collapse = "\n")
  reference_strings <- vapply(
    db$relationships,
    reference_string,
    character(1)
  ) |>
    paste(collapse = "\n")
  teardown_string <- "}\n"
  paste(
    setup_string,
    "",
    df_strings,
    "",
    reference_strings,
    teardown_string,
    sep = "\n"
  )
}


#' Generate Graphviz input text to plot database schemes
#'
#' Produces text input for Graphviz to make an HTML diagram of a given database
#' scheme.
#'
#' Each relation in the scheme is presented as a set of rows, one for each
#' attribute in the relation. These rows have the following contents:
#' \itemize{
#'   \item On the left, the attribute names.
#'   \item On the right, a depiction of the table's (candidate) keys. Each
#'   column represents a key, and a filled cell indicates that the attribute
#'   in that row is in that key. The keys are given in lexical order, with
#'   precedence given to keys with fewer attributes, and keys with attributes
#'   that appear earlier in the original table's attribute order. Default
#'   output from other package functions will thus have the primary key given
#'   first. In the future, this will be changed to always give the primary key
#'   first.
#' }
#'
#' Any foreign key references are represented by arrows
#' between the attribute pairs.
#'
#' If \code{dbs_name} is not missing, this name is attached to the resulting
#' graph in Graphviz. This is to allow easier combination of several such graphs
#' into a single image, if a user wishes to do so.
#'
#' @param dbs a database scheme, as given by \code{\link{normalise}} or
#'   \code{\link{cross_reference}}.
#' @param dbs_name a character scalar, giving the name of the scheme, if any.
#'
#' @return A scalar character, containing text input for Graphviz or the
#'   \code{DiagrammeR} package.
#' @export
gv_database_scheme <- function(dbs, dbs_name = NA_character_) {
  setup_string <- gv_setup_string(dbs_name)
  df_strings <- mapply(
    nameless_relation_string,
    seq_along(dbs$attrs),
    dbs$attrs,
    dbs$keys
  ) |>
    paste(collapse = "\n")
  reference_strings <- vapply(
    dbs$relationships,
    dbs_reference_string,
    character(1)
  ) |>
    paste(collapse = "\n")
  teardown_string <- "}\n"
  paste(
    setup_string,
    "",
    df_strings,
    "",
    reference_strings,
    teardown_string,
    sep = "\n"
  )
}

#' Generate Graphviz input text to plot single tables
#'
#' Produces text input for Graphviz to make an HTML diagram of a given table,
#' represented by a data.frame.
#'
#' The table is presented as a record-like shape, with the following elements:
#' \itemize{
#'   \item A header with the table's name, and the number of (unique) rows.
#'   \item A set of rows, one for each attribute in the table. These rows have the following contents:
#'   \itemize{
#'     \item On the left, the attribute names.
#'     \item In the middle, a depiction of the table's (candidate) keys. Each
#'     column represents a key, and a filled cell indicates that the attribute
#'     in that row is in that key. The keys are given in lexical order, with
#'     precedence given to keys with fewer attributes, and keys with attributes
#'     that appear earlier in the original table's attribute order. Default
#'     output from other package functions will thus have the primary key given
#'     first. In the future, this will be changed to always give the primary key
#'     first.
#'     \item On the right, the attribute types: specifically, the first element
#'     when passing the attribute's values into \code{\link{class}}.
#'   }
#' }
#'
#' The name of the table is also used as the name for the resulting graph, if it
#' is not missing (\code{NA_character_}). This is to allow easier combination of
#' several such graphs into a single image, if a user wishes to do so.
#'
#' @param df a data.frame, to be plotted as a record.
#' @param df_name a character scalar, giving the name of the record.
#'
#' @return A scalar character, containing text input for Graphviz or the
#'   \code{DiagrammeR} package.
#' @export
gv_table <- function(df, df_name) {
  setup_string <- gv_setup_string(df_name)
  table_string <- df_string(list(df = df, keys = list()), df_name)
  teardown_string <- "}\n"
  paste(
    setup_string,
    "",
    table_string,
    teardown_string,
    sep = "\n"
  )
}

gv_setup_string <- function(df_name) {
  paste0(
    "digraph ",
    if (!is.na(df_name))
      paste0(snakecase::to_snake_case(df_name), " "),
    "{\n",
    "  rankdir = \"LR\"\n",
    "  node [shape=plaintext];"
  )
}

df_string <- function(dataframe, df_name) {
  df <- dataframe$df
  keys <- dataframe$keys
  df_snake <- snakecase::to_snake_case(df_name)
  col_names <- colnames(df)
  col_snake <- snakecase::to_snake_case(col_names)
  column_typing_info <- vapply(
    seq_along(col_names),
    \(n) {
      col_class <- class(df[[n]])[[1]]
      key_memberships <- vapply(keys, is.element, el = col_names[n], logical(1))
      paste0(
        "    <TR><TD PORT=\"TO_",
        col_snake[n],
        "\">",
        col_names[n],
        "</TD>",
        paste(
          vapply(
            key_memberships,
            \(m) if (m) "<TD BGCOLOR=\"black\"></TD>" else "<TD></TD>",
            character(1)
          ),
          collapse = ""
        ),
        "<TD PORT=\"", paste0("FROM_", col_snake[n]), "\">", col_class, "</TD>",
        "</TR>"
      )
    },
    character(1)
  )
  columns_string <- paste(column_typing_info, collapse = "\n")

  nrows <- nrow(df)
  label <- paste0(
    "    <TR><TD COLSPAN=\"", length(keys) + 2, "\">",
    df_name,
    " (",
    nrows,
    " row",
    if (nrows != 1) "s",
    ")",
    "</TD></TR>",
    "\n",
    columns_string
  )
  paste0(
    "  ",
    df_snake,
    " ",
    "[label = <",
    "\n",
    "    ",
    "<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
    "\n",
    label,
    "\n    </TABLE>>];"
  )
}

nameless_relation_string <- function(id, attrs, keys) {
  col_names <- attrs
  col_snake <- snakecase::to_snake_case(col_names)
  column_typing_info <- vapply(
    seq_along(col_names),
    \(n) {
      key_memberships <- vapply(
        keys,
        is.element,
        el = col_names[n],
        logical(1)
      )
      key_table <- paste(
        vapply(
          seq_along(key_memberships),
          \(m) {
            preamble <- if (m == length(key_memberships))
              paste0("<TD PORT =\"FROM_", col_snake[n], "\"")
            else
              "<TD"
            paste0(
              preamble,
              if (key_memberships[m])
                " BGCOLOR=\"black\"></TD>"
              else
                "></TD>"
            )
          },
          character(1)
        ),
        collapse = ""
      )
      paste0(
        "    <TR><TD PORT=\"TO_",
        col_snake[n],
        "\">",
        col_names[n],
        "</TD>",
        key_table,
        "</TR>"
      )
    },
    character(1)
  )
  columns_string <- paste(column_typing_info, collapse = "\n")

  paste0(
    "  ",
    id,
    " ",
    "[label = <",
    "\n",
    "    ",
    "<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
    "\n",
    columns_string,
    "\n    </TABLE>>];"
  )
}

reference_string <- function(reference) {
  paste0(
    "  ",
    paste(
      snakecase::to_snake_case(reference[1]),
      paste0("FROM_", snakecase::to_snake_case(reference[2])),
      sep = ":"
    ),
    " -> ",
    paste(
      snakecase::to_snake_case(reference[3]),
      paste0("TO_", snakecase::to_snake_case(reference[4])),
      sep = ":"
    ),
    ";"
  )
}

dbs_reference_string <- function(reference) {
  paste0(
    "  ",
    paste(
      snakecase::to_snake_case(as.character(reference[[1]][1])),
      paste0("FROM_", snakecase::to_snake_case(reference[[2]])),
      sep = ":"
    ),
    " -> ",
    paste(
      snakecase::to_snake_case(as.character(reference[[1]][2])),
      paste0("TO_", snakecase::to_snake_case(reference[[2]])),
      sep = ":"
    ),
    ";"
  )
}
