#' Plot dataframes with relationships
#'
#' Produces an HTML diagram of a database, via Graphviz, or outputs the text
#' input for Graphviz.
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
#'     when passing the attribute's values into \code{\link{base::class}}.
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
#' @param text a logical, indicating whether to write the plot information to a
#'   connection, as a Graphviz input, instead of plotting.
#' @param con a connection, to which the Graphviz text input is written if
#'   \code{text} is \code{TRUE}.
#'
#' @export
plot_tables <- function(db, text = FALSE, con = stdout()) {
  gv_string <- plot_string_database(db)
  if (text)
    writeLines(gv_string, con)
  else
    DiagrammeR::grViz(gv_string)
}

#' Plot single dataframe
#'
#' @param df a data.frame, to be plotted as a record.
#' @param df_name a character scalar, giving the name of the record.
#' @inheritParams plot_tables
#'
#' @export
plot_table <- function(df, df_name, text = FALSE, con = stdout()) {
  gv_string <- plot_string_df(df, df_name)
  if (text)
    writeLines(gv_string, con)
  else
    DiagrammeR::grViz(gv_string)
}

plot_string_database <- function(db) {
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

plot_string_df <- function(df, df_name) {
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
