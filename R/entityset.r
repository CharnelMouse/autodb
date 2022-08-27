#' Add foreign key references to a normalised database
#'
#' @param tables a relation set, i.e. named list of tables, as given by
#'   \code{\link{decompose}}.
#'
#' @return A "database", defined as a list of two elements:
#'   \itemize{
#'     \item \code{dataframes} contains the given relation set, \code{tables}.
#'     \item \code{relationships} contains a list of length-four character
#'     vectors, describing a single attribute pair in a foreign key
#'     relationship. In order, the elements give the name of the child table,
#'     the attribute in the child table, the parent table, and the attribute in
#'     the parent table. normalised into the required number of tables.
#'  }
#' @export
cross_reference <- function(tables) {
  relationships <- list()
  stack <- tables
  while (length(stack) > 0) {
    current_df_name <- names(stack)[1]
    current <- stack[[1]]
    child_attrs <- names(current$df)
    stack <- stack[-1]

    for (parent_name in current$parents) {
      parent <- tables[[parent_name]]
      relationships <- c(
        relationships,
        lapply(
          intersect(child_attrs, unlist(parent$keys)),
          \(ind) c(current_df_name, ind, parent_name, ind)
        )
      )
    }
  }

  es <- list(
    dataframes = tables,
    relationships = relationships
  )
  class(es) <- c("database", class(es))
  es
}

#' Plot dataframes with relationships
#'
#' @param es an entity set, to plotted as linked records.
#' @param text a logical, indicating whether to write the plot information to a
#'   connection, as a Graphviz input, instead of plotting.
#' @param con a connection, to which the Graphviz text input is written if
#'   \code{text} is \code{TRUE}.
#'
#' @export
plot_tables <- function(es, text = FALSE, con = stdout()) {
  gv_string <- plot_string_entityset(es)
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

plot_string_entityset <- function(es) {
  setup_string <- gv_setup_string(es$name)
  df_strings <- mapply(
    df_string,
    es$dataframes,
    names(es$dataframes)
  ) |>
    paste(collapse = "\n")
  reference_strings <- vapply(
    es$relationships,
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
    "digraph ", snakecase::to_snake_case(df_name), " {\n",
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
