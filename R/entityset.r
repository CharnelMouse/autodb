EntitySet <- function(tables, norm_deps, name = NA_character_) {
  # Creates a normalised EntitySet from df based on the dependencies given.
  # Keys for the newly created DataFrames can only be columns that are strings,
  # ints, or categories. Keys are chosen according to the priority:
  #   1) shortest lenghts 2) has "id" in some form in the name of an attribute
  # 3) has attribute furthest to left in the table
  #
  # Arguments:
  #   df (pd.DataFrame) : dataframe to normalise and make entity set from
  # dependencies (Dependenies) : the dependencies discovered in df
  # name (str, optional) : the name of created EntitySet
  #
  # Returns:
  #   entityset (ft.EntitySet) : created entity set
  tables <- make_indexes(tables)

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
    name = name,
    dataframes = tables,
    relationships = relationships
  )
  class(es) <- c("EntitySet", class(es))
  es
}

#' Plot dataframes with relationships
#'
#' @param es an entity set, to plotted as linked records.
#' @param to_file a logical, indicating whether to write the plot information to
#'   a file, as a Graphviz input, instead of plotting. Currently not
#'   implemented.
#'
#' @export
plot_tables <- function(es, to_file = FALSE) {
  gv_string <- plot_string_entityset(es)
  DiagrammeR::grViz(gv_string)
}

#' Plot single dataframe
#'
#' @param df a data.frame, to be plotted as a record.
#' @param df_name a character scalar, giving the name of the record.
#' @param to_file a logical, indicating whether to write the plot information to
#'   a file, as a Graphviz input, instead of plotting. Currently not
#'   implemented.
#'
#' @export
plot_table <- function(df, df_name, to_file = FALSE) {
  gv_string <- plot_string_df(df, df_name)
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
      characteristics <- c(
        col_class,
        if (col_names[n] %in% unlist(keys))
          "prime"
      )
      paste0(
        "    <TR><TD PORT=\"",
        col_snake[n],
        "\">",
        col_names[n],
        " : ",
        toString(characteristics),
        "</TD></TR>"
      )
    },
    character(1)
  )
  columns_string <- paste(column_typing_info, collapse = "\n")

  nrows <- nrow(df)
  label <- paste0(
    "    <TR><TD>",
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
      snakecase::to_snake_case(reference[2]),
      sep = ":"
    ),
    " -> ",
    paste(
      snakecase::to_snake_case(reference[3]),
      snakecase::to_snake_case(reference[4]),
      sep = ":"
    ),
    ";"
  )
}
