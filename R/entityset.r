EntitySet <- function(x, name = NA, time_index = NULL) {
  UseMethod("EntitySet")
}

#' @export
EntitySet.DepDF <- function(depdf, name = NA, time_index = NULL) {
  # Creates a normalized EntitySet from df based on the dependencies given.
  # Keys for the newly created DataFrames can only be columns that are strings,
  # ints, or categories. Keys are chosen according to the priority:
  #   1) shortest lenghts 2) has "id" in some form in the name of an attribute
  # 3) has attribute furthest to left in the table
  #
  # Arguments:
  #   df (pd.DataFrame) : dataframe to normalize and make entity set from
  # dependencies (Dependenies) : the dependencies discovered in df
  # name (str, optional) : the name of created EntitySet
  #
  # Returns:
  #   entityset (ft.EntitySet) : created entity set
  depdf <- normalize(depdf)
  depdf <- make_indexes(depdf)

  dataframes <- list()
  relationships <- list()

  stack <- depdf[1]

  while (length(stack) > 0) {
    current_df_name <- names(stack)[1]
    current <- stack[[1]]
    stack <- stack[-1]

    if (!is.null(time_index) && time_index %in% colnames(current$df))
      dataframes[[current_df_name]] <- list(
        df = current$df,
        index = current$index[1],
        time_index = time_index
      )
    else
      dataframes[[current_df_name]] <- list(
        df = current$df,
        index = current$index[1]
      )
    for (child_name in current$children) {
      child <- depdf[[child_name]]
      # add to stack
      stack <- c(stack, stats::setNames(list(child), child_name))
      # add relationship
      relationships <- c(
        relationships,
        lapply(
          child$index,
          \(ind) c(current_df_name, ind, child_name, ind)
        )
      )
    }
  }

  es <- list(
    name = name,
    dataframes = dataframes,
    relationships = relationships
  )
  class(es) <- c("EntitySet", class(es))
  es
}

#' @export
EntitySet.data.frame <- function(df, df_name, name = NA, time_index = NA) {
  depdf <- DepDF(list(), df)
  if (!is.na(time_index) && time_index %in% colnames(depdf$df))
    dataframes <- list(list(
      df = depdf$df,
      index = depdf$index[1],
      time_index = time_index
    ))
  else
    dataframes <- list(list(
      df = depdf$df,
      index = depdf$index[1]
    ))
  es <- list(
    name = name,
    dataframes = stats::setNames(dataframes, df_name),
    relationships = list()
  )
  class(es) <- c("EntitySet", class(es))
  es
}

#' Plot dataframes with relationships
#'
#' @param x either a data.frame, or an entity set.
#' @param ... additional arguments required for the given method.
#' @param to_file a logical, indicating whether to write the plot information to
#'   a file, as a Graphviz input, instead of plotting. Currently not
#'   implemented.
#'
#' @export
plot_tables <- function(x, ..., to_file = FALSE) {
  UseMethod("plot_tables")
}

#' @export
plot_tables.EntitySet <- function(x, ..., to_file = FALSE) {
  # Create a UML diagram-ish graph of the EntitySet.
  # Args:
  #     to_file (str, optional) : Path to where the plot should be saved.
  #         If set to None (as by default), the plot will not be saved.
  # Returns:
  #     graphviz.Digraph : Graph object that can directly be displayed in
  #         Jupyter notebooks. Nodes of the graph correspond to the DataFrames
  #         in the EntitySet, showing the typing information for each column.
  # Note:
  #     The typing information displayed for each column is based off of the Woodwork
  #     ColumnSchema for that column and is represented as ``LogicalType; semantic_tags``,
  #     but the standard semantic tags have been removed for brevity.

  # Initialize a new directed graph
  gv_string <- paste0(
    "digraph ", gsub(" ", "_", x$name), " {\n",
    "  node [shape=record];\n"
  )

  # Draw dataframes
  df_string <- character()
  for (df_name in names(x$dataframes)) {
    df <- x$dataframes[[df_name]]
    column_typing_info <- vapply(
      colnames(df$df),
      \(col_name) {
        col_class <- class(df$df[[col_name]])[[1]]
        paste0("<", col_name, "> ", col_name, " : ", col_class)
      },
      character(1)
    )
    columns_string <- paste(column_typing_info, collapse = "|")

    nrows <- nrow(df$df)
    label <- paste0(
      "{",
      df_name,
      " (",
      nrows,
      " row",
      if (nrows != 1) "s",
      ")|",
      columns_string,
      "}"
    )
    df_string <- paste0("  ", df_name, " [label = \"", label, "\"];\n")
    gv_string <- paste0(gv_string, df_string)
  }

  # Draw relationships
  for (rel in x$relationships) {
    rel_string <- paste0(
      "  ",
      paste(rel[1], rel[2], sep = ":"),
      " -> ",
      paste(rel[3], rel[4], sep = ":"),
      ";\n"
    )
    gv_string <- paste(gv_string, rel_string, sep = "\n")
  }
  gv_string <- paste0(gv_string, "}\n")

  # if (to_file)
  #   save_graph(graph, to_file, format_)
  DiagrammeR::grViz(gv_string)
}

#' @export
plot_tables.data.frame <- function(x, df_name, ..., to_file = FALSE) {

  df_name <- gsub(" ", "_", df_name)

  # Initialize a new directed graph
  gv_string <- paste0(
    "digraph ", df_name, " {",
    "  node [shape=record];\n"
  )

  # Draw dataframes
  df_string <- character()
  column_typing_info <- vapply(
    colnames(x),
    \(col_name) {
      col_class <- class(x[[col_name]])[[1]]
      paste0("<", col_name, "> ", col_name, " : ", col_class)
    },
    character(1)
  )
  columns_string <- paste(column_typing_info, collapse = "|")

  nrows <- nrow(x)
  label <- paste0(
    "{",
    df_name,
    " (",
    nrows,
    " row",
    if (nrows != 1) "s",
    ")|",
    columns_string,
    "}"
  )
  df_string <- paste0("  ", df_name, " [label = \"", label, "\"];\n")
  gv_string <- paste0(gv_string, df_string)

  gv_string <- paste0(gv_string, "}\n")

  # if (to_file)
  #   save_graph(graph, to_file, format_)
  DiagrammeR::grViz(gv_string)
}
