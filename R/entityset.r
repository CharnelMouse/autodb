EntitySet <- function(df, deps, name = NA) {
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
  depdfs <- normalize_dataframe(df, deps)
  depdfs <- make_indexes(depdfs)

  relationships <- list()

  stack <- depdfs

  while (length(stack) > 0) {
    current_df_name <- names(stack)[1]
    current <- stack[[1]]
    stack <- stack[-1]

    for (child_name in current$children) {
      child <- depdfs[[child_name]]
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
    dataframes = depdfs,
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
  gv_string <- paste0(
    "digraph ", gsub(" ", "_", es$name), " {\n",
    "  node [shape=record];\n"
  )

  df_string <- character()
  for (df_name in names(es$dataframes)) {
    df <- es$dataframes[[df_name]]$df
    column_typing_info <- vapply(
      colnames(df),
      \(col_name) {
        col_class <- class(df[[col_name]])[[1]]
        paste0("<", col_name, "> ", col_name, " : ", col_class)
      },
      character(1)
    )
    columns_string <- paste(column_typing_info, collapse = "|")

    nrows <- nrow(df)
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

  for (rel in es$relationships) {
    rel_string <- paste0(
      "  ",
      paste(rel[1], rel[2], sep = ":"),
      " -> ",
      paste(rel[3], rel[4], sep = ":"),
      ";"
    )
    gv_string <- paste(gv_string, rel_string, sep = "\n")
  }

  gv_string <- paste0(gv_string, "\n}\n")
  gv_string
}

plot_string_df <- function(df, df_name) {
  df_name <- gsub(" ", "_", df_name)

  gv_string <- paste0(
    "digraph ", df_name, " {\n",
    "  node [shape=record];\n"
  )

  df_string <- character()
  column_typing_info <- vapply(
    colnames(df),
    \(col_name) {
      col_class <- class(df[[col_name]])[[1]]
      paste0("<", col_name, "> ", col_name, " : ", col_class)
    },
    character(1)
  )
  columns_string <- paste(column_typing_info, collapse = "|")

  nrows <- nrow(df)
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
  gv_string
}
