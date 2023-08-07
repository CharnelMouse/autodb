#' Generate Graphviz input text to plot objects
#'
#' Produces text input for Graphviz to make an HTML diagram of a given object.
#'
#' Details of what is plotted are given in individual methods. There are
#' expected commonalities, which are described below.
#'
#' The object is expected to be one of the following:
#' \itemize{
#'   \item an object whose elements have the same length. Examples would be
#'   data frames, matrices, and other objects that can represent relations, with
#'   names for the elements, and an optional name for the object itself.
#'   \item a graph of sub-objects, each of which represent a relation as
#'   described above, possibly with connections between the objects, and an
#'   optional name for the graph as a whole.
#' }
#'
#' Each relation is presented as a record-like shape, with the following elements:
#' \itemize{
#'   \item A optional header with the relation's name, and the number of (unique)
#'   records.
#'   \item A set of rows, one for each attribute in the relation. These rows
#'   have the following contents:
#'   \itemize{
#'     \item the attribute names.
#'     \item a depiction of the relation's (candidate) keys. Each
#'     column represents a key, and a filled cell indicates that the attribute
#'     in that row is in that key. The keys are given in lexical order, with
#'     precedence given to keys with fewer attributes, and keys with attributes
#'     that appear earlier in the original data frame's attribute order. Default
#'     output from other package functions will thus have the primary key given
#'     first. In the future, this will be changed to always give the primary key
#'     first.
#'     \item optionally, the attribute types: specifically, the first element
#'     when passing the attribute's values into \code{\link{class}}.
#'   }
#' }
#'
#' Any foreign key references between relations are represented by one-way arrows,
#' one per attribute in the foreign key.
#'
#' If the object has a name, this name is attached to the resulting graph in
#' Graphviz. This is to allow easier combination of several such graphs into a
#' single image, if a user wishes to do so.
#'
#' @param x an object to be plotted.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A scalar character, containing text input for Graphviz or the
#'   \code{DiagrammeR} package.
#' @seealso \code{\link{gv.data.frame}}, \code{\link{gv.relation_schema}},
#'   \code{\link{gv.database_schema}}, \code{\link{gv.database}}
#' @examples
#' # simple data.frame example
#' txt_df <- gv(ChickWeight, "chick")
#' cat(txt_df)
#' DiagrammeR::grViz(txt_df)
#' # simple database example
#' db <- autodb(ChickWeight, "chick")
#' txt_db <- gv(db)
#' cat(txt_db)
#' DiagrammeR::grViz(txt_db)
#' # simple relation schemas
#' rschema <- synthesise(discover(ChickWeight, 1))
#' txt_rschema <- gv(rschema)
#' cat(txt_rschema)
#' DiagrammeR::grViz(txt_rschema)
#' # simple database schema
#' dschema <- normalise(discover(ChickWeight, 1))
#' txt_dschema <- gv(dschema)
#' cat(txt_dschema)
#' DiagrammeR::grViz(txt_dschema)
#' @export
gv <- function(x, ...) {
  UseMethod("gv", x)
}

#' Generate Graphviz input text to plot databases
#'
#' Produces text input for Graphviz to make an HTML diagram of a given database.
#'
#' Each relation in the database is presented as a set of rows, one for each
#' attribute in the relation. These rows include information about the attribute
#' classes.
#'
#' @param x a database, as returned by \code{\link{cross_reference}} or
#'   \code{\link{autodb}}.
#' @inheritParams gv
#'
#' @return A scalar character, containing text input for Graphviz or the
#'   \code{DiagrammeR} package.
#' @seealso The generic \code{\link{gv}}.
#' @exportS3Method
gv.database <- function(x, ...) {
  empty_names <- which(names(x$relations) == "" | names(x$relations) == "empty")
  names(x$relations)[empty_names] <- make.names(
    rep("empty", length(empty_names)),
    unique = TRUE
  )
  setup_string <- gv_setup_string(x$name)
  df_strings <- mapply(
    relation_string,
    x$relations,
    names(x$relations),
    "record"
  ) |>
    paste(collapse = "\n")
  reference_strings <- vapply(
    x$relationships,
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


#' Generate Graphviz input text to plot database schemas
#'
#' Produces text input for Graphviz to make an HTML diagram of a given database
#' schema.
#'
#' Each relation in the schema is presented as a set of rows, one for each
#' attribute in the relation. These rows do not include information about the
#' attribute classes.
#'
#' Any foreign key references are represented by arrows
#' between the attribute pairs.
#'
#' @param x a database schema, as given by \code{\link{normalise}},
#'   \code{\link{synthesise}}, or \code{\link{cross_reference}}.
#' @param name a character scalar, giving the name of the schema, if any.
#' @inheritParams gv
#'
#' @return A scalar character, containing text input for Graphviz or the
#'   \code{DiagrammeR} package.
#' @seealso The generic \code{\link{gv}}.
#' @exportS3Method
gv.database_schema <- function(x, name = NA_character_, ...) {
  empty_names <- which(names(x) == "" | names(x) == "empty")
  names(x)[empty_names] <- make.names(
    rep("empty", length(empty_names)),
    unique = TRUE
  )
  setup_string <- gv_setup_string(name)
  df_strings <- mapply(
    relation_schema_string,
    attrs(x),
    keys(x),
    names(x)
  ) |>
    paste(collapse = "\n")
  reference_strings <- vapply(
    relationships(x),
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

#' Generate Graphviz input text to plot relation schemas
#'
#' Produces text input for Graphviz to make an HTML diagram of a given relation
#' schema.
#'
#' Each relation in the schema is presented as a set of rows, one for each
#' attribute in the relation. These rows do not include information about the
#' attribute classes.
#'
#' @param x a relation schema, as given by \code{\link{relation_schema}} or
#'   \code{\link{synthesise}}.
#' @param name a character scalar, giving the name of the schema, if any.
#' @inheritParams gv
#'
#' @return A scalar character, containing text input for Graphviz or the
#'   \code{DiagrammeR} package.
#' @seealso The generic \code{\link{gv}}.
#' @exportS3Method
gv.relation_schema <- function(x, name = NA_character_, ...) {
  empty_names <- which(names(x) == "" | names(x) == "empty")
  names(x)[empty_names] <- make.names(
    rep("empty", length(empty_names)),
    unique = TRUE
  )
  setup_string <- gv_setup_string(name)
  df_strings <- mapply(
    relation_schema_string,
    attrs(x),
    keys(x),
    names(x)
  ) |>
    paste(collapse = "\n")
  teardown_string <- "}\n"
  paste(
    setup_string,
    "",
    df_strings,
    teardown_string,
    sep = "\n"
  )
}

#' Generate Graphviz input text to plot a data frame
#'
#' Produces text input for Graphviz to make an HTML diagram of a given data
#' frame.
#'
#' The rows in the plotted data frame include information about the attribute
#' classes.
#'
#' @param x a data.frame.
#' @param name a character scalar, giving the name of the record, if any. The
#'   name must be non-empty.
#' @inheritParams gv
#'
#' @return A scalar character, containing text input for Graphviz or the
#'   \code{DiagrammeR} package.
#' @seealso The generic \code{\link{gv}}.
#' @exportS3Method
gv.data.frame <- function(x, name, ...) {
  if (name == "")
    stop("name must be non-empty")
  setup_string <- gv_setup_string(name)
  table_string <- relation_string(list(df = x, keys = list()), name, "row")
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

relation_string <- function(dataframe, df_name, row_name = c("record", "row")) {
  row_name <- match.arg(row_name)
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
    with_number(nrows, row_name, "", "s"),
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

relation_schema_string <- function(attrs, keys, relation_name) {
  col_names <- attrs
  col_snake <- snakecase::to_snake_case(col_names)
  rel_snake <- snakecase::to_snake_case(relation_name)
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
  label <- paste0(
    "    <TR><TD COLSPAN=\"", length(keys) + 1, "\">",
    relation_name,
    "</TD></TR>",
    "\n",
    columns_string
  )

  paste0(
    "  ",
    rel_snake,
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

dbs_reference_string <- function(reference) {
  paste0(
    "  ",
    paste(
      snakecase::to_snake_case(reference[[1]][1]),
      paste0("FROM_", snakecase::to_snake_case(reference[[2]][[1]])),
      sep = ":"
    ),
    " -> ",
    paste(
      snakecase::to_snake_case(reference[[1]][2]),
      paste0("TO_", snakecase::to_snake_case(reference[[2]][[2]])),
      sep = ":"
    ),
    ";"
  )
}
