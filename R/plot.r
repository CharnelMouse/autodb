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
#' @param name a scalar character, giving the name of the object, if any. This
#'   name is used for the resulting graph, to allow for easier combining of
#'   graphs into a single diagram if required.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A scalar character, containing text input for Graphviz.
#' @seealso \code{\link{gv.data.frame}}, \code{\link{gv.relation_schema}},
#'   \code{\link{gv.database_schema}}, \code{\link{gv.relation}}, and
#'   \code{\link{gv.database}} for individual methods.
#' @examples
#' # simple data.frame example
#' txt_df <- gv(ChickWeight, "chick")
#' cat(txt_df)
#' if (requireNamespace("DiagrammeR", quietly = TRUE)) {
#'   DiagrammeR::grViz(txt_df)
#' }
#' # simple database example
#' db <- autodb(ChickWeight)
#' txt_db <- gv(db)
#' cat(txt_db)
#' if (requireNamespace("DiagrammeR", quietly = TRUE)) {
#'   DiagrammeR::grViz(txt_db)
#' }
#' # simple relation schemas
#' rschema <- synthesise(discover(ChickWeight))
#' txt_rschema <- gv(rschema)
#' cat(txt_rschema)
#' if (requireNamespace("DiagrammeR", quietly = TRUE)) {
#'   DiagrammeR::grViz(txt_rschema)
#' }
#' # simple database schema
#' dschema <- normalise(discover(ChickWeight))
#' txt_dschema <- gv(dschema)
#' cat(txt_dschema)
#' DiagrammeR::grViz(txt_dschema)
#' # simple relations
#' rel <- create(synthesise(discover(ChickWeight)))
#' txt_rel <- gv(rel)
#' cat(txt_rel)
#' if (requireNamespace("DiagrammeR", quietly = TRUE)) {
#'   DiagrammeR::grViz(txt_rel)
#' }
#' @export
gv <- function(x, name = NA_character_, ...) {
  UseMethod("gv", x)
}

#' Generate D2 input text to plot objects
#'
#' Produces text input for D2 to make a diagram of a given object, usually
#' rendered with SVG.
#'
#' The D2 language is in an early stage of development (pre-v1.0), so it may be
#' subject to changes that make it unable to use output from the current version
#' of \code{d2}.
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
#' If the object has a name, then currently the name is not used, except as a
#' single data frame's name. In the future, this will be used to give a name to
#' the generated board, to make use of D2's composition features.
#'
#' @param x an object to be plotted.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A scalar character, containing text input for D2.
#' @seealso \code{\link{d2.data.frame}} and \code{\link{d2.relation_schema}} for
#'   individual methods.
#'
#' D2 language site: \url{https://d2lang.com}
#'
#' Playground for running online without installation:
#' \url{https://play.d2lang.com/}
#'
#' Quarto extension: \url{https://github.com/data-intuitive/quarto-d2}
#' @examples
#' # simple data.frame example
#' cat(d2(ChickWeight, "chick"))
#' @export
d2 <- function(x, ...) {
  UseMethod("d2", x)
}

#' Generate Graphviz input text to plot databases
#'
#' Produces text input for Graphviz to make an HTML diagram of a given database.
#'
#' Each relation in the database is presented as a set of rows, one for each
#' attribute in the relation. These rows include information about the attribute
#' classes.
#'
#' @param x a database, as returned by \code{\link{autoref}} or
#'   \code{\link{autodb}}.
#' @param name a scalar character, giving the name of the database, if any. This
#'   name is used for the resulting graph, to allow for easier combining of
#'   graphs into a single diagram if required.
#' @inheritParams gv
#'
#' @return A scalar character, containing text input for Graphviz.
#' @seealso The generic \code{\link{gv}}.
#' @exportS3Method
gv.database <- function(x, name = NA_character_, ...) {
  if (any(names(x) == ""))
    stop("relation names can not be zero characters in length")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")
  x_labelled <- to_labelled(x)
  x_elemented <- to_elemented(x)
  setup_string <- setup_string_gv(name)
  df_strings <- Map(
    relation_string_gv,
    attrs = attrs(x_elemented),
    attr_labels = attrs(x_labelled),
    keys = keys(x_elemented),
    name = names(x_elemented),
    label = names(x_labelled),
    classes = lapply(
      records(x_elemented),
      \(df) vapply(df, \(a) class(a)[[1]], character(1))
    ),
    nrow = lapply(records(x_elemented), nrow),
    row_name = "record"
  ) |>
    Reduce(f = c, init = character())
  reference_strings <- reference_strings(x_labelled)
  teardown_string <- "}\n"
  paste(
    c(
      setup_string,
      if (length(df_strings > 0))
        c("", df_strings),
      if (length(reference_strings) > 0)
        c("", reference_strings),
      teardown_string
    ),
    collapse = "\n"
  )
}

#' Generate Graphviz input text to plot relations
#'
#' Produces text input for Graphviz to make an HTML diagram of a given relation.
#'
#' Each relation is presented as a set of rows, one for each
#' attribute in the relation. These rows include information about the
#' attribute classes.
#'
#' @param x a \code{\link{relation}}.
#' @param name a character scalar, giving the name of the schema, if any.
#' @inheritParams gv
#'
#' @return A scalar character, containing text input for Graphviz.
#' @seealso The generic \code{\link{gv}}.
#' @exportS3Method
gv.relation <- function(x, name = NA_character_, ...) {
  if (any(names(x) == ""))
    stop("relation names can not be zero characters in length")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")
  x_labelled <- to_labelled(x)
  x_elemented <- to_elemented(x)
  setup_string <- setup_string_gv(name)
  df_strings <- Map(
    relation_string_gv,
    attrs = attrs(x_elemented),
    attr_labels = attrs(x_labelled),
    keys = keys(x_elemented),
    name = names(x_elemented),
    label = names(x_labelled),
    classes = lapply(
      records(x_elemented),
      \(df) vapply(df, \(a) class(a)[[1]], character(1))
    ),
    nrow = lapply(records(x_elemented), nrow)
  ) |>
    Reduce(f = c, init = character())
  teardown_string <- "}\n"
  paste(
    c(
      setup_string,
      if (length(df_strings > 0))
        c("", df_strings),
      teardown_string
    ),
    collapse = "\n"
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
#'   \code{\link{synthesise}}, or \code{\link{autoref}}.
#' @param name a character scalar, giving the name of the schema, if any.
#' @inheritParams gv
#'
#' @return A scalar character, containing text input for Graphviz.
#' @seealso The generic \code{\link{gv}}.
#' @exportS3Method
gv.database_schema <- function(x, name = NA_character_, ...) {
  if (any(names(x) == ""))
    stop("relation schema names can not be zero characters in length")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")
  x_labelled <- to_labelled(x)
  x_elemented <- to_elemented(x)
  setup_string <- setup_string_gv(name)
  df_strings <- Map(
    relation_schema_string,
    attrs = attrs(x_elemented),
    attr_labels = attrs(x_labelled),
    keys = keys(x_elemented),
    name = names(x_elemented),
    label = names(x_labelled)
  ) |>
    Reduce(f = c, init = character())
  reference_strings <- reference_strings(x_labelled)
  teardown_string <- "}\n"
  paste(
    c(
      setup_string,
      if (length(df_strings > 0))
        c("", df_strings),
      if (length(reference_strings) > 0)
        c("", reference_strings),
      teardown_string
    ),
    collapse = "\n"
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
#' @return A scalar character, containing text input for Graphviz.
#' @seealso The generic \code{\link{gv}}.
#' @exportS3Method
gv.relation_schema <- function(x, name = NA_character_, ...) {
  if (any(names(x) == ""))
    stop("relation schema names can not be zero characters in length")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")
  x_labelled <- to_labelled(x)
  x_elemented <- to_elemented(x)
  setup_string <- setup_string_gv(name)
  df_strings <- Map(
    relation_schema_string,
    attrs = attrs(x_elemented),
    attr_labels = attrs(x_labelled),
    keys = keys(x_elemented),
    name = names(x_elemented),
    label = names(x_labelled)
  )
  teardown_string <- "}\n"
  paste(
    c(
      setup_string,
      if (length(df_strings) > 0)
        c("", df_strings),
      teardown_string
    ),
    collapse = "\n"
  )
}

#' Generate D2 input text to plot relation schemas
#'
#' Produces text input for D2 to make a diagram of a given relation schema,
#' usually rendered with SVG.
#'
#' Each relation in the schema is presented as a set of rows, one for each
#' attribute in the relation. These rows do not include information about the
#' attribute classes.
#'
#' @param x a relation schema, as given by \code{\link{relation_schema}} or
#'   \code{\link{synthesise}}.
#' @param name a character scalar, giving the name of the schema, if any.
#' @inheritParams d2
#'
#' @return A scalar character, containing text input for D2.
#' @seealso The generic \code{\link{d2}}.
#' @exportS3Method
d2.relation_schema <- function(x, name = NA_character_, ...) {
  if (any(names(x) == ""))
    stop("relation schema names can not be zero characters in length")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")
  x_labelled <- x
  x_elemented <- x
  df_strings <- Map(
    relation_schema_string_d2,
    attrs = attrs(x_elemented),
    attr_labels = attrs(x_labelled),
    keys = keys(x_elemented),
    name = names(x_elemented),
    label = names(x_labelled)
  ) |>
    Reduce(f = c, init = character())
  teardown_string <- ""
  full_text <- if (is.na(name))
    c(df_strings, teardown_string)
  else
    c(
      paste0("\"", name, "\" {"),
      paste0("  ", df_strings, recycle0 = TRUE),
      "}",
      teardown_string
    )
  paste(full_text, collapse = "\n")
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
#'   name must be non-empty, since it is also used to name the single table in
#'   the plot. Defaults to `NA`: if left missing, it is set to "data".
#' @inheritParams gv
#'
#' @return A scalar character, containing text input for Graphviz.
#' @seealso The generic \code{\link{gv}}.
#' @exportS3Method
gv.data.frame <- function(x, name = NA_character_, ...) {
  if (is.na(name))
    name <- "data"
  if (name == "")
    stop("name must be non-empty")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")
  setup_string <- setup_string_gv(name) |>
    paste(collapse = "\n")
  x_labelled <- x
  names(x_labelled) <- to_attr_name(names(x))
  table_string <- relation_string_gv(
    attrs = to_element_name(names(x)),
    attr_labels = colnames(x_labelled),
    keys = list(),
    name = to_element_name(name),
    label = to_node_name(name),
    classes = vapply(x, \(a) class(a)[[1]], character(1)),
    nrow = nrow(x),
    row_name = "row"
  )
  teardown_string <- "}\n"
  paste(
    c(
      setup_string,
      "",
      table_string,
      teardown_string
    ),
    collapse = "\n"
  )
}

#' Generate D2 input text to plot a data frame
#'
#' Produces text input for D2 to make a diagram of a given data frame, usually
#' rendered with SVG.
#'
#' The rows in the plotted data frame include information about the attribute
#' classes.
#'
#' @param x a data.frame.
#' @param name a character scalar, giving the name of the record, if any. The
#'   name must be non-empty, since it is also used to name the single table in
#'   the plot. Defaults to `NA`: if left missing, it is set to "data".
#' @inheritParams d2
#'
#' @return A scalar character, containing text input for Graphviz.
#' @seealso The generic \code{\link{d2}}.
#' @exportS3Method
d2.data.frame <- function(x, name = NA_character_, ...) {
  if (is.na(name))
    name <- "data"
  if (name == "")
    stop("name must be non-empty")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")

  x_labelled <- x
  names(x_labelled) <- names(x)
  table_string <- relation_string_d2(
    attrs = names(x),
    attr_labels = colnames(x_labelled),
    keys = list(),
    name = name,
    label = paste0("\"", name, "\""),
    classes = vapply(x, \(a) class(a)[[1]], character(1)),
    nrow = nrow(x),
    row_name = "row"
  )
  teardown_string <- ""
  paste(
    c(table_string, teardown_string),
    collapse = "\n"
  )
}

setup_string_gv <- function(df_name) {
  c(
    paste0(
      "digraph ",
      if (!is.na(df_name))
        paste0(to_main_name(df_name), " "),
      "{"
    ),
    "  rankdir = \"LR\"",
    "  node [shape=plaintext];"
  )
}

relation_string_gv <- function(
  attrs,
  attr_labels,
  keys,
  name,
  label,
  classes,
  nrow,
  row_name = c("record", "row")
) {
  row_name <- match.arg(row_name)

  columns_string <- columns_string_gv(
    attrs,
    attr_labels,
    keys,
    classes
  )
  columns_label <- c(
    paste0(
      "    <TR><TD COLSPAN=\"", length(keys) + 2, "\">",
      name,
      " (",
      with_number(nrow, row_name, "", "s"),
      ")",
      "</TD></TR>"
    ),
    columns_string
  )
  c(
    paste0(
      "  ",
      label,
      " ",
      "[label = <"
    ),
    paste0(
      "    ",
      "<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">"
    ),
    columns_label,
    "    </TABLE>>];"
  )
}

relation_string_d2 <- function(
  attrs,
  attr_labels,
  keys,
  name,
  label,
  classes,
  nrow,
  row_name = c("record", "row")
) {
  row_name <- match.arg(row_name)

  columns_string <- columns_string_d2(
    attrs,
    attr_labels,
    keys,
    classes
  )
  columns_label <- columns_string
  c(
    paste0(label, ": {"),
    "  shape: sql_table",
    columns_label,
    "}"
  )
}

relation_schema_string <- function(
  attrs,
  attr_labels,
  keys,
  name,
  label
) {
  columns_string <- columns_schema_string(attrs, attr_labels, keys)
  columns_label <- paste0(
    "    <TR><TD COLSPAN=\"", length(keys) + 1, "\">",
    name,
    "</TD></TR>",
    "\n",
    columns_string
  )
  paste0(
    "  ",
    label,
    " ",
    "[label = <",
    "\n",
    "    ",
    "<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">",
    "\n",
    columns_label,
    "\n    </TABLE>>];"
  )
}

relation_schema_string_d2 <- function(
  attrs,
  attr_labels,
  keys,
  name,
  label
) {
  columns_string <- columns_schema_string_d2(
    attrs,
    attr_labels,
    keys
  )
  columns_label <- columns_string
  c(
    paste0("\"", name, "\": {"),
    "  shape: sql_table",
    paste0("  ", columns_label),
    "}"
  )
}

columns_string_gv <- function(col_names, col_labels, keys, col_classes) {
  key_membership_strings <- vapply(
    col_names,
    \(nm) paste(
      vapply(
        keys,
        \(key) if (is.element(nm, key))
          "<TD BGCOLOR=\"black\"></TD>"
        else
          "<TD></TD>",
        character(1)
      ),
      collapse = ""
    ),
    character(1)
  )
  column_typing_info <- paste0(
    "    <TR><TD PORT=\"TO_",
    col_labels,
    "\">",
    col_names,
    "</TD>",
    key_membership_strings,
    "<TD PORT=\"FROM_", col_labels, "\">", col_classes, "</TD>",
    "</TR>",
    recycle0 = TRUE
  )
  column_typing_info
}

columns_string_d2 <- function(col_names, col_labels, keys, col_classes) {
  column_typing_info <- paste0(
    "  \"",
    col_names,
    "\": ",
    col_classes,
    recycle0 = TRUE
  )
  paste(column_typing_info, collapse = "\n")
}

columns_schema_string <- function(col_names, col_labels, keys) {
  key_membership_strings <- vapply(
    col_names,
    \(nm) paste(
      vapply(
        seq_along(keys),
        \(n) {
          preamble <- if (n == length(keys))
            paste0("<TD PORT=\"FROM_", col_labels[[match(nm, col_names)]], "\"")
          else
            "<TD"
          cell <- if (is.element(nm, keys[[n]]))
            " BGCOLOR=\"black\"></TD>"
          else
            "></TD>"
          paste0(preamble, cell)
        },
        character(1)
      ),
      collapse = ""
    ),
    character(1)
  )
  column_typing_info <- paste0(
    "    <TR><TD PORT=\"TO_",
    col_labels,
    "\">",
    col_names,
    "</TD>",
    key_membership_strings,
    "</TR>",
    recycle0 = TRUE
  )
  paste(column_typing_info, collapse = "\n")
}

columns_schema_string_d2 <- function(col_names, col_labels, keys) {
  key_matches <- lapply(keys, \(k) is.element(col_labels, k)) |>
    do.call(what = cbind)
  key_labels <- paste0("UNQ", seq_along(keys) - 1)
  if (length(keys) >= 1)
    key_labels[[1]] <- "PK"
  key_constraints <- apply(key_matches, 1, \(x) toString(key_labels[x]))
  key_strings <- ifelse(
    nchar(key_constraints) == 0,
    "",
    paste0(": {constraint: ", key_constraints, "}")
  )
  paste0(
    "\"",
    col_names,
    "\"",
    key_strings,
    recycle0 = TRUE
  )
}

reference_strings <- function(x) {
  lapply(
    references(x),
    reference_string
  ) |>
    do.call(what = c) |>
    unique() # can have dups if child-parent pairs are linked by multiple keys
}

reference_string <- function(reference) {
  paste0(
    "  ",
    paste(
      reference[[1]],
      paste0("FROM_", reference[[2]]),
      sep = ":"
    ),
    " -> ",
    paste(
      reference[[3]],
      paste0("TO_", reference[[4]]),
      sep = ":"
    ),
    ";"
  )
}

to_labelled <- function(x) {
  x_labelled <- rename_attrs(x, to_attr_name(attrs_order(x)))
  names(x_labelled) <- to_node_name(names(x_labelled))
  x_labelled
}

to_elemented <- function(x) {
  x_elemented <- rename_attrs(x, to_element_name(attrs_order(x)))
  names(x_elemented) <- to_element_name(names(x_elemented))
  x_elemented
}

to_main_name <- function(nm) paste0("\"", make.gv_names(nm), "\"")
to_element_name <- function(nm) make.html_names(nm)
to_node_name <- function(nm) paste0("\"", make.gv_names(nm), "\"", recycle0 = TRUE)
# attrs to lower case, because GraphViz HTML record port connections ignore case
to_attr_name <- function(nm) make.gv_names(tolower(nm))

make.gv_names_base <- function(
  string,
  transform_gv_names
) {
  string <- enc2utf8(string)
  if (length(string) == 0) {
    return(character())
  }
  string_attributes <- attributes(string)
  string <- string |>
    transform_gv_names() |>
    make.unique(sep = "")
  attributes(string) <- string_attributes
  string <- enc2utf8(string)
  string
}
make.html_names <- function(
  string
) {
  make.gv_names_base(
    string,
    \(s) s |>
      gsub(pattern = "&", replacement = "&amp;") |>
      gsub(pattern = "<", replacement = "&lt;") |>
      gsub(pattern = ">", replacement = "&gt;") |>
      gsub(pattern = "\"", replacement = "&quot;")
  )
}
make.html_attribute_names <- function(
  string
) {
  make.gv_names_base(
    string,
    \(s) gsub(pattern = "[^[:alnum:]&<>\"]", replacement = "_", s) |>
      gsub(pattern = "&", replacement = "&amp;") |>
      gsub(pattern = "<", replacement = "&lt;") |>
      gsub(pattern = ">", replacement = "&gt;") |>
      gsub(pattern = "\"", replacement = "&quot;") |>
      sub(pattern = "^_(^_)", replacement = "\\1", perl = TRUE) |>
      sub(pattern = "(^_)_$", replacement = "\\1", perl = TRUE)
  )
}
make.gv_names <- function(
  string
) {
  make.gv_names_base(
    string,
    \(s) gsub(pattern = "[^[:alnum:]]", replacement = "_", s) |>
      sub(pattern = "^_(^_)", replacement = "\\1", perl = TRUE) |>
      sub(pattern = "(^_)_$", replacement = "\\1", perl = TRUE)
  )
}
