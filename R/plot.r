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
#' @seealso \code{\link{d2.data.frame}}, \code{\link{d2.relation_schema}},
#'   \code{\link{d2.database_schema}}, \code{\link{d2.relation}}, and
#'   \code{\link{d2.database}} for individual methods.
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
#' @param nest_level an integer, giving the amount of nesting allowed when
#'   giving the class of a list column. Since lists can hold anything in R, this
#'   allows showing common element classes and lengths.
#' @inheritParams gv
#'
#' @return A scalar character, containing text input for Graphviz.
#' @seealso The generic \code{\link{gv}}.
#' @exportS3Method
gv.database <- function(x, name = NA_character_, nest_level = Inf, ...) {
  if (any(names(x) == ""))
    stop("relation names can not be zero characters in length")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")
  x_labelled <- to_labelled(x)
  x_elemented <- to_elemented(x)
  setup_string <- setup_string_gv(name)
  classes_info <- lapply(
    records(x_elemented),
    lapply,
    column_class_plot_info,
    nest_level
  )
  df_strings <- Map(
    relation_string_gv,
    attrs = attrs(x_elemented),
    attr_labels = attrs(x_labelled),
    keys = keys(x_elemented),
    name = names(x_elemented),
    label = names(x_labelled),
    classes = lapply(
      classes_info,
      vapply,
      column_class_2gv,
      character(1)
    ),
    nrow = lapply(records(x_elemented), nrow),
    row_name = "record"
  ) |>
    Reduce(f = c, init = character())
  reference_strings <- reference_strings_gv(x_labelled)
  teardown_string <- c("}", "")
  paste(
    c(
      setup_string,
      if (length(df_strings > 0))
        c("", indent(df_strings)),
      if (length(reference_strings) > 0)
        c("", indent(reference_strings)),
      teardown_string
    ),
    collapse = "\n"
  )
}

#' Generate D2 input text to plot databases
#'
#' Produces text input for D2 to make a diagram of a given database, usually
#' rendered with SVG.
#'
#' Each relation in the database is presented as a set of rows, one for each
#' attribute in the relation. These rows include information about the attribute
#' classes.
#'
#' Any foreign key references are represented by arrows between either the
#' attribute pairs or the relation pairs, depending on the value of
#' \code{reference_level}. This allows the output to be geared towards a
#' specific layout engine. Of the engines currently available for D2, Dagre can
#' not plot references between relation attributes, just the attributes
#' themselves, so using \code{reference_level = "relation"} prevents compound
#' foreign keys resulting in duplicate reference arrows. ELK and Tala
#' can plot between relation attributes, so the default \code{reference_level =
#' "attr"} works as intended.
#'
#' @param x a \code{\link{database}}.
#' @param name a character scalar, giving the name of the schema, if any.
#' @param reference_level a character scalar, indicating the format to use for
#'   foreign key references. "relation" only specifies the relations involved;
#'   "attr" also specifies the attributes involved, one pair at a time.
#' @param nest_level an integer, giving the amount of nesting allowed when
#'   giving the class of a list column. Since lists can hold anything in R, this
#'   allows showing common element classes and lengths.
#' @inheritParams d2
#'
#' @return A scalar character, containing text input for D2.
#' @seealso The generic \code{\link{d2}}.
#' @exportS3Method
d2.database <- function(
  x,
  name = NA_character_,
  reference_level = c("attr", "relation"),
  nest_level = Inf,
  ...
) {
  if (any(names(x) == ""))
    stop("relation names can not be zero characters in length")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")
  reference_level <- match.arg(reference_level)
  x_labelled <- to_quoted(x)
  x_elemented <- to_quoted(x)
  setup_string <- "direction: right"
  classes_info <- lapply(
    records(x_elemented),
    lapply,
    column_class_plot_info,
    nest_level
  )
  df_strings <- Map(
    relation_string_d2,
    attrs = attrs(x_elemented),
    attr_labels = attrs(x_labelled),
    keys = keys(x_elemented),
    name = names(x),
    label = names(x_labelled),
    classes = lapply(
      classes_info,
      vapply,
      column_class_2d2,
      character(1)
    ),
    nrow = lapply(records(x_elemented), nrow),
    references = lapply(
      names(x_labelled),
      \(label) Filter(\(ref) ref[[1]] == label, references(x_labelled))
    )
  ) |>
    Reduce(f = c, init = character())
  reference_strings <- reference_strings_d2(x_labelled, reference_level)
  teardown_string <- ""
  full_text <- if (is.na(name))
    c(
      setup_string,
      df_strings,
      if (length(reference_strings) > 0)
        c("", reference_strings),
      teardown_string
    )
  else
    c(
      setup_string,
      paste0(to_quoted_name(name), " {"),
      indent(df_strings),
      if (length(reference_strings) > 0)
        c("", indent(reference_strings)),
      "}",
      teardown_string
    )
  paste(
    full_text,
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
#' @param nest_level an integer, giving the amount of nesting allowed when
#'   giving the class of a list column. Since lists can hold anything in R, this
#'   allows showing common element classes and lengths.
#' @inheritParams gv
#'
#' @return A scalar character, containing text input for Graphviz.
#' @seealso The generic \code{\link{gv}}.
#' @exportS3Method
gv.relation <- function(x, name = NA_character_, nest_level = Inf, ...) {
  if (any(names(x) == ""))
    stop("relation names can not be zero characters in length")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")
  x_labelled <- to_labelled(x)
  x_elemented <- to_elemented(x)
  setup_string <- setup_string_gv(name)
  classes_info <- lapply(
    records(x_elemented),
    lapply,
    column_class_plot_info,
    nest_level
  )
  df_strings <- Map(
    relation_string_gv,
    attrs = attrs(x_elemented),
    attr_labels = attrs(x_labelled),
    keys = keys(x_elemented),
    name = names(x_elemented),
    label = names(x_labelled),
    classes = lapply(
      classes_info,
      vapply,
      column_class_2gv,
      character(1)
    ),
    nrow = lapply(records(x_elemented), nrow)
  ) |>
    Reduce(f = c, init = character())
  teardown_string <- c("}", "")
  paste(
    c(
      setup_string,
      if (length(df_strings > 0))
        c("", indent(df_strings)),
      teardown_string
    ),
    collapse = "\n"
  )
}

#' Generate D2 input text to plot relations
#'
#' Produces text input for D2 to make a diagram of a given relation, usually
#' rendered with SVG.
#'
#' Each relation is presented as a set of rows, one for each
#' attribute in the relation. These rows include information about the
#' attribute classes.
#'
#' @param x a \code{\link{relation}}.
#' @param name a character scalar, giving the name of the schema, if any.
#' @param nest_level an integer, giving the amount of nesting allowed when
#'   giving the class of a list column. Since lists can hold anything in R, this
#'   allows showing common element classes and lengths.
#' @inheritParams d2
#'
#' @return A scalar character, containing text input for D2.
#' @seealso The generic \code{\link{d2}}.
#' @exportS3Method
d2.relation <- function(x, name = NA_character_, nest_level = Inf, ...) {
  if (any(names(x) == ""))
    stop("relation names can not be zero characters in length")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")
  x_labelled <- to_quoted(x)
  x_elemented <- to_quoted(x)
  setup_string <- "direction: right"
  classes_info <- lapply(
    records(x_elemented),
    lapply,
    column_class_plot_info,
    nest_level
  )
  df_strings <- Map(
    relation_string_d2,
    attrs = attrs(x_elemented),
    attr_labels = attrs(x_labelled),
    keys = keys(x_elemented),
    name = names(x),
    label = names(x_labelled),
    classes = lapply(
      classes_info,
      vapply,
      column_class_2d2,
      character(1)
    ),
    nrow = lapply(records(x_elemented), nrow),
    MoreArgs = list(references = list())
  ) |>
    Reduce(f = c, init = character())
  teardown_string <- ""
  full_text <- if (is.na(name))
    c(
      setup_string,
      df_strings,
      teardown_string
    )
  else
    c(
      setup_string,
      paste0(to_quoted_name(name), " {"),
      indent(df_strings),
      "}",
      teardown_string
    )
  paste(
    full_text,
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
  reference_strings <- reference_strings_gv(x_labelled)
  teardown_string <- c("}", "")
  paste(
    c(
      setup_string,
      if (length(df_strings > 0))
        c("", indent(df_strings)),
      if (length(reference_strings) > 0)
        c("", indent(reference_strings)),
      teardown_string
    ),
    collapse = "\n"
  )
}

#' Generate D2 input text to plot database schemas
#'
#' Produces text input for D2 to make a diagram of a given database schema,
#' usually rendered with SVG.
#'
#' Each relation in the schema is presented as a set of rows, one for each
#' attribute in the relation. These rows do not include information about the
#' attribute classes.
#'
#' Any foreign key references are represented by arrows between either the
#' attribute pairs or the relation pairs, depending on the value of
#' \code{reference_level}. This allows the output to be geared towards a
#' specific layout engine. Of the engines currently available for D2, Dagre can
#' not plot references between relation attributes, just the attributes
#' themselves, so using \code{reference_level = "relation"} prevents compound
#' foreign keys resulting in duplicate reference arrows. ELK and Tala
#' can plot between relation attributes, so the default \code{reference_level =
#' "attr"} works as intended.
#'
#' @param x a database schema, as given by \code{\link{database_schema}} or
#'   \code{\link{normalise}}.
#' @param name a character scalar, giving the name of the schema, if any.
#' @param reference_level a character scalar, indicating the format to use for
#'   foreign key references. "relation" only specifies the relations involved;
#'   "attr" also specifies the attributes involved, one pair at a time.
#' @inheritParams d2
#'
#' @return A scalar character, containing text input for D2.
#' @seealso The generic \code{\link{d2}}.
#' @exportS3Method
d2.database_schema <- function(
  x,
  name = NA_character_,
  reference_level = c("attr", "relation"),
  ...
) {
  if (any(names(x) == ""))
    stop("relation schema names can not be zero characters in length")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")
  reference_level <- match.arg(reference_level)
  x_labelled <- to_quoted(x)
  x_elemented <- to_quoted(x)
  setup_string <- "direction: right"
  df_strings <- Map(
    relation_schema_string_d2,
    attrs = attrs(x_elemented),
    attr_labels = attrs(x_labelled),
    keys = keys(x_elemented),
    name = names(x_elemented),
    label = names(x_labelled),
    references = lapply(
      names(x_labelled),
      \(label) Filter(\(ref) ref[[1]] == label, references(x_labelled))
    )
  ) |>
    Reduce(f = c, init = character())
  reference_strings <- reference_strings_d2(x_labelled, reference_level)
  teardown_string <- ""
  full_text <- if (is.na(name))
    c(
      setup_string,
      df_strings,
      if (length(reference_strings) > 0)
        c("", reference_strings),
      teardown_string
    )
  else
    c(
      setup_string,
      paste0(to_quoted_name(name), " {"),
      indent(df_strings),
      if (length(reference_strings) > 0)
        c("", indent(reference_strings)),
      "}",
      teardown_string
    )
  paste(full_text, collapse = "\n")
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
  ) |>
    Reduce(f = c, init = character())
  teardown_string <- c("}", "")
  paste(
    c(
      setup_string,
      if (length(df_strings) > 0)
        c("", indent(df_strings)),
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
  x_labelled <- to_quoted(x)
  x_elemented <- to_quoted(x)
  setup_string <- "direction: right"
  df_strings <- Map(
    relation_schema_string_d2,
    attrs = attrs(x_elemented),
    attr_labels = attrs(x_labelled),
    keys = keys(x_elemented),
    name = names(x_elemented),
    label = names(x_labelled),
    MoreArgs = list(references = list())
  ) |>
    Reduce(f = c, init = character())
  teardown_string <- ""
  full_text <- if (is.na(name))
    c(setup_string, df_strings, teardown_string)
  else
    c(
      setup_string,
      paste0(to_quoted_name(name), " {"),
      indent(df_strings),
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
#' @param nest_level an integer, giving the amount of nesting allowed when
#'   giving the class of a list column. Since lists can hold anything in R, this
#'   allows showing common element classes and lengths.
#' @inheritParams gv
#'
#' @return A scalar character, containing text input for Graphviz.
#' @seealso The generic \code{\link{gv}}.
#' @exportS3Method
gv.data.frame <- function(x, name = NA_character_, nest_level = Inf, ...) {
  plot_info <- df_plot_info(x, name, nest_level)
  setup_string <- setup_string_gv(plot_info$name)
  table_string <- df_string_gv(plot_info)
  teardown_string <- c("}", "")
  paste(
    c(
      setup_string,
      "",
      indent(table_string),
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
#' @param nest_level an integer, giving the amount of nesting allowed when
#'   giving the class of a list column. Since lists can hold anything in R, this
#'   allows showing common element classes and lengths.
#' @inheritParams d2
#'
#' @return A scalar character, containing text input for Graphviz.
#' @seealso The generic \code{\link{d2}}.
#' @exportS3Method
d2.data.frame <- function(x, name = NA_character_, nest_level = Inf, ...) {
  plot_info <- df_plot_info(x, name, nest_level)
  setup_string <- "direction: right"
  table_string <- df_string_d2(plot_info)
  teardown_string <- ""
  paste(
    c(setup_string, table_string, teardown_string),
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
    indent("rankdir = \"LR\""),
    indent("node [shape=plaintext];")
  )
}

column_class_plot_info <- function(a, nest_level) {
  size_info <- column_size_plot_info(a)
  sublist_info <- column_subclass_plot_info(a, nest_level)
  c(
    list(class = class(a)[[1]]),
    if (length(size_info) > 0)
      list(length = size_info),
    if (length(sublist_info) > 0)
      list(element = sublist_info)
  )
}

column_class_2gv <- function(plot_info) {
  if (length(plot_info) == 0)
    return(character())
  paste0(
    plot_info$class,
    paste0("[", plot_info$length, "]", recycle0 = TRUE),
    paste0("&lt;", column_class_2gv(plot_info$element), "&gt;", recycle0 = TRUE)
  )
}

column_class_2d2 <- function(plot_info) {
  if (length(plot_info) == 0)
    return(character())
  paste0(
    plot_info$class,
    paste0("[", plot_info$length, "]", recycle0 = TRUE),
    paste0("<", column_class_2d2(plot_info$element), ">", recycle0 = TRUE)
  )
}

column_subclass_plot_info <- function(a, nest_level) {
  if (nest_level <= 0)
    return(list())
  UseMethod("column_subclass_plot_info")
}

#' @exportS3Method
column_subclass_plot_info.default <- function(a, nest_level) {
  list()
}

#' @exportS3Method
column_subclass_plot_info.matrix <- function(a, nest_level) {
  b <- a[TRUE, drop = TRUE]
  cl <- class(b)[[1]]
  if (cl != "list")
    return(list(class = cl))
  sublist_info <- column_subclass_plot_info(b, nest_level - 1L)
  if (length(sublist_info) == 0)
    list(class = cl)
  else
    list(class = cl, element = sublist_info)
}

#' @exportS3Method
column_subclass_plot_info.list <- function(a, nest_level) {
  if (length(a) == 0)
    return(list())
  lens <- lengths(a)
  same_length <- all(lens == lens[[1]])
  element_classes <- vapply(a, \(x) class(x)[[1]], character(1))
  same_class <- all(element_classes == element_classes[[1]])
  if (!same_length && !same_class)
    return(list())

  res <- c(
    if (same_class) list(class = element_classes[[1]]),
    if (same_length && any(element_classes != "NULL")) list(length = lens[[1]])
  )
  if (!same_class)
    return(res)
  if (element_classes[[1]] != "list" || nest_level <= 0)
    return(res)
  subelements <- unlist(a, recursive = FALSE)
  if (length(subelements) == 0)
    return(res)
  substrings <- column_subclass_plot_info(
    subelements,
    nest_level - 1L
  )
  if (length(substrings) == 0)
    return(res)
  c(res, list(element = substrings))
}

column_size_plot_info <- function(a) {
  UseMethod("column_size_plot_info")
}

#' @exportS3Method
column_size_plot_info.default <- function(a) {
  integer()
}

#' @exportS3Method
column_size_plot_info.matrix <- function(a) {
  ncol(a)
}

#' @exportS3Method
column_size_plot_info.data.frame <- function(a) {
  ncol(a)
}

df_string_gv <- function(plot_info) {
  relation_string_gv(
    attrs = to_element_name(plot_info$names),
    attr_labels = to_attr_name(plot_info$names),
    keys = list(),
    name = to_element_name(plot_info$name),
    label = to_node_name(plot_info$name),
    classes = vapply(plot_info$classes, column_class_2gv, character(1)),
    nrow = plot_info$length,
    row_name = "row"
  )
}

df_string_d2 <- function(plot_info) {
  relation_string_d2(
    attrs = to_quoted_name(plot_info$names),
    attr_labels = to_quoted_name(plot_info$names),
    keys = list(),
    name = plot_info$name,
    label = to_quoted_name(plot_info$name),
    classes = vapply(plot_info$classes, column_class_2d2, character(1)),
    nrow = plot_info$length,
    references = list(),
    row_name = "row"
  )
}

df_plot_info <- function(x, name, nest_level) {
  if (is.na(name))
    name <- "data"
  if (name == "")
    stop("name must be non-empty")
  if (!is.character(name) || length(name) != 1)
    stop("name must be a length-one character")

  classes_info <- lapply(x, column_class_plot_info, nest_level)
  list(name = name, names = names(x), length = nrow(x), classes = classes_info)
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
      "<TR><TD COLSPAN=\"", length(keys) + 2, "\">",
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
      label,
      " ",
      "[label = <"
    ),
    indent(paste0(
      "<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">"
    )),
    indent(columns_label),
    indent("</TABLE>>];")
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
  references,
  row_name = c("record", "row")
) {
  row_name <- match.arg(row_name)

  columns_string <- columns_string_d2(
    attrs,
    attr_labels,
    keys,
    classes,
    references
  )
  columns_label <- columns_string
  c(
    paste0("\"", name, "\": \"", name, " (", with_number(nrow, row_name, "", "s"), ")\" {"),
    indent("shape: sql_table"),
    indent(columns_label),
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
  columns_string <- columns_schema_string_gv(attrs, attr_labels, keys)
  columns_label <- c(
    paste0(
      "<TR><TD COLSPAN=\"", length(keys) + 1, "\">",
      name,
      "</TD></TR>"
    ),
    columns_string
  )
  table_header <-
    "<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">"
  c(
    paste0(
      label,
      " ",
      "[label = <"
    ),
    indent(table_header),
    indent(columns_label),
    indent("</TABLE>>];")
  )
}

relation_schema_string_d2 <- function(
  attrs,
  attr_labels,
  keys,
  name,
  label,
  references
) {
  columns_string <- columns_schema_string_d2(
    attrs,
    attr_labels,
    keys,
    references
  )
  columns_label <- columns_string
  c(
    paste0(name, ": {"),
    indent("shape: sql_table"),
    indent(columns_label),
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
    "<TR><TD PORT=\"TO_",
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

columns_string_d2 <- function(col_names, col_labels, keys, col_classes, references) {
  column_typing_info <- paste0(
    col_names,
    ": \"",
    col_classes,
    "\"",
    recycle0 = TRUE
  )
  key_matches <- lapply(
    col_labels,
    \(label) vapply(keys, is.element, logical(1), el = label)
  )
  key_labels <- paste0("UNQ", seq_along(keys) - 1)
  if (length(keys) >= 1)
    key_labels[[1]] <- "PK"
  key_constraints <- lapply(key_matches, \(x) key_labels[x])
  ref_constraints <- lapply(
    col_labels,
    \(cl) vapply(references, \(ref) is.element(cl, ref[[2]]), logical(1))
  ) |>
    sapply(\(x) paste0("FK", which(x), recycle0 = TRUE))
  all_constraints <- mapply(
    \(x, y) paste(c(x, y), collapse = "; "),
    key_constraints,
    ref_constraints
  )
  constraint_strings <- ifelse(
    nchar(all_constraints) == 0,
    "",
    paste0(" {constraint: [", all_constraints, "]}")
  )
  paste0(column_typing_info, constraint_strings)
}

columns_schema_string_gv <- function(col_names, col_labels, keys) {
  stopifnot(length(keys) > 0)

  key_membership <- outer(
    col_names,
    keys,
    \(cns, ks) mapply(is.element, cns, ks)
  )
  key_membership_mat <- matrix(
    "",
    nrow = length(col_names),
    ncol = length(keys)
  )
  key_membership_mat[, length(keys)] <- paste0(
    key_membership_mat[, length(keys)],
    " PORT=\"FROM_",
    col_labels,
    "\""
  )
  key_membership_mat[] <- paste0(
    key_membership_mat,
    ifelse(key_membership, " BGCOLOR=\"black\"", "")
  )
  key_membership_mat[] <- paste0(
    "<TD",
    key_membership_mat,
    "></TD>"
  )

  paste0(
    "<TR>",
    "<TD PORT=\"TO_", col_labels, "\">", col_names, "</TD>",
    apply(key_membership_mat, 1, paste, collapse = ""),
    "</TR>",
    recycle0 = TRUE
  )
}

columns_schema_string_d2 <- function(col_names, col_labels, keys, references) {
  key_matches <- lapply(keys, \(k) is.element(col_labels, k)) |>
    do.call(what = cbind)
  key_labels <- paste0("UNQ", seq_along(keys) - 1)
  if (length(keys) >= 1)
    key_labels[[1]] <- "PK"
  key_constraints <- apply(key_matches, 1, \(x) key_labels[x], simplify = FALSE)
  ref_constraints <- lapply(
    col_labels,
    \(cl) vapply(references, \(ref) is.element(cl, ref[[2]]), logical(1))
  ) |>
    sapply(\(x) paste0("FK", which(x), recycle0 = TRUE))
  all_constraints <- mapply(
    \(x, y) paste(c(x, y), collapse = "; "),
    key_constraints,
    ref_constraints
  )
  constraint_strings <- ifelse(
    nchar(all_constraints) == 0,
    "",
    paste0(": {constraint: [", all_constraints, "]}")
  )

  paste0(
    col_names,
    constraint_strings,
    recycle0 = TRUE
  )
}

reference_strings_gv <- function(x) {
  lapply(
    references(x),
    reference_string_gv
  ) |>
    do.call(what = c) |>
    unique() # can have dups if child-parent pairs are linked by multiple keys
}

reference_strings_d2 <- function(
  x,
  reference_level = c("attr", "relation")
) {
  reference_level <- match.arg(reference_level)
  lapply(
    references(x),
    reference_string_d2,
    reference_level = reference_level
  ) |>
    do.call(what = c) |>
    unique() # can have dups if child-parent pairs are linked by multiple keys
}

reference_string_gv <- function(reference) {
  paste0(
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

reference_string_d2 <- function(
  reference,
  reference_level = c("attr", "relation")
) {
  switch(
    match.arg(reference_level),
    attr = paste0(
      paste(
        reference[[1]],
        reference[[2]],
        sep = "."
      ),
      " -> ",
      paste(
        reference[[3]],
        reference[[4]],
        sep = "."
      )
    ),
    relation = paste0(
      reference[[1]],
      " -> ",
      reference[[3]]
    )
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

to_quoted <- function(x) {
  x_quoted <- rename_attrs(x, to_quoted_name(attrs_order(x)))
  names(x_quoted) <- to_quoted_name(names(x_quoted))
  x_quoted
}

to_main_name <- function(nm) to_quoted_name(make.gv_names(nm))
to_element_name <- function(nm) make.html_names(nm)
to_node_name <- function(nm) to_quoted_name(make.gv_names(nm))
# attrs to lower case, because GraphViz HTML record port connections ignore case
to_attr_name <- function(nm) make.gv_names(tolower(nm))
to_quoted_name <- function(nm, ...) paste0("\"", nm, "\"", recycle0 = TRUE)

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

indent <- function(x) paste0("  ", x, recycle0 = TRUE)
