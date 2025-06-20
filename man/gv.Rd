% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.r
\name{gv}
\alias{gv}
\title{Generate Graphviz input text to plot objects}
\usage{
gv(x, name = NA_character_, ...)
}
\arguments{
\item{x}{an object to be plotted.}

\item{name}{a scalar character, giving the name of the object, if any. This
name is used for the resulting graph, to allow for easier combining of
graphs into a single diagram if required.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
A scalar character, containing text input for Graphviz.
}
\description{
Produces text input for Graphviz to make an HTML diagram of a given object.
}
\details{
Details of what is plotted are given in individual methods. There are
expected commonalities, which are described below.

The object is expected to be one of the following:
\itemize{
\item an object whose elements have the same length. Examples would be
data frames, matrices, and other objects that can represent relations, with
names for the elements, and an optional name for the object itself.
\item a graph of sub-objects, each of which represent a relation as
described above, possibly with connections between the objects, and an
optional name for the graph as a whole.
}

Each relation is presented as a record-like shape, with the following elements:
\itemize{
\item A optional header with the relation's name, and the number of (unique)
records.
\item A set of rows, one for each attribute in the relation. These rows
have the following contents:
\itemize{
\item the attribute names.
\item a depiction of the relation's (candidate) keys. Each
column represents a key, and a filled cell indicates that the attribute
in that row is in that key. The keys are given in lexical order, with
precedence given to keys with fewer attributes, and keys with attributes
that appear earlier in the original data frame's attribute order. Default
output from other package functions will thus have the primary key given
first. In the future, this will be changed to always give the primary key
first.
\item optionally, the attribute types: specifically, the first element
when passing the attribute's values into \code{\link{class}}.
}
}

Any foreign key references between relations are represented by one-way arrows,
one per attribute in the foreign key.

If the object has a name, this name is attached to the resulting graph in
Graphviz. This is to allow easier combination of several such graphs into a
single image, if a user wishes to do so.
}
\examples{
# simple data.frame example
txt_df <- gv(ChickWeight, "chick")
cat(txt_df)
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  DiagrammeR::grViz(txt_df)
}
# simple database example
db <- autodb(ChickWeight)
txt_db <- gv(db)
cat(txt_db)
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  DiagrammeR::grViz(txt_db)
}
# simple relation schemas
rschema <- synthesise(discover(ChickWeight))
txt_rschema <- gv(rschema)
cat(txt_rschema)
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  DiagrammeR::grViz(txt_rschema)
}
# simple database schema
dschema <- normalise(discover(ChickWeight))
txt_dschema <- gv(dschema)
cat(txt_dschema)
DiagrammeR::grViz(txt_dschema)
# simple relations
rel <- create(synthesise(discover(ChickWeight)))
txt_rel <- gv(rel)
cat(txt_rel)
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  DiagrammeR::grViz(txt_rel)
}
}
\seealso{
\code{\link{gv.data.frame}}, \code{\link{gv.relation_schema}},
\code{\link{gv.database_schema}}, \code{\link{gv.relation}}, and
\code{\link{gv.database}} for individual methods.
}
