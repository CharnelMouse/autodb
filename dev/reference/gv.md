# Generate Graphviz input text to plot objects

Produces text input for Graphviz to make an HTML diagram of a given
object.

## Usage

``` r
gv(x, name = NA_character_, ...)
```

## Arguments

- x:

  an object to be plotted.

- name:

  a scalar character, giving the name of the object, if any. This name
  is used for the resulting graph, to allow for easier combining of
  graphs into a single diagram if required.

- ...:

  further arguments passed to or from other methods.

## Value

A scalar character, containing text input for Graphviz.

## Details

Details of what is plotted are given in individual methods. There are
expected commonalities, which are described below.

The object is expected to be one of the following:

- an object whose elements have the same length. Examples would be data
  frames, matrices, and other objects that can represent relations, with
  names for the elements, and an optional name for the object itself.

- a graph of sub-objects, each of which represent a relation as
  described above, possibly with connections between the objects, and an
  optional name for the graph as a whole.

Each relation is presented as a record-like shape, with the following
elements:

- A optional header with the relation's name, and the number of (unique)
  records.

- A set of rows, one for each attribute in the relation. These rows have
  the following contents:

  - the attribute names.

  - a depiction of the relation's (candidate) keys. Each column
    represents a key, and a filled cell indicates that the attribute in
    that row is in that key. The keys are given in lexical order, with
    precedence given to keys with fewer attributes, and keys with
    attributes that appear earlier in the original data frame's
    attribute order. Default output from other package functions will
    thus have the primary key given first. In the future, this will be
    changed to always give the primary key first.

  - optionally, the attribute types: specifically, the first element
    when passing the attribute's values into
    [`class`](https://rdrr.io/r/base/class.html).

Any foreign key references between relations are represented by one-way
arrows, one per attribute in the foreign key.

If the object has a name, this name is attached to the resulting graph
in Graphviz. This is to allow easier combination of several such graphs
into a single image, if a user wishes to do so.

## See also

[`gv.data.frame`](https://charnelmouse.github.io/autodb/dev/reference/gv.data.frame.md),
[`gv.relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/gv.relation_schema.md),
[`gv.database_schema`](https://charnelmouse.github.io/autodb/dev/reference/gv.database_schema.md),
[`gv.relation`](https://charnelmouse.github.io/autodb/dev/reference/gv.relation.md),
and
[`gv.database`](https://charnelmouse.github.io/autodb/dev/reference/gv.database.md)
for individual methods.

## Examples

``` r
# simple data.frame example
txt_df <- gv(ChickWeight, "chick")
cat(txt_df)
#> digraph "chick" {
#>   rankdir = "LR"
#>   node [shape=plaintext];
#> 
#>   "chick" [label = <
#>     <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
#>     <TR><TD COLSPAN="2">chick (578 rows)</TD></TR>
#>     <TR><TD PORT="TO_weight">weight</TD><TD PORT="FROM_weight">numeric</TD></TR>
#>     <TR><TD PORT="TO_time">Time</TD><TD PORT="FROM_time">numeric</TD></TR>
#>     <TR><TD PORT="TO_chick">Chick</TD><TD PORT="FROM_chick">ordered</TD></TR>
#>     <TR><TD PORT="TO_diet">Diet</TD><TD PORT="FROM_diet">factor</TD></TR>
#>     </TABLE>>];
#> }
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  DiagrammeR::grViz(txt_df)
}

{"x":{"diagram":"digraph \"chick\" {\n  rankdir = \"LR\"\n  node [shape=plaintext];\n\n  \"chick\" [label = <\n    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n    <TR><TD COLSPAN=\"2\">chick (578 rows)<\/TD><\/TR>\n    <TR><TD PORT=\"TO_weight\">weight<\/TD><TD PORT=\"FROM_weight\">numeric<\/TD><\/TR>\n    <TR><TD PORT=\"TO_time\">Time<\/TD><TD PORT=\"FROM_time\">numeric<\/TD><\/TR>\n    <TR><TD PORT=\"TO_chick\">Chick<\/TD><TD PORT=\"FROM_chick\">ordered<\/TD><\/TR>\n    <TR><TD PORT=\"TO_diet\">Diet<\/TD><TD PORT=\"FROM_diet\">factor<\/TD><\/TR>\n    <\/TABLE>>];\n}\n","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}# simple database example
db <- autodb(ChickWeight)
txt_db <- gv(db)
cat(txt_db)
#> digraph {
#>   rankdir = "LR"
#>   node [shape=plaintext];
#> 
#>   "Chick" [label = <
#>     <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
#>     <TR><TD COLSPAN="3">Chick (50 records)</TD></TR>
#>     <TR><TD PORT="TO_chick">Chick</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_chick">ordered</TD></TR>
#>     <TR><TD PORT="TO_diet">Diet</TD><TD></TD><TD PORT="FROM_diet">factor</TD></TR>
#>     </TABLE>>];
#>   "Time_Chick" [label = <
#>     <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
#>     <TR><TD COLSPAN="3">Time_Chick (578 records)</TD></TR>
#>     <TR><TD PORT="TO_time">Time</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_time">numeric</TD></TR>
#>     <TR><TD PORT="TO_chick">Chick</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_chick">ordered</TD></TR>
#>     <TR><TD PORT="TO_weight">weight</TD><TD></TD><TD PORT="FROM_weight">numeric</TD></TR>
#>     </TABLE>>];
#> 
#>   "Time_Chick":FROM_chick -> "Chick":TO_chick;
#> }
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  DiagrammeR::grViz(txt_db)
}

{"x":{"diagram":"digraph {\n  rankdir = \"LR\"\n  node [shape=plaintext];\n\n  \"Chick\" [label = <\n    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n    <TR><TD COLSPAN=\"3\">Chick (50 records)<\/TD><\/TR>\n    <TR><TD PORT=\"TO_chick\">Chick<\/TD><TD BGCOLOR=\"black\"><\/TD><TD PORT=\"FROM_chick\">ordered<\/TD><\/TR>\n    <TR><TD PORT=\"TO_diet\">Diet<\/TD><TD><\/TD><TD PORT=\"FROM_diet\">factor<\/TD><\/TR>\n    <\/TABLE>>];\n  \"Time_Chick\" [label = <\n    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n    <TR><TD COLSPAN=\"3\">Time_Chick (578 records)<\/TD><\/TR>\n    <TR><TD PORT=\"TO_time\">Time<\/TD><TD BGCOLOR=\"black\"><\/TD><TD PORT=\"FROM_time\">numeric<\/TD><\/TR>\n    <TR><TD PORT=\"TO_chick\">Chick<\/TD><TD BGCOLOR=\"black\"><\/TD><TD PORT=\"FROM_chick\">ordered<\/TD><\/TR>\n    <TR><TD PORT=\"TO_weight\">weight<\/TD><TD><\/TD><TD PORT=\"FROM_weight\">numeric<\/TD><\/TR>\n    <\/TABLE>>];\n\n  \"Time_Chick\":FROM_chick -> \"Chick\":TO_chick;\n}\n","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}# simple relation schemas
rschema <- synthesise(discover(ChickWeight))
txt_rschema <- gv(rschema)
cat(txt_rschema)
#> digraph {
#>   rankdir = "LR"
#>   node [shape=plaintext];
#> 
#>   "Chick" [label = <
#>     <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
#>     <TR><TD COLSPAN="2">Chick</TD></TR>
#>     <TR><TD PORT="TO_chick">Chick</TD><TD PORT="FROM_chick" BGCOLOR="black"></TD></TR>
#>     <TR><TD PORT="TO_diet">Diet</TD><TD PORT="FROM_diet"></TD></TR>
#>     </TABLE>>];
#>   "Time_Chick" [label = <
#>     <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
#>     <TR><TD COLSPAN="2">Time_Chick</TD></TR>
#>     <TR><TD PORT="TO_time">Time</TD><TD PORT="FROM_time" BGCOLOR="black"></TD></TR>
#>     <TR><TD PORT="TO_chick">Chick</TD><TD PORT="FROM_chick" BGCOLOR="black"></TD></TR>
#>     <TR><TD PORT="TO_weight">weight</TD><TD PORT="FROM_weight"></TD></TR>
#>     </TABLE>>];
#> }
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  DiagrammeR::grViz(txt_rschema)
}

{"x":{"diagram":"digraph {\n  rankdir = \"LR\"\n  node [shape=plaintext];\n\n  \"Chick\" [label = <\n    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n    <TR><TD COLSPAN=\"2\">Chick<\/TD><\/TR>\n    <TR><TD PORT=\"TO_chick\">Chick<\/TD><TD PORT=\"FROM_chick\" BGCOLOR=\"black\"><\/TD><\/TR>\n    <TR><TD PORT=\"TO_diet\">Diet<\/TD><TD PORT=\"FROM_diet\"><\/TD><\/TR>\n    <\/TABLE>>];\n  \"Time_Chick\" [label = <\n    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n    <TR><TD COLSPAN=\"2\">Time_Chick<\/TD><\/TR>\n    <TR><TD PORT=\"TO_time\">Time<\/TD><TD PORT=\"FROM_time\" BGCOLOR=\"black\"><\/TD><\/TR>\n    <TR><TD PORT=\"TO_chick\">Chick<\/TD><TD PORT=\"FROM_chick\" BGCOLOR=\"black\"><\/TD><\/TR>\n    <TR><TD PORT=\"TO_weight\">weight<\/TD><TD PORT=\"FROM_weight\"><\/TD><\/TR>\n    <\/TABLE>>];\n}\n","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}# simple database schema
dschema <- normalise(discover(ChickWeight))
txt_dschema <- gv(dschema)
cat(txt_dschema)
#> digraph {
#>   rankdir = "LR"
#>   node [shape=plaintext];
#> 
#>   "Chick" [label = <
#>     <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
#>     <TR><TD COLSPAN="2">Chick</TD></TR>
#>     <TR><TD PORT="TO_chick">Chick</TD><TD PORT="FROM_chick" BGCOLOR="black"></TD></TR>
#>     <TR><TD PORT="TO_diet">Diet</TD><TD PORT="FROM_diet"></TD></TR>
#>     </TABLE>>];
#>   "Time_Chick" [label = <
#>     <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
#>     <TR><TD COLSPAN="2">Time_Chick</TD></TR>
#>     <TR><TD PORT="TO_time">Time</TD><TD PORT="FROM_time" BGCOLOR="black"></TD></TR>
#>     <TR><TD PORT="TO_chick">Chick</TD><TD PORT="FROM_chick" BGCOLOR="black"></TD></TR>
#>     <TR><TD PORT="TO_weight">weight</TD><TD PORT="FROM_weight"></TD></TR>
#>     </TABLE>>];
#> 
#>   "Time_Chick":FROM_chick -> "Chick":TO_chick;
#> }
DiagrammeR::grViz(txt_dschema)

{"x":{"diagram":"digraph {\n  rankdir = \"LR\"\n  node [shape=plaintext];\n\n  \"Chick\" [label = <\n    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n    <TR><TD COLSPAN=\"2\">Chick<\/TD><\/TR>\n    <TR><TD PORT=\"TO_chick\">Chick<\/TD><TD PORT=\"FROM_chick\" BGCOLOR=\"black\"><\/TD><\/TR>\n    <TR><TD PORT=\"TO_diet\">Diet<\/TD><TD PORT=\"FROM_diet\"><\/TD><\/TR>\n    <\/TABLE>>];\n  \"Time_Chick\" [label = <\n    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n    <TR><TD COLSPAN=\"2\">Time_Chick<\/TD><\/TR>\n    <TR><TD PORT=\"TO_time\">Time<\/TD><TD PORT=\"FROM_time\" BGCOLOR=\"black\"><\/TD><\/TR>\n    <TR><TD PORT=\"TO_chick\">Chick<\/TD><TD PORT=\"FROM_chick\" BGCOLOR=\"black\"><\/TD><\/TR>\n    <TR><TD PORT=\"TO_weight\">weight<\/TD><TD PORT=\"FROM_weight\"><\/TD><\/TR>\n    <\/TABLE>>];\n\n  \"Time_Chick\":FROM_chick -> \"Chick\":TO_chick;\n}\n","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}# simple relations
rel <- create(synthesise(discover(ChickWeight)))
txt_rel <- gv(rel)
cat(txt_rel)
#> digraph {
#>   rankdir = "LR"
#>   node [shape=plaintext];
#> 
#>   "Chick" [label = <
#>     <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
#>     <TR><TD COLSPAN="3">Chick (0 records)</TD></TR>
#>     <TR><TD PORT="TO_chick">Chick</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_chick">logical</TD></TR>
#>     <TR><TD PORT="TO_diet">Diet</TD><TD></TD><TD PORT="FROM_diet">logical</TD></TR>
#>     </TABLE>>];
#>   "Time_Chick" [label = <
#>     <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
#>     <TR><TD COLSPAN="3">Time_Chick (0 records)</TD></TR>
#>     <TR><TD PORT="TO_time">Time</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_time">logical</TD></TR>
#>     <TR><TD PORT="TO_chick">Chick</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_chick">logical</TD></TR>
#>     <TR><TD PORT="TO_weight">weight</TD><TD></TD><TD PORT="FROM_weight">logical</TD></TR>
#>     </TABLE>>];
#> }
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  DiagrammeR::grViz(txt_rel)
}

{"x":{"diagram":"digraph {\n  rankdir = \"LR\"\n  node [shape=plaintext];\n\n  \"Chick\" [label = <\n    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n    <TR><TD COLSPAN=\"3\">Chick (0 records)<\/TD><\/TR>\n    <TR><TD PORT=\"TO_chick\">Chick<\/TD><TD BGCOLOR=\"black\"><\/TD><TD PORT=\"FROM_chick\">logical<\/TD><\/TR>\n    <TR><TD PORT=\"TO_diet\">Diet<\/TD><TD><\/TD><TD PORT=\"FROM_diet\">logical<\/TD><\/TR>\n    <\/TABLE>>];\n  \"Time_Chick\" [label = <\n    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n    <TR><TD COLSPAN=\"3\">Time_Chick (0 records)<\/TD><\/TR>\n    <TR><TD PORT=\"TO_time\">Time<\/TD><TD BGCOLOR=\"black\"><\/TD><TD PORT=\"FROM_time\">logical<\/TD><\/TR>\n    <TR><TD PORT=\"TO_chick\">Chick<\/TD><TD BGCOLOR=\"black\"><\/TD><TD PORT=\"FROM_chick\">logical<\/TD><\/TR>\n    <TR><TD PORT=\"TO_weight\">weight<\/TD><TD><\/TD><TD PORT=\"FROM_weight\">logical<\/TD><\/TR>\n    <\/TABLE>>];\n}\n","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
```
