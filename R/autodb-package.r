#' Database-style normalisation for data.frames
#'
#' Automatic normalisation of a data.frame to third normal form, with the
#' intention of easing the process of data cleaning. (Usage to design your
#' actual database for you is not advised.) Originally inspired by Alteryx's
#' AutoNormalize Python library (https://github.com/alteryx/autonormalize), with
#' various changes and improvements. Automatic discovery of functional or
#' approximate dependencies, normalisation based on those, and plotting of the
#' resulting "database" via Graphviz, with options to exlude some attributes at
#' discovery time, or remove discovered dependencies at normalisation time.
#'
#' @docType package
#' @name autodb-package
NULL
