% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/equivalence.r
\name{df_equiv}
\alias{df_equiv}
\title{Test data frames for equivalence under row reordering}
\usage{
df_equiv(df1, df2, digits = getOption("digits"))
}
\arguments{
\item{df1, df2}{Data frames.}

\item{digits}{a positive integer, indicating how many significant digits are
to be used for numeric and complex variables. A value of NA results in no
rounding. By default, this uses \code{getOption("digits")}, similarly to
\code{\link{format}}. See the note in \code{\link{print.default}} about
digits >= 16.}
}
\value{
A logical.
}
\description{
A convenience function, mostly used to testing that \code{\link{rejoin}}
works as intended. It checks that data frames have the same dimensions and
column names, with duplicates allowed, then checks they contain the same
data. For the latter step, column names are made unique first, so columns
with duplicate names must be presented in the same order in both data frames.
}
