% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_regression.R
\name{make_mean_diffs}
\alias{make_mean_diffs}
\title{Make Mean Differences}
\usage{
make_mean_diffs(
  data,
  id,
  sex,
  race,
  demographics,
  variable,
  pair_identifiers,
  row,
  coding_method = "none"
)
}
\arguments{
\item{data}{The data set with kinship pairs}

\item{id}{Default's to NULL. If supplied, must specify the column name
corresponding to unique kinship pair identifiers.}

\item{sex}{A character string for the sex column name.}

\item{race}{A character string for the race column name.}

\item{demographics}{Indicator variable for if the data has the sex and race
demographics. If both are present (default, and recommended), value should
be "both". Other options include "sex", "race", or "none".}

\item{variable}{outcomes and predictors for manipulating the data}

\item{pair_identifiers}{A character vector of length two that contains the
variable identifier for each kinship pair}

\item{row}{The row number of the data frame}

\item{coding_method}{A character string that indicates what kind of
additional coding schemes should be used. Default is none. Other options include "binary" and "multi".}
}
\description{
This function calculates differences and means of a given variable for each kinship pair. The order of subtraction and the variables' names in the output dataframe depend on the order column set by check_sibling_order().
If the demographics parameter is set to "race", "sex", or "both", it also prepares demographic information accordingly,
swapping the order of demographics as per the order column.
}
