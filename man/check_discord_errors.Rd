% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_regression.R
\name{check_discord_errors}
\alias{check_discord_errors}
\title{Check Discord Errors}
\usage{
check_discord_errors(data, id, sex, race, pair_identifiers)
}
\arguments{
\item{data}{The data to perform a discord regression on.}

\item{id}{A unique kinship pair identifier.}

\item{sex}{A character string for the sex column name.}

\item{race}{A character string for the race column name.}

\item{pair_identifiers}{A character vector of length two that contains the variable identifier for each kinship pair.}
}
\value{
An error message if one of the conditions are met.
}
\description{
This function checks for common errors in the provided data, including the correct specification of identifiers (ID, sex, race) and their existence in the data.
}
