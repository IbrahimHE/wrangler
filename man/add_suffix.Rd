% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_suffix.R
\name{add_suffix}
\alias{add_suffix}
\title{Add a suffix in a dataframe column}
\usage{
add_suffix(suffix, data, column)
}
\arguments{
\item{suffix}{specify a suffix for the wanted columns in data.}

\item{data}{specify data frame}

\item{column}{specify a dataframe containing variables}
}
\value{
returns a dataframe with suffix added
}
\description{
This function used to add a suffix in a dataframe column or columns
}
\examples{
add_suffix("_G", head(iris, 2))
}
