% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renamer.R
\name{renamer}
\alias{renamer}
\title{Renaming dataframe columns}
\usage{
renamer(data, from, to, warn_missing = TRUE)
}
\arguments{
\item{data}{a dataframe or a vector}

\item{from}{names in data to be changed from}

\item{to}{names to changed to}

\item{warn_missing}{TRUE/FALSE whether a warning message will be returned if the from values not present in data}
}
\description{
a wrapper function for renaming functions in plyr package
}
\examples{
l <- "first"
k <- "second"
renamer(names(iris), from = "Species", to = "k")
renamer(names(iris), from = c("Species", "Sepal.Length"), to = c("hey", "tyfg"))
renamer(names(iris), from = names(iris)[1], to = c("hey"))
renamer(names(iris), from = 1, to = "k")
renamer(names(iris), from = 1, to = l)
renamer(names(iris), from = "Species", to = k)
renamer(names(iris), from = "Specis", to = k)

renamer(iris, from = "Species", to = "k")
renamer(iris, from = c("Species", "Sepal.Length"), to = c("hey", "tyfg"))
renamer(iris, from = names(iris)[1], to = c("hey"))
renamer(iris, from = 1, to = "k")
renamer(iris, from = 1, to = l)
renamer(iris, from = "Species", to = k)
renamer(iris, from = "Specis", to = k)
}
