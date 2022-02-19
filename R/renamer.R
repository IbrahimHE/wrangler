#' @title Renaming dataframe columns
#' @description a wrapper function for renaming functions in plyr package
#' @param data a dataframe or a vector
#' @param from names in data to be changed from
#' @param to names to changed to
#' @param warn_missing TRUE/FALSE whether a warning message will be returned if the from values not present in data
#' @examples
#' l <- "first"
#' k <- "second"
#' renamer(names(iris), from = "Species", to = "k")
#' renamer(names(iris), from = c("Species", "Sepal.Length"), to = c("hey", "tyfg"))
#' renamer(names(iris), from = names(iris)[1], to = c("hey"))
#' renamer(names(iris), from = 1, to = "k")
#' renamer(names(iris), from = 1, to = l)
#' renamer(names(iris), from = "Species", to = k)
#' renamer(names(iris), from = "Specis", to = k)
#'
#' renamer(iris, from = "Species", to = "k")
#' renamer(iris, from = c("Species", "Sepal.Length"), to = c("hey", "tyfg"))
#' renamer(iris, from = names(iris)[1], to = c("hey"))
#' renamer(iris, from = 1, to = "k")
#' renamer(iris, from = 1, to = l)
#' renamer(iris, from = "Species", to = k)
#' renamer(iris, from = "Specis", to = k)
#' @export
renamer <- function(data, from, to, warn_missing = TRUE) {
  if (!is.atomic(data)) {
    x <- names(data)
    # stop("`x` must be an atomic vector.")
  } else {
    x <- data
  }

  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }

  if (is.factor(x)) {
    levels(x) <- renamer(levels(x), from, to, warn_missing)
    return(x)
  }

  if (!is.character(from)) {
    from <- x[from]
  }

  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message(
      "The following `from` values were not present in `x`: ",
      paste(from[!(1:length(from) %in% from_found)], collapse = ", ")
    )
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]

  if (!is.atomic(data)) {
    names(data) <- x
    data
  } else {
    x
  }
}

# l <- "first"
# k <- "second"
# renamer(names(iris),  from = "Species", to = "k")
# renamer(names(iris), from = c("Species","Sepal.Length"), to = c("hey","tyfg"))
# renamer(names(iris), from = names(iris)[1], to = c("hey"))
# renamer(names(iris), from = 1, to = "k")
# renamer(names(iris), from = 1, to = l)
# renamer(names(iris), from = "Species", to = k)
# renamer(names(iris), from = "Specis", to = k)
#
# renamer(iris,  from = "Species", to = "k")
# renamer(iris, from = c("Species","Sepal.Length"), to = c("hey","tyfg"))
# renamer(iris, from = names(iris)[1], to = c("hey"))
# renamer(iris, from = 1, to = "k")
# renamer(iris, from = 1, to = l)
# renamer(iris, from = "Species", to = k)
# renamer(iris, from = "Specis", to = k)
