#' @title Get index of a value in dataframe, row or colum or both
#' @param data specify a dataframe
#' @param value specify the of which the index will be returned
#' @param ind specify the index to be returned: "row", "col","both"
#' @param pair TRUE/FALSE if the index to be returned as pairs like [x,y]
#' @return the index of a value: within a column, a row, or both like [x,y]
#' @author Ibrahim H. Elkhidir
#' @export
#'
# To do: Correct the function to work also in vector, not only dataframes like match
index_val <- function(data, value, ind = NULL, pair = NULL) {
  if (is.data.frame(data)) {
    d <- which(data == value, arr.ind = T)
    f <- vector()
    for (i in 1:nrow(d)) {
      f[i] <- paste0("[", d[i, 1], ",", d[i, 2], "]")
    }
  } else {
    d <- match(value, data)
    names(d) <- data[d]
    d
  }

  i <- c("row", "col", "both")

  if (is.data.frame(data)) {
    if (is.null(ind)) {
      message('Please specify "ind" argument from c("row","col","both").\nThe default is printing both row and column index')
      return(d)
    } else {
      if (ind %in% i) {
        if (ind == "row") {
          return(d[, 1])
        } else if (ind == "col") {
          return(d[, 2])
        } else {
          if (is.null(pair)) {
            message('Please specify "pair" argument TRUE or FALSE.\nThe default is printing both row and column index but Not paired')
            return(d)
          } else if (pair == TRUE) {
            return(f)
          } else {
            return(d)
          }
        }
      } else {
        message('Please choose either "row", "col" or "both" ')
      }
    }
  } else {
    if (!is.null(ind) | !is.null(pair)) {
      message("Warning: arguments 'ind', 'pair' is ignored because provided data is not a dataframe.")
      f
    } else {
      f
    }
  }
}

# library(dplyr)
# library(wrangler)
# data = search()
# value = c("package:wrangler")
# value = c("package:wrangler", "package:dplyr")
# ind = 'row'
# if (is.data.frame(data)) {
#   d <- which(data == value, arr.ind = T)
#   f <- vector()
#   for (i in 1:nrow(d)) {
#     f[i] <- paste0("[", d[i, 1], ",", d[i, 2], "]")
#   }
# } else {
#   d <- match(value, data)
#   names(d) <- data[d]
#   d
# }

# index_val(data, value, pair = T)
# index_val(data, value)
# index_val(data, value,'row')
