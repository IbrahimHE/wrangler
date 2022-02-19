#' @title get remaining names of a dataframe
#' @description this function used to get remaining names of a dataframe
#' @examples
#' \dontrun{
#' othernames(iris, "Species")
#' }
#' @param data specify a dataframe object
#' @param name specify the column to be be sliced in quotation marks ""
#' @export
other_names <- function(data, name) {
  setdiff(names(data), name)
}


# mydf <- data.frame(a = 1:2, b = 3:4, c = 5:6)
# microbenchmark::microbenchmark(othernames(mydf, "a"))
# microbenchmark::microbenchmark(other_names(mydf, "a"))

