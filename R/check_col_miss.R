#' @title Chech for columns missing values
#' @description This function test for missing values in a dataframe columns
#' @param data specify dataframe
#' @param decreasing TRUE/FALSE order missing values
#' @param transpos TRUE/FALSE
#' @returns returns a dataframe containing number of missing values per dataframe column
#' @export
# check for missing in every column in data frame 'data' ----
check_col_miss <- function(data, decreasing = TRUE, transpos = NULL) {

  missvalues <- data.frame(missing = apply(data, 2, function(x) sum(is.na(x))))

  if (is.null(decreasing)) missvalues <- missvalues

  if (decreasing == T) {
    missvalues <- dplyr::arrange(missvalues, dplyr::desc(missing))
  } else if (decreasing == F) {
    missvalues <- dplyr::arrange(missvalues, missing)
  }

  if (is.null(transpos)) {
    missvalues <- missvalues
  } else if (transpos == T) {
    missvalues <- t(missvalues)
  } else {
    missvalues <- missvalues
  }

  missvalues
}

# data(iris)
# library(dplyr)
# g <- check_col_miss(animeCopy, T)
# microbenchmark(check_col_miss(anime))
