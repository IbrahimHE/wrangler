#' @title Chech for rows missing values
#' @description This function test for missing values in a dataframe rows
#' @param data specify dataframe
#' @param decreasing TRUE/FALSE order missing values
#' @param transpos TRUE/FALSE
#' @returns returns a dataframe containing number of missing values per dataframe row
#' @export
#' @import dplyr
# check for missing in every row in data frame 'data' ----
check_row_miss <- function(data, decreasing = NULL, transpos = NULL) {

  missvalues <- data.frame(missing = apply(data, 1, function(x) sum(is.na(x))))

  if (is.null(decreasing)) {
    missvalues <- missvalues
  } else if (decreasing == T) {
    missvalues <- dplyr::arrange(missvalues, desc(missing))
  } else {
    missvalues <- missvalues
  }

  if (is.null(transpos)) {
    missvalues <- missvalues
  } else if (transpos == T) {
    missvalues <- t(missvalues)
    colnames(missvalues) <- c(1:nrow(data))
  }

  missvalues
}

# demo_r_miss <- data.frame(
#   A = c(NA, NA, 1, 3),
#   B = c(NA, 2, 1, NA),
#   C = c(NA, NA, 1, 3)
# )
# h <- check_row_miss(demo_r_miss, transpos = T);h
# microbenchmark(check_row_miss(anime))
# hist(microbenchmark(check_row_miss(anime))$time)
