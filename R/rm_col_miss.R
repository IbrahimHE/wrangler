#' @title Remove columns with missing values
#' @description This function remove columns with missing values
#' @param data specify a dataframe containing variables
#' @returns returns a dataframe with columns with missing values removed
#' @export
rm_col_miss <- function(data) {
  # return missing data information
  g <- check_col_miss(data)

  # extract the names of columns with no missing data
  comcol <- rownames(g)[which(g == g[g == 0])]

  #extract the data with no missing data in its columns
  data[,comcol]
}

# f <- rm_col_miss(anime)
# microbenchmark(sapply(anime, function(x) sum(is.na(x))))
# microbenchmark(apply(anime, 2, function(x) sum(is.na(x))))
# microbenchmark(rm_col_miss(anime))
