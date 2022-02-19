#' @title remove row or columns with percentage of missing values
#' @description This function test for missing values in a dataframe rows and columns, then removes rows or columns with percentage of missing values
#' @param data specify dataframe
#' @param dim 1 for rows, or 2 for columns
#' @param percent_miss numeric. cut of number for row or column which is equal or above to be removed
#' @returns returns a dataframe containing remaining row or columns
#' @export
#' @import dplyr
rm_miss <- function(data, dim, percent_miss) {
  if(percent_miss < 0 | percent_miss > 100) {
    stop('Percent should be between 0 and 1 or 0 and 100')
  }
  if (dim == 1) {
    a1 <- data[((rowSums(is.na(data))/ncol(data)*100) >= percent_miss),]
    dplyr::setdiff(data,a1)
  }
  else if(dim == 2) {
    a1 <- data[((colSums(is.na(data))/nrow(data)*100) >= percent_miss),]
    dplyr::setdiff(data,a1)
  }
  else
    stop('dim should be either 1 for "rows", or 2 for "columns"')
}
# rm_miss1 <- function(data, dim, percent_miss) {
#   if (dim == 1) {
#     data[check_row_miss(data) >= percent_miss,]
#   }
#   else if(dim == 2) {
#     # return missing data information
#     g <- check_col_miss(data)
#
#     # extract the names of columns with no missing data
#     comcol <- rownames(g)[which(g == g[g >= percent_miss])]
#
#     #extract the data with no missing data in its columns
#     data[,other_names(data,comcol)]
#   }
#   else
#     message('dim should be either 1 for "rows", or 2 for "columns"')
# }

# wrangler::index_val(a, ((colSums(is.na(a))/nrow(a)*100) > 10))[,0]
# wrangler::index_val(a, ((rowSums(is.na(a))/nrow(a)*100) > 20))[,0]
# a
#
# library(splitstackshape)
# temp <- head(concat.test)
# library(wrangler)
# data <- slice_col(temp, "Likes", mode = 'value', nearcol = T)
# a1 <- data[((rowSums(is.na(data))/ncol(data)*100) >= 20),]
# setdiff(data,a1)
# rm_miss1(data, dim = 1, 2)
# library(dplyr)
# setdiff()
# a
# rm_miss(a, dim = 2, 20)


