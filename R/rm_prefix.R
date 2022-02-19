#' @title Remove a prefix in a dataframe column
#' @description This function used to Remove a prefix in a dataframe column or columns
#' @param prefix specify a prefix for the wanted columns in data.
#' @param data specify data frame
#' @param column specify a dataframe containing variables
#' @returns returns a dataframe with prefix removed
#' @examples
#' df <- add_prefix("G_", head(iris, 2))
#' rm_prefix("G_", df)
#' @export rm_prefix
rm_prefix <- function(prefix, data, column) {
  # a <- as.data.frame(unlist(strsplit(column, prefix)))
  # a[a != '']
  colnames(data)[column] <- gsub(prefix, "", colnames(data)[column])
  data
}
