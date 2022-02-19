#' @title Add a prefix in a dataframe column
#' @description This function used to add a prefix in a dataframe column or columns
#' @param prefix specify a prefix for the wanted columns in data.
#' @param data specify data frame
#' @param column specify a dataframe containing variables
#' @returns returns a dataframe with prefix added
#' @examples add_prefix("G_", head(iris, 2))
#' @export add_prefix
add_prefix <- function(prefix, data, column) {
  colnames(data)[column] <- paste0(prefix, colnames(data)[column])
  data
}
