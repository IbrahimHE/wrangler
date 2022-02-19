#' @title Add a suffix in a dataframe column
#' @description This function used to add a suffix in a dataframe column or columns
#' @param suffix specify a suffix for the wanted columns in data.
#' @param data specify data frame
#' @param column specify a dataframe containing variables
#' @returns returns a dataframe with suffix added
#' @examples add_suffix("_G", head(iris, 2))
#' @export
add_suffix <- function(suffix, data, column) {
  colnames(data)[column] <- paste0(colnames(data)[column], suffix)
  data
}

add_suffix("_G", head(iris, 2))
