#' @title Remove a suffix in a dataframe column
#' @description This function used to Remove a suffix in a dataframe column or columns
#' @param suffix specify a suffix for the wanted columns in data.
#' @param data specify data frame
#' @param column specify a dataframe containing variables
#' @returns returns a dataframe with suffix removed
#' @examples
#' df <- add_suffix("_G", head(iris, 2))
#' rm_suffix("_G", df)
#' @export
rm_suffix <- function(suffix, data, column) {
  # a <- as.data.frame(unlist(strsplit(column, suffix)))
  # a[a != '']
  colnames(data)[column] <- gsub(suffix, "", colnames(data)[column])
  data
}
# demo_r_s <- head(iris)
# add_suffix("_G", demo_r_p)
# rm_suffix("_G", demo_r_p)
