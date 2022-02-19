#' @title Get numerical variable in a dataframe
#' @description get numerical variable in a dataframe
#' @param data specify a dataframe containing variables
#' @returns returns a dataframe with frequancy table
#' @export get_num_var
get_num_var <- function(data) {
  ind <- lapply(data, class)
  j <- c(ind[] == 'integer' | ind[] == 'numeric')
  ind1 <- wrangler::index_val(data.frame(j), TRUE, ind = 'row')
  data[,ind1]
}
