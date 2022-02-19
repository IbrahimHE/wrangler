#' @title Get categorical variable in a dataframe
#' @description get categorical variable in a dataframe
#' @param data specify a dataframe containing variables
#' @returns returns a dataframe with frequancy table
#' @export get_categ_var
get_categ_var <- function(data) {
  ind <- lapply(data, class)
  j <- c(ind[] == 'character' | ind[] == 'factor')
  ind1 <- wrangler::index_val(data.frame(j), TRUE, ind = 'row')
  data[,ind1]
}
