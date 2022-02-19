#' @title Get Chi-square test of a specific variable with other categorical variables in a dataframe
#' @description Get Chi-square test of a specific variable with other categorical variables in a dataframe
#' @param data specify a dataframe containing variables
#' @param var specify a categorical variable to get the chi-square tests
#' @returns returns a dataframe with chi-square test
#' @importFrom stats chisq.test
#' @export mchisq.test
mchisq.test <- function(data, var) {
  h <- as.data.frame(get_categ_var(data))
  # var <- as.character(quote(y))
  id <- wrangler::index_col(h, column = var)
  df1 <- h[wrangler::other_names(h,var)]
  y <- data[var]
  names(h[,id]) <- var
  j <- suppressWarnings(mapply(function(x, y) chisq.test(x, y)$p.value, h[,-id], MoreArgs=list(h[,id])))
  i <- suppressWarnings(mapply(function(x, y) chisq.test(x, y)$statistic, h[,-id], MoreArgs=list(h[,id])))
  names(i) <- paste0(names(h)[id],'-',names(i))
  names(i) <- gsub('.X-squared','',names(i))
  result <- data.frame('ID' = names(i),'Chi-square value' = i[], 'p-value'=j)
  rownames(result) <- NULL
  result
}
