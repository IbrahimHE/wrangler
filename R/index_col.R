#' @title Get index of a dataframe column by name, or get a name by index
#' @param data must be a dataframe
#' @param column specify the column name to return the index
#' @param id specify the index of wanted column to return the name
#' @details Provide either an id or a name to return a name or an id repectively
#' @return the index of column name, or the column name of the index
#' @author Ibrahim H. Elkhidir, originally
#' @export
#' @examples
#' \dontrun{
#' index_col(iris, column = "Sepal.Length")
#' index_col(iris, id = 1)
#' index_col(iris, id = c(1, 2))
#' }
index_col <- function(data, column = NULL, id = NULL) {
  a <- colnames(data)
  if (is.null(column) & is.null(id)) {
    message("Please enter either colnames or id")
  } else if (!is.null(column) & is.null(id)) {
    which(a == column)
  } else {
    a[id]
  }
}


# which(colnames(iris) == 'Sepal.Length')
# index_col(iris, column = 'Sepal.Length')
# index_col(iris, id = 1)
# index_col(iris, id = c(1,2))


# f <- c('#141414', '#333333', '#0fa1e0', '#ffffff', '#000000', '#262626' ,'#070707' , '#31b7f1', '#0c7fb0', '#0a6d98')
# plotCol(f, nrow = 2)

#' @title Get index of a dataframe column by name, or get a name by index
#' @param data must be a dataframe
#' @param column specify the column name to return the index
#' @param id specify the index of wanted column to return the name
#' @details Provide either an id or a name to return a name or an id repectively
#' @return the index of column name, or the column name of the index
#' @author Ibrahim H. Elkhidir, originally
#' @export
#' @examples
#' \dontrun{
#' index_col(iris, column = "Sepal.Length")
#' index_col(iris, id = 1)
#' index_col(iris, id = c(1, 2))
#' }
index_col <- function(data, column = NULL, id = NULL) {
  a <- colnames(data)
  if (is.null(column) & is.null(id)) {
    message("Please enter either colnames or id")
  } else if (!is.null(column) & is.null(id)) {
    which(a == column)
  } else {
    a[id]
  }
}

# which(colnames(iris) == 'Sepal.Length')
# index_col(iris, column = 'Sepal.Length')
# index_col(iris, id = 1)
# index_col(iris, id = c(1,2))


# f <- c('#141414', '#333333', '#0fa1e0', '#ffffff', '#000000', '#262626' ,'#070707' , '#31b7f1', '#0c7fb0', '#0a6d98')
# plotCol(f, nrow = 2)
