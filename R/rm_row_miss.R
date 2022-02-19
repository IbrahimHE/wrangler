#' @title Remove rows with missing values
#' @description This function remove rows with missing values
#' @param data specify a dataframe containing variables
#' @returns returns a dataframe with rows with missing values removed
#' @export
rm_row_miss <- function(data) {
  data[check_row_miss(data) == 0,]
}

{
  # rm_row_miss_o <- function(data) {
  #   m <- c()
  #   # Remove missing values
  #   for (i in 1:nrow(data)) {
  #     data$m[i] <- sum(is.na(data[i, ]))
  #   }
  #   # filter complete data by missing variable 'm'
  #   data <- data %>%
  #     dplyr::filter(m == 0) %>%
  #     dplyr::select(colnames(data)[1:(ncol(data) - 1)])
  #   # make sure no missing value it should equal to 0
  #   missing <- sum(is.na(data))
  #   return(data)
  #   print(paste("missing values are", missing))
  # }
}
# a <- data.frame(A = c(1:3, NA), B = c(1:4))

# library(microbenchmark)
# microbenchmark(rm_row_miss(anime))
# microbenchmark(rm_row_miss_o(anime))

