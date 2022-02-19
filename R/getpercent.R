#' @title Get percentage of a "variable" in data
#' @description get percentage of a "variable" in data, which is separated by another variable, from the the total total "variable" in data1
#' @param data specify dataframe
#' @param data1 specify second dataframe
#' @param var specify data frame
#' @returns returns a dataframe
#' @export
# get percentage of a "variable" in data, which is separated by another variable, from the the total total "variable" in data1
getpercent <- function(data, data1, var) {
  # for (i in 1:nrow(f)) {
  #   index = match(f$Traffic_source[i], d$Traffic_source)
  #   f$p[i] = f$N[i]/(d$n[index]) * 100
  # }
  colindex <- match(var, colnames(data))
  for (i in 1:nrow(data)) {
    index <- match(data[i, colindex], data1[, colindex])
    data$p[i] <- data$n[i] / (data1$n[index]) * 100
  }
  return(as.data.frame(data))
}
