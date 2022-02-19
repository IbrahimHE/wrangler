#' @title Get multiple response frequency tables
#' @description get percentage of a "variable" in data, which is separated by another variable, from the the total total "variable" in data1
#' @param data specify a dataframe containing variables
#' @param question.prefix specify prefix for the the wanted questions in data.
#' @param colname specify the name of question variable in the returned freq table.
#' @returns returns a dataframe with frequancy table
#' @export mrfreqtable
mrfreqtable <- function(data, question.prefix, colname = NULL) {
  # Find the columns with the questions
  a <- grep(question.prefix, names(data))
  # Find the total number of responses
  b <- sum(data[, a] != 0)
  # Find the totals for each question
  d <- colSums(data[, a] != 0)
  # Find the number of respondents
  e <- sum(rowSums(data[, a]) != 0)
  # d + b as a vector. This is your overfall frequency
  f <- as.numeric(c(d, b))
  df <- data.frame(
    question = c(names(d), "Total"),
    freq = f,
    percent = (f / b) * 100,
    percent_of_cases = (f / e) * 100
  )
  if(!is.null(colname)) {colnames(df)[1] <- colname}
  df[,1] <- gsub(question.prefix,"", df[,1])
  df
}

# k <- slice_col(animeCopy, 'Genres', sep = ', ',fill = 0, trans = T)
# k <- add_prefix("G_",k)
# .GlobalEnv$mrfreqtable(k,"G_", "Var")
