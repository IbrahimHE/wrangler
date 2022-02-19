#' @title Return character into a vector (you can copy into r source file instead of writing it manually)
#' @description This function returns characters into a vector (you can copy into r source file instead of writing it manually)
#' @param x specify character or string vector to be converted
#' @param writeline TRUE/FALSE whether to return interpretable written language or not
#' @returns returns a dataframe
#' @examples
#' \dontrun{
#' letters[1:4]
#' chr_string(letters[1:4])
#' chr_string(letters[1:4], writeline = T)
#' chr_string(letters[1:4], writeline = F)
#' }
#' @export
chr_string <- function(x, writeline = NULL) {
  # enclose each chr element with ""
  a <- encodeString(x, quote = '"')
  # paste the element as one character separated by ,
  b <- paste(a, collapse = ", ")
  # paste the character and enclose it withing c()
  c <- paste("c(", b, ")", collapse = "")

  if (is.null(writeline)) {
    writeLines(c)
  } else if (writeline == T) {
    c
  } else {
    writeLines(c)
  }
}
