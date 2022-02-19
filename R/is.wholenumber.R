#' @title Test a number for being an integer or not
#' @description This function test a number or a vector of number for being integer or not
#' @param x a number or a vector of numbers
#' @examples
#' \dontrun{
#' is.wholenumber(4)
#' TRUE
#'
#' is.wholenumber(4.5)
#' FALSE
#'
#' is.wholenumber(c(1, 3, 4.5, 6, 7.3))
#' }
#' @export
is.wholenumber <- function(x) {
  tol <- .Machine$double.eps^0.5
  abs(x - round(x)) < tol
}

# x <- seq(1, 5, by = 0.5)
# prime_numbers(x)
# is.integer(x)
# is.wholenumber(4)
# is.wholenumber(4.5)
# is.wholenumber(c(1,3,4.5,6,7.3))
