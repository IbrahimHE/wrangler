#' @title Get the prime numbers of up to a number(n)
#' @description This function print the prime numbers from lowest up to an integer value(n)
#' @param n integer number
#' @export prime_numbers
prime_numbers <- function(n) {
  if (is.wholenumber(n)) {
    if (n >= 2) {
      x <- seq(2, n)
      prime_nums <- c()
      for (i in seq(2, n)) {
        if (any(x == i)) {
          prime_nums <- c(prime_nums, i)
          x <- c(x[(x %% i) != 0], i)
        }
      }
      return(prime_nums)
    } else {
      stop("Input number should be at least 2.")
    }
  } else {
    message("Input number should be integer or a whole number.")
  }
}

# x <- seq(1, 5, by = 0.5)
# prime_numbers(x)
