#' @name not in
#' @title  \strong{x not in y: \%!in\%}
#' @description This function used to negate the "\code{\%in\%}" operator = \strong{x not in y}
#' @param x first or left arm
#' @param y second or right arm
#' @details x \code{\%!in\%} y == \strong{x not in y}
#' @export
"%!in%" <- function(x, y) {
  !("%in%"(x, y))
} # function to negate %in%
# `%!in%` <- function(x,y) {!(x %in% y)} # function to negate %in%
# library(dplyr)
# h <- seq(1:4)
# b <- letters[1:4]
# h %in% b
# h %!in% b
# rm(`%!in%`)


# "%w/o%" <- function(x, y) x[!x %in% y]
# a <- c( "1", "2", "3", "4", "5" )
# b <- c( "2", "3", "4", "5", "6" )
# a %w/o% b
# a[a %!in% b]
# b[b %in% a]
# b %w/o% a
# c(1:6,7:2) %w/o% c(3,7,12)  # -> keeps duplicates
# setdiff(c(1:6,7:2), c(3,7,12)) # -> unique values
