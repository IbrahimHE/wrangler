#' @title Get mode of a vector
#' @param x specify a vector of numbers
#' @export
get_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# @title Get minimum value within a vector
# @param x specify a vector of numbers
# @export
# get_min <- function(x) {
#   x[which.min(x)]
# }

# @title Get maximum value within a vector
# @param x specify a vector of numbers
# @export
# get_max <- function(x) {
#   x[which.max(x)]
# }

# a <- c(rep(7,5),3,3:9)
# a
# getmin(a)
# getmax(a)
# getmode(a)
# which.min(a)
# which.max(a)
