#' @title Getting near r colors
#' @param rgb show the col
#' @param cSpace specify the number of rows
#' @param dist specify the number of columns
#' @return the grid layout, invisibly
#' @author Ibrahim H. Elkhidir, originally
#' @import grDevices
#' @export
# Find close R colors() to a given color {original by Marius Hofert)
# using Euclidean norm in (HSV / RGB / ...) color space
nearRcolor <- function(rgb, cSpace = c("hsv", "rgb255", "Luv", "Lab"),
                       dist = switch(cSpace,
                         "hsv" = 0.10,
                         "rgb255" = 30,
                         "Luv" = 15,
                         "Lab" = 12
                       )) {
  if (is.character(rgb)) rgb <- col2rgb(rgb)
  stopifnot(length(rgb <- as.vector(rgb)) == 3)
  Rcol <- col2rgb(.cc <- colors())
  uniqC <- !duplicated(t(Rcol)) # gray9 == grey9 (etc)
  Rcol <- Rcol[, uniqC]
  .cc <- .cc[uniqC]
  cSpace <- match.arg(cSpace)
  convRGB2 <- function(Rgb, to) {
    t(convertColor(t(Rgb), from = "sRGB", to = to, scale.in = 255))
  }
  ## the transformation,  rgb{0..255} --> cSpace :
  TransF <- switch(cSpace,
    "rgb255" = identity,
    "hsv" = rgb2hsv,
    "Luv" = function(RGB) convRGB2(RGB, "Luv"),
    "Lab" = function(RGB) convRGB2(RGB, "Lab")
  )
  d <- sqrt(colSums((TransF(Rcol) - as.vector(TransF(rgb)))^2))
  iS <- sort.list(d[near <- d <= dist]) # sorted: closest first
  setNames(.cc[near][iS], format(zapsmall(d[near][iS]), digits = 3))
}
