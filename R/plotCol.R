##' @title Comparing Colors
##' @param col give a color or a vector of colors
##' @param nrow number of rows for the the color to be displayed in
##' @param ncol number of columns for the the color to be displayed in
##' @param txt.col default is black
##' @return the grid layout, invisibly
##' @author Marius Hofert, originally
##' @import grid
##' @export
plotCol <- function(col, nrow = 1, ncol = ceiling(length(col) / nrow),
                    txt.col = "black") {
  stopifnot(nrow >= 1, ncol >= 1)
  if (length(col) > nrow * ncol) {
    warning("some colors will not be shown")
  }
  # require(grid)
  grid.newpage()
  gl <- grid.layout(nrow, ncol)
  pushViewport(viewport(layout = gl))
  ic <- 1
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      pushViewport(viewport(layout.pos.row = i, layout.pos.col = j))
      grid.rect(gp = gpar(fill = col[ic]))
      grid.text(col[ic], gp = gpar(col = txt.col))
      upViewport()
      ic <- ic + 1
    }
  }
  upViewport()
  invisible(gl)
}
