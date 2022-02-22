#' @title Loading packages
#' @param pkg specify packages names
#' @param ... specify additional packages separated by ",".
#' For example: "pkg1","pkg2"
#' @return the grid layout, invisibly
#' @author Ibrahim H. Elkhidir, originally
#' @examples
#' \dontrun{
#' loadpackages("dplyr", "sjPlot", "wrangler")
#' }
#' @import utils
#' @export
loadpackages <- function(pkg, ...) {
  req <- substitute(require(x, character.only = TRUE))
  libs <- c(pkg, ...)
  sapply(libs, function(x) {
    eval(req) || {
      install.packages(x)
      eval(req)
    }
  })
}
# loadpackages('dplyr',"sjPlot", "wrangler")
# loadpackages(pn("dplyr"), pn("wrangler"))
# loadpackages("dplyr", 'ggplot2', "dplyr", "ggplot2", pn("dplyr"))




# function 4
# get_package_name <- function(...) {
# pkgs = c(...)
# unlist(lapply(pkgs,pn))
# gsub("_[.](zip|tar[.]gz|tar[.]bzip2|tar[.]xz)", "",
#      gsub(.standard_regexps()$valid_package_version,"",
#           basename()
#      ))
# }
# pkgnames <- get_package_name( c("dplyr","ggplot2"))
# library()
# get_package_name("ggpubr", "ggplor", ggproto())
# l <- c("dplyr", 'ggplot', "dplyr")
# c <- unlist(lapply(l, basename))
# class(c);length(c)
