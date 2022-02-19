#' @title Reload a package or name space
#' @param pkg specify package name
#' @examples
#' reload("dplyr")
#' @export
reload <- function(pkg) {
  p <- paste0("package:", pkg)
  db <- search()
  if (isTRUE(db[db == p] == p)) {
    i <- match(p, db)
  } else {
    i <- NA
  }
  if (is.na(i)) {
    message("The package isn't loaded. The package is loading now:")
    base::require(pkg, character.only = T, warn.conflicts = F)
  } else {
    message("The package is already loaded, it will be detached and reloaded again.")
    detach(pos = i, unload = T, force = T)
    message("")
    message("The package is loading now:")
    base::require(pkg, character.only = T, warn.conflicts = F)
  }
}

# reload("dplyr")
