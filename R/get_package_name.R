#' @title Get package name
#' @description Get package name
#' @param pkg specify columns to be deleted
#' @returns returns package name
#' @examples
#' \dontrun{
#' get_package_name("wrangler")
#' }
#' @export
get_package_name <- function(pkg) {
  gsub(
    "_[.](zip|tar[.]gz|tar[.]bzip2|tar[.]xz)", "",
    gsub(
      .standard_regexps()$valid_package_version,
      "", basename(pkg)
    )
  )
}
