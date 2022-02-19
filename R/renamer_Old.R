#' @title Renaming dataframe columns (Old version similar to plyr::rename())
#' @description a wrapper function for renaming functions in plyr package
#' @param x a dataframe
#' @param replace original value and replace_with value
#' @param warn_missing TRUE/FALSE
#' @param warn_duplicated TRUE/FALSE
renamer_Old <- function(x, replace, warn_missing = TRUE, warn_duplicated = TRUE) {

  # mapvalues
  mapvalues <- function(x, from, to, warn_missing = TRUE) {
    if (length(from) != length(to)) {
      stop("`from` and `to` vectors are not the same length.")
    }
    if (!is.atomic(x)) {
      stop("`x` must be an atomic vector.")
    }
    if (is.factor(x)) {
      levels(x) <- mapvalues(levels(x), from, to, warn_missing)
      return(x)
    }
    mapidx <- match(x, from)
    mapidxNA <- is.na(mapidx)
    from_found <- sort(unique(mapidx))
    if (warn_missing && length(from_found) != length(from)) {
      message(
        "The following `from` values were not present in `x`: ",
        paste(from[!(1:length(from) %in% from_found)], collapse = ", ")
      )
    }
    x[!mapidxNA] <- to[mapidx[!mapidxNA]]
    x
  }

  # revalue
  revalue <- function(x, replace = NULL, warn_missing = TRUE) {
    if (!is.null(x) && !is.factor(x) && !is.character(x)) {
      stop("x is not a factor or a character vector.")
    }
    mapvalues(x, from = names(replace), to = replace, warn_missing = warn_missing)
  }

  # dplyr::rename
  names(x) <- revalue(names(x), replace, warn_missing = warn_missing)
  duplicated_names <- names(x)[duplicated(names(x))]
  if (warn_duplicated && (length(duplicated_names) > 0L)) {
    duplicated_names_message <- paste0("`", duplicated_names,
      "`",
      collapse = ", "
    )
    warning("The plyr::rename operation has created duplicates for the ",
      "following name(s): (", duplicated_names_message,
      ")",
      call. = FALSE
    )
  }
  x
}

# l <- "first"
# k <- "second"
# head(renamer1(iris,  replace = c(1 = k)),2)
# x <- c("a" = 1, "b" = 2, d = 3, 4)
# # Rename column d to "c", updating the variable "x" with the result
# x <- renamer1(x, replace = c("d" = "k"))
# x
# # Rename column "disp" to "displacement"
# renamer1(mtcars, c("disp" = "displacement"))
