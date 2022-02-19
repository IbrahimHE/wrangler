#' @title Delete rows in a dataframe
#' @description This function delete rows in a dataframe
#' @param r specify rows to be deleted
#' @param df specify dataframe
#' @returns returns a dataframe
#' @examples
#' \dontrun{
#' del_rows(c(1, 3), data)
#' del_rows(c("1", "3"), data)
#' }
#' @export
del_rows <- function(r, df) {
  k <- as.numeric(length(unlist(as.numeric(unlist(strsplit(as.character(r), ","))))))
  u <- {
    if (k > 1) {
      r <- as.numeric(unlist(strsplit(as.character(r), ",")))
    } else {
      if (k == 1) {
        if (as.numeric(r) == 0) {
          r <- 0
        } else {
          r <- as.numeric(r)
        }
      } else {
        r <- 0
      }
    }
  }
  df <- {
    if (k > 1) {
      df[-c(u), ]
    } else if (k == 1) {
      df[-c(u), ]
    } else {
      return(df)
    }
  }
  df
}


#' @title Delete columns in a dataframe
#' @description This function delete columns in a dataframe
#' @param c specify columns to be deleted
#' @param df specify dataframe
#' @returns returns a dataframe
#' @examples
#' \dontrun{
#' del_cols(c(1, 3), data)
#' del_cols(c("1", "3"), data)
#' }
#' @export
del_cols <- function(c, df) {
  k <- as.numeric(length(unlist(as.numeric(unlist(strsplit(as.character(c), ","))))))
  u <- {
    if (k > 1) {
      c <- as.numeric(unlist(strsplit(as.character(c), ",")))
    } else {
      if (k == 1) {
        if (as.numeric(c) == 0) {
          c <- 0
        } else {
          c <- as.numeric(c)
        }
      } else {
        c <- 0
      }
    }
  }
  df <- {
    if (k > 1) {
      df[, -c(u)]
    } else if (k == 1) {
      df[, -c(u)]
    } else {
      return(df)
    }
  }
  df
}
