#' @title Slice a dataframe column by a seperator
#' @description this function used to slice a column with a separator
#' @param data specify a dataframe object
#' @param column specify the column to be be sliced in quotation marks ""
#' @param sep specify a separator to slice the column text with (",",";","-",..)
#' @param trans TRUE/FALSE whether to return values or trans form it into 1.
#' @param fill specify a value to fill the results NA with
#' @param return_names TRUE/FALSE whether to print the unique column names or not
#' @param replace whether to replace or not
#' @importFrom splitstackshape cSplit
#' @importFrom magrittr "%>%"
#' @importFrom reshape2 melt
#' @importFrom stats na.omit
#' @importFrom stats qt
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @importFrom stats setNames
#' @author Ibrahim H. Elkhidir
slice_col_old <- function(data, column, sep, return_names = NULL, trans = NULL, fill = NULL, replace = FALSE) {
  df <- as.data.frame(eval(parse(text = paste0(quote(data), "$", column))))
  colnames(df) <- column
  h <- data.frame(
    id = seq(1:nrow(df)),
    splitstackshape::cSplit(df, column, sep = sep, type.convert = T)
  )
  names <- unique(na.omit(reshape2::melt(h, id.vars = "id")[, -c(1, 2)]))
  ndf <- data.frame(matrix(nrow = nrow(h), ncol = length(names)))
  colnames(ndf) <- names

  # create new dataframe ndf
  for (i in 1:length(names)) {
    a <- index_val(h, names[i], "row")
    # a <- row_id(h,names[i])
    ndf[, i][a] <- names[i]
  }

  # whether to return unique names ater splitting or not
  if (is.null(return_names) || return_names == F) {
    ndf
  } else {
    writeLines(paste0("Unique values are: \n", chr_string(names, T)))
  }
  ndf

  if (is.null(trans) & is.null(fill)) {
    ndf
  } else if (!is.null(trans) & is.null(fill)) {
    if (trans == T) {
      ndf[!is.na(ndf)] <- 1
    } else {
      ndf
    }
  } else if (is.null(trans) & !is.null(fill)) {
    ndf[is.na(ndf)] <- fill
  } else {
    if (trans == T) {
      ndf[!is.na(ndf)] <- 1
    } else {
      ndf
    }
    ndf[is.na(ndf)] <- fill
  }

  if(isTRUE(trans) && fill == 0){
    ndf <- as.data.frame(lapply(ndf, as.numeric))
  }
  ndf
  if(isTRUE(replace)) {
    ind <- index_col(data,column)
    c1 <- colnames(data)
    c2 <- colnames(ndf)
    df1 <- data[c1[1:(ind-1)]]
    df2 <- data[c1[c(ind+1):max(ncol(data))]]
    data <- cbind(df1,ndf,df2)
    data
  } else {
    # ndf <- cbind(data,ndf)
    # ndf
    ind <- index_col(data,column)
    c1 <- colnames(data)
    c2 <- colnames(ndf)
    df1 <- data[c1[1:(ind)]]
    df2 <- data[c1[c(ind+1):max(ncol(data))]]
    data <- cbind(df1,ndf,df2)
    data
  }
}


#' @title Slice a dataframe column by a seperator
#' @description this function used to slice a column with a separator
#' @param data specify a dataframe object
#' @param split.col specify the column to be be sliced in quotation marks ""
#' @param sep specify a separator to slice the column text with (",",";","-",..)
#' @param mode binary/value whether to return values or transform it into 1.
#' @param fill specify a value to fill the results NA with
#' @param type TRUE/FALSE whether to print the unique column names or not
#' @param drop whether to drop the column specfified or not
#' @param fixed allowing for regular expression
#' @param addcol whether to paste colname on splitted columns
#' @param nearcol whether to put splitted col near the original col
#' @param sort.col whether to sort the the resulting sliced columns or not
#' @param decreasing if sorted, whether to be in descending or ascending
#' @export
#' @importFrom splitstackshape cSplit
#' @importFrom magrittr "%>%"
#' @importFrom reshape2 melt
#' @importFrom stats na.omit
#' @importFrom data.table ":="
#' @importFrom data.table is.data.table
#' @importFrom glue trim
#' @importFrom stats qt
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @importFrom stats setNames
#' @author Ibrahim H. Elkhidir
slice_col <- function (data, split.col, sep = ",", type = NULL,
                       mode = NULL, fill = NA, drop = FALSE,
                       nearcol = TRUE, addcol = F, fixed = TRUE,
                       sort.col = F, decreasing = NULL)
{
  charMat <- function (listOfValues, fill = NA, mode = "binary") {
    len <- length(listOfValues)
    vec <- unlist(listOfValues, use.names = FALSE)
    if (sort.col == T ) {
      if (decreasing == F || is.null(decreasing)) lvl <- sort(unique(vec), decreasing = F)
      else lvl <- sort(unique(vec), decreasing = T)
    }
    else lvl <- unique(vec)
    out <- matrix(fill, nrow = len, ncol = length(lvl), dimnames = list(NULL,
                                                                        lvl))
    i.idx <- rep(seq.int(len), vapply(listOfValues, length,
                                      integer(1L)))
    j.idx <- match(vec, lvl)
    out[cbind(i.idx, j.idx)] <- switch(mode, binary = 1L, value = vec,
                                       stop("'mode' must be 'binary' or 'value'"))
    out
  }
  numMat <- function (listOfValues, fill = NA, mode = "binary") {
    listOfValues <- lapply(listOfValues, as.integer)
    len <- length(listOfValues)
    vec <- unlist(listOfValues, use.names = FALSE)
    slvl <- seq(min(vec, na.rm = TRUE), max(vec, na.rm = TRUE))
    out <- matrix(fill, nrow = len, ncol = length(slvl), dimnames = list(NULL,
                                                                         slvl))
    i.idx <- rep(seq_len(len), vapply(listOfValues, length,
                                      integer(1L)))
    j.idx <- match(vec, slvl)
    out[na.omit(cbind(i.idx, j.idx))] <- switch(mode, binary = 1L,
                                                value = na.omit(vec), stop("'mode' must be 'binary' or 'value'"))
    out
  }
  Names <- function (data, invec) {
    if (!is.numeric(invec))
      invec <- match(invec, names(data))
    names(data)[invec]
  }

  .pad <- function (invec) {
    nchars <- max(nchar(invec))
    sprintf(paste0("%0", nchars, "d"), invec)
  }

  if (is.numeric(split.col)) split.col <- Names(data, split.col)

  if (!is.character(data[[split.col]]))
    a <- as.character(data[[split.col]])
  else a <- data[[split.col]]

  if (is.null(mode)) mode = "binary"

  if(is.null(type)){
  if(class(a) == 'character') type = 'character'
  else if(class(a) == 'numeric') type = 'numeric'
  else stop('Can not determine the type of the variable')
  }

  b <- strsplit(a, sep, fixed = fixed)
  b <- lapply(b, trim)
  temp1 <- switch(type, character = {
    temp1 <- charMat(b, fill = fill, mode = mode)
    if (isTRUE(addcol))
      colnames(temp1) <- paste(split.col, colnames(temp1), sep = "_")
    temp1
  }, numeric = {
    nchars <- max(nchar(unlist(b, use.names = FALSE)))
    temp1 <- numMat(b, fill = fill, mode = mode)
    if(isTRUE(addcol))
      colnames(temp1) <- paste(split.col, .pad(seq_len(ncol(temp1))), sep = "_")
    temp1
  }, stop("'type' must be either 'character' or 'numeric'"))
  if (isTRUE(drop) & isFALSE(nearcol)) {
    # if (is.data.table(data)) {
    #   cbind(data, temp1)[, `:=`((split.col), NULL)][]
    # }
    # else {
    #   cbind(data[other_names(data, split.col)], temp1)
    # }
    ind1 <- wrangler::index_col(data, split.col)-1
    ind2 <- wrangler::index_col(data, split.col)+1
    a = data[][1:(ind1)]
    b = data[][ind2:ncol(data)]
    cbind(a,b,temp1)
  }
  else if(isFALSE(drop) & isTRUE(nearcol)) {
    ind1 <- wrangler::index_col(data, split.col)
    ind2 <- wrangler::index_col(data, split.col)+1
    a = data[][1:(ind1)]
    b = data[][ind2:ncol(data)]
    cbind(a,temp1,b)
  }
  else if(isTRUE(drop) & isTRUE(nearcol)) {
    ind1 <- wrangler::index_col(data, split.col)-1
    ind2 <- wrangler::index_col(data, split.col)+1
    a = data[][1:(ind1)]
    b = data[][ind2:ncol(data)]
    cbind(a,temp1,b)
  }
  else cbind(data, temp1)
}

# k <- slice_col(anime, 'Genres', sep = ', ',fill = 0, nearcol = T)
# k <- cSplit_e(anime, 'Genres', sep = ', ', type = "character", mode = "value",fill = 0)

# library(microbenchmark)
# microbenchmark(slice_col(anime, 'Genres', sep = ', ',fill = 0, trans = T, replace = F))
# microbenchmark(cSplit_e(anime, 'Genres', sep = ', ', type = "character", mode = "value",fill = 0))

