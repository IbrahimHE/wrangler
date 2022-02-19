#' @title Automatically create summary standard error for plotting error bar in ggplot2
#' @description This function creates summary standard error for plotting error bar in ggplot2
#' @param data specify dataframe
#' @param measurevar specify measure variables for mean, n, sd, se, ci
#' @param groupvars specify grouping variables
#' @param na.rm TRUE/FALSE whether to remove missing values or not
#' @param conf.interval specify confidence interval
#' @param drop TRUE/FALSE specify whether to drop or not
#' @returns returns a dataframe with n, mean, sd, ci, se
#' @examples
#' \dontrun{
#' dsummary <- summarySE(iris,
#'   measurevar = "Petal.Width",
#'   groupvars = c("Species", "Sepal.Length")
#' )
#' dsummary <- summarySE(iris,
#'   measurevar = "Sepal.Length",
#'   groupvars = c("Species", "Petal.Width"), F
#' )
#' ggplot(dsummary, aes(Sepal.Length, Petal.Width)) +
#'   geom_point() +
#'   geom_errorbar(aes(ymin = Petal.Width - se, ymax = Petal.Width + se), width = .03) +
#'   geom_smooth()
#' }
#' @export
#' @importFrom plyr ddply
summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = TRUE,
                      conf.interval = .95, drop = TRUE) {
  if (!isNamespaceLoaded("plyr")) {
    attachNamespace("plyr")
  }

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm) {
      sum(!is.na(x))
    } else {
      length(x)
    }
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars,
    .drop = drop,
    .fun = function(xx, col = measurevar) {
      c(
        N = length2(xx[[col]], na.rm = na.rm),
        mean = mean(xx[[col]], na.rm = na.rm),
        sd = sd(xx[[col]], na.rm = na.rm)
      )
    }
  )

  # remove missing values: added by ibrahim
  if (na.rm) {
    datac <- na.omit(datac)
  } else {
    datac
  }

  # Rename the "mean" column
  # colnames(datac)[4]  <- measurevar
  datac <- renamer(datac, "mean", measurevar)
  # datac <- plyr::rename(datac, c("mean" = measurevar))
  # datac <- dplyr::rename(datac, measurevar = mean)


  datac$se <- datac$sd / sqrt(datac$N) # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval / 2 + .5, datac$N - 1)
  datac$ci <- datac$se * ciMult

  return(datac)
}

# dsummary <- summarySE(iris, measurevar = "Petal.Width",
#                       groupvars = c("Species", "Sepal.Length"))

# l <- as.POSIXlt.POSIXct(Sys.time(), tz = "Africa/Khartoum");l
# l <- as.POSIXlt.POSIXct(Sys.time(), tz = "Europe/London");l
# l <- as.POSIXlt.POSIXct(Sys.time(), tz = "America/Los_Angeles");l
# l <- as.POSIXlt.POSIXct(Sys.time(), tz = "Pacific/Easter");l
# l <- as.POSIXlt(Sys.time(), tz = "Africa/Cairo");l

# as.POSIXct("2021-10-28 20:48:45 GMT", tz = "2021-10-28 20:48:45 GMT")
# m <- strptime(l, format = "%Y-%m-%d %H:%M:%S",tz = "Africa/Khartoum");m
Sys.setenv(TZ = "Africa/Khartoum")
# Sys.unsetenv("TZ")
# Sys.timezone()
# as.POSIXlt.POSIXct(Sys.time())
