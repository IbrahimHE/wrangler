#' @title Calculate summary statistic for numerical variables
#' @param data specify a dataframe
#' @param decimal round the values to a number of decimal
#' @examples
#' stat_calc(iris[, 1:4])
#' stat_calc(iris[, 1:4], 3)
#' @export
stat_calc <- function(data, decimal = NULL) {
  dstats <- function(x, na.omit = FALSE) {
    if (na.omit) {
      x <- x[!is.na(x)]
    }
    min <- min(x)
    max <- max(x)
    m <- mean(x)
    median <- median(x)
    n <- length(x)
    s <- sd(x) * sqrt(length(x) - 1) / length(x)
    skew <- (sum((x - m)^3 / s^3) / n) * ((sqrt(n * (n - 1))) / (n - 2))
    akurt <- ((sum((x - m)^4) / n) / ((sum((x - m)^2) / n)^2)) - 3
    kurt <- (n / (n - 1) * (n - 3)) * ((n + 1) * (akurt) + 6)
    stds <- sd(x)
    return(c(n = n, min = min, max = max, mean = m, median = median, stdp = s, stds = stds, skew = skew, kurtosis = kurt))
  }
  df <- as.data.frame(sapply(data, dstats))
  if (is.numeric(decimal)) {
    round(df, decimal)
  } else {
    df
  }
}

# cor.test(iris$Sepal.Length, iris$Sepal.Width, method = c("pearson"), conf.level = 0.95 )
#
# t.test(iris$Species, iris$Sepal.Width, paired = TRUE, conf.level = 0.95, alternative = c("two.sided"), data= wbc)
# anova(iris$Species, iris$Sepal.Width)
