#' @title Load movies and myMovieData dataset in global environment
#' @export
#' @import ggplot2movies
#' @examples load_movies_data()
#'
load_movies_data <- function() {
  movies <- ggplot2movies::movies
  d1 <-data.frame(movies[movies$Action==1, c("budget", "Short","year")])
  d1$Type <- "Animation"
  d2 <-data.frame(movies[movies$Animation==1, c("budget", "Short", "year")])
  d2$Type <- "Animation"
  d3 <-data.frame(movies[movies$Comedy==1, c("budget", "Short", "year")])
  d3$Type <- "Comedy"
  d4 <-data.frame(movies[movies$Drama==1, c("budget", "Short", "year")])
  d4$Type <- "Drama"
  d5 <-data.frame(movies[movies$Documentary==1, c("budget", "Short", "year")])
  d5$Type <- "Documentary"
  d6 <-data.frame(movies[movies$Romance==1, c("budget", "Short", "year")])
  d6$Type <- "Romance"

  myMovieData <- rbind(d1, d2, d3, d4, d5, d6)
  names(myMovieData) <- c("Budget", "Short", "Year", "Type" )
  .GlobalEnv$myMovieData <- myMovieData
  .GlobalEnv$movies <- movies
}
