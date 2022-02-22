#' @title Generate random sample from a random numbers
#' @description This function generates random data from certain parameters: sample size, total generated pool pf data (N),  mean(N), sd(N), seed, minimum and maximum values
#' @param n number of sampled data to be returned from data with size (N), (mean), and (sd)
#' @param N Total numbers to be generated
#' @param mean mean of generated random numbers (N)
#' @param sd standard deviation of generated random numbers (N)
#' @param seed the seed of normal random generation (for reproducibility)
#' @param lower minimum value possible in sampled data
#' @param upper maximum value possible in sampled data
#' @returns returns sampled numbers of length (n)
#' @examples
#' \dontrun{
#' sampler(4, 200, 7, 5)
#' sampler(4, 200, 7, 5, lower = 8)
#' sampler(4, 200, 7, 5, upper = 8, seed = 4)
#' }
#' @export
# function generates random data from certain parameters
sampler <- function(n, N, mean, sd, lower = NULL, upper = NULL, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  gendata <- rnorm(N, mean, sd)
  if (!is.null(lower) & is.null(upper)) {
    gendata <- gendata[gendata >= lower]
  } else if (is.null(lower) & !is.null(upper)) {
    gendata <- gendata[gendata <= upper]
  } else if (!is.null(lower) & !is.null(upper)) {
    gendata <- gendata[gendata >= lower & gendata <= upper]
  } else {
    gendata <- gendata
  }

  if (length(gendata) >= n) {
    return(sample(gendata, n))
  }
  stop(simpleError("Not enough values to sample from. Try increasing N"))
}

# sampler(4, 200, 7, 5, upper = 8)
