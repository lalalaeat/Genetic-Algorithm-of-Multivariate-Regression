#' Tournament Selection
#'
#' Sample from population
#'
#' @param scores The scores of model fitness
#' @param k The tournament size, default is 2
#'
#' @return An array of population indices
#' @export
#'
#' @examples
#' scores <- 1:10
#' tournament(scores)
tournament <- function(scores, k = 2) {
  size <- length(scores)
  if (!is.numeric(k) || k <= 0) stop("Must have a valid tournament size!")
  if (k > size) stop("Can not have tournament size larger than sample size!")

  indices <- numeric(2*size)
  i <- 1
  while (i <= 2*size) {
    pool <- sample(scores, k)
    indices[i] <- match(max(pool), scores) # Pick the numerically largest value
    i <- i + 1
  }
  return(indices)
}
