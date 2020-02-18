#' Rank Selection
#'
#' Sample from scores by rank selection method and get index
#'
#' @param scores parents scores
#' @return A vector of parents index of generated data
#' @export
#'
#' @examples score <- runif(2)
#' rankselection(score)#get 4 samples from score
rankselection <- function(scores){
  require(data.table)
  ord <- frank(scores, ties.method="first")
  n <- length(scores)
  ranks <- 2*ord/n/(n+1)
  newgeneration <- sample(1:n, size = 2*n, prob = ranks, replace = TRUE)#sample from x by rankselection
  return(newgeneration)#return new generation index
}
