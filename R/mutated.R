#' Mutate
#'
#' Mutate the generated data
#'
#' @param population parents population
#' @param prob probability to mutate
#'
#' @return Mutated data
#' @export
#'
#' @examples
#' mutated(matrix(rep(1,20), nrow = 2), 0.5)
mutated <- function(population, prob){
  n1 <- nrow(population)
  n2 <- ncol(population)
  s <- sample(c(FALSE, TRUE), size = n1*n2, prob = c(prob, 1-prob), replace = TRUE)
  S <- matrix(s, nrow = n1)
  I <- matrix(rep(TRUE, n1*n2), nrow = n1)
  newmutation <- population*S + (I - S)*(I - population)
  return(newmutation == 1)
}
