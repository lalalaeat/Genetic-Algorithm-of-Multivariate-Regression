recombination <- function(couple_indices, population) {
  # Function produces "ncol(population)" # of children by recombining the genes of
  # each pair of parents (binary-split) in "couple_indices" to produce one child. 
  # "couple_indices" must be a 2x(# of couples) matrix of indices (one pair per column).
  # "population" must be a matrix, with number of rows equal to number of variables and number
  # of columns equal to number of individuals in the population
  
  # Randomly generating locations for gene split for each of the "ncol(population)" # of
  # sampled couples (splitting ABOVE index, hence starting at 2)
  split_indices <- sample(2:nrow(population), ncol(population), replace=TRUE)
  
  children <- matrix(rep(FALSE, length(population)), nrow=nrow(population), ncol=ncol(population))
  
  for (j in 1:ncol(population)) {
    temp_couple <- couple_indices[,j]
    temp_partnerA <- population[,temp_couple[1]]
    temp_partnerB <- population[,temp_couple[2]]
    
    split_index <- split_indices[j]
    children[,j] <- c(temp_partnerA[1:split_index-1], 
                      temp_partnerB[split_index:length(temp_partnerB)])
  }
  
  return(children)
}
