#' Genetic Algorithm for Variable Selection in Generalized Linear Models
#'
#' This is an iterative algorithm for maximizing a fitness function evaluated on
#' fitted generalized linear models.
#'
#' @param dat Matrix of variables involved in regression.
#'   nxp matrix of p variables (p-1 covariates and one response),
#'   n observations per variable.
#' @param x Vector of column indices corresponding to covariates in `dat`.
#'   `x` is used to subset `dat` to produce the design matrix for `glm.fit()`.
#' @param y Column index corresponding to response variable in `dat`.
#'   `y` must be a unit length vector, used to subset `dat` to return
#'   response variable for `glm.fit()`.
#' @param glm_family Regression family used in `glm.fit()`.
#'   See `family()` for more information.
#' @param P Size of population.
#'   `P` must be greater than or equal to 2.
#' @param fit_func Fitness function to be maximized by GA.
#'   Default is AIC. If alternative is provided, it must be a function object
#'   where the function takes a `glm` object and returns a score to be
#'   maximized.
#' @param num_iter Number of iterations for GA algorithm.
#'   Must be greater than 1.
#' @param parent_sel Parent selection function for GA.
#'   If an alternative function is provided, it must take as input a vector
#'   of scores and return a vector of indicies of length `2*P` corresponding
#'   to sampled parents.
#' @param genetic_op Genetic operator function for GA.
#'   Default is `recombination` function in package. If alternative function
#'   is provided, it must take in as arguments a `2xP` matrix (each column consisting)
#'   of a pair of indices for two coupled parents) as well as the population matrix
#'   (`length(x)` by `P` matrix, each column consisting of booleans for each of the
#'   `length(x)` variables in the original model). The function must then return
#'   a new population matrix of same dimensions for the next generation.
#' @param mut_prob Mutation probability. Must be between 0 and 1.
#' @param elitism Whether or not to use elitism (guarantees that maximum fitness
#' is non-decreasing over iterations).
#'
#' @return Returns a named list with components `final_gen`, `scores`, `fittest`,
#' `fitness_by_gen` corresponding to the final population matrix, the vector of
#' scores for the final population, the column vector of `final_gen` corresponding
#' to the fittest individual, and a vector containing max fitness by generation (used
#' for plotting convergence).
#'
#' @examples
#' # Try ordinary linear regression
#' data('mtcars')
#' fit <- lm(mpg ~ . -vs -am, data = mtcars)
#'
#' # Generate input matrices
#' x <- model.matrix(fit)[,-1]
#' y <- model.response(fit$model)
#'
#' # Select with rank selection
#' select(cbind(x,y), seq(ncol(x)), ncol(x) + 1, gaussian(link = 'identity'),
#' P = 100, num_iter = 100, mut_prob = .1, elitism = TRUE, parent_sel = rankselection)
#'
#' # Try logistic regression
#' data('titanic')
#' fit <- glm(Survived ~ ., family = binomial(link = 'logit'), data = titanic)
#'
#' # Generate input matrices
#' x <- model.matrix(fit)[,-1]
#' y <- model.response(fit$model)
#'
#' # Select with default tournament selection
#' select(cbind(x,y), seq(ncol(x)), ncol(x) + 1, binomial(link = 'logit'),
#' P = 100, num_iter = 100, mut_prob = .1, elitism = TRUE)

select <- function(dat, x, y, glm_family, P, fit_func="aic", num_iter, parent_sel=tournament,
                   genetic_op=recombination, mut_prob=0, elitism=TRUE) {

  if(missing(dat))  stop("A dataframe must be provided.")
  if(missing(x))  stop("A set of variates must be provided.")
  if(missing(y))  stop("Responses must be provided.")
  if(missing(glm_family)) stop("glm_family must be provided.")
  if(missing(P))  stop("Number of covariates wanted be provided.")
  if(missing(num_iter)) stop("Number of iteration must be provided.")
  if(missing(mut_prob)) stop("Probability to mutate must be provided.")

  if(!is.function(fit_func) & fit_func != "aic") population <- get(fit_func)
  if(!is.function(parent_sel))  parent_sel  <- get(parent_sel)
  if(!is.function(genetic_op))  genetic_op  <- get(genetic_op)

  if(!is.integer(P))  P <- as.integer(P)
  if(P < 2) stop("P must be no less than 2.")
  if(P < 5) warning("The number of covariates wanted is less than 5.")
  if(!is.integer(num_iter))  num_iter <- as.integer(num_iter)
  if(num_iter < 1)  stop("Number of iteration must be no less than 1.")
  if(num_iter < 5)  warning("Number of iteration is less than 5.")
  if(!is.numeric(mut_prob))  stop("Probability to mutate must be a numeric value.")
  if(mut_prob<0 | mut_prob>1) stop("Probability to mutate must be between 0 and 1.")
  if(!is.logical(elitism))  stop("eltism should be TRUE or FALSE.")
  if(!is.function(fit_func) & fit_func != "aic")  stop("Fitness function must be a complete function.")
  if(!is.function(parent_sel))  stop("Parent select function must be a complete function.")
  if(!is.function(genetic_op))  stop("Recombination function must be a complete function.")

  # Randomly initialized population, number of rows equal to number of variables
  # (each row entry of a column consists of booleans for each variable) and
  # number of columns equal to size "P" of population
  pop <- matrix(rbinom(P*length(x), 1, 0.5), nrow=length(x), ncol=P)
  pop <- pop == 1

  # Used to store elite for any given iteration of below loop (if "elitism" param=TRUE)
  current_elite <- list(individual=rep(TRUE, nrow(pop)), fitness=-Inf)

  # Returns vector of max fitnesses per iteration (for graphing convergence)
  convergence_report <- rep(0, num_iter+1)

  # Iterative algorithm (number of iterations specified by user)
  for (i in 1:num_iter) {
    # Looping through each column of boolean matrix "pop" (population), using this to subset
    # variable names from "dat_subset_colnames" to fit GLM for each solution and return
    # fitness score
    scores <- apply(pop, 2, function(bool) {
      x_ind <- which(bool)
      mod <- glm.fit(dat[,x[x_ind]], dat[,y], family=glm_family)

      # Returning score using user-supplied "fit_func" function (default is "-1*aic")
      if (is.character(fit_func)) {   # Default argument is string "aic"
        return(-1*(mod$aic))
      } else {
        # Calling "fit_func" with glm object to score model
        return(fit_func(mod))
      }

    })

    if(!is.numeric(scores)) stop("Fitness function should return a number for score.")
    if(length(scores)!=P) stop("Fitness function should return one value as score.")

    # Using scores to sample (obv. with replacement) 2*P number of parents for P couples
    # and P children ("parent_sel" MUST RETURN 2*P INDICES)
    parent_ind <- parent_sel(scores=scores)

    if(length(parent_ind)!=2*P)stop("Parent select function should return a pair of generated data.")

    # Randomly assigning 2*P sampled parents to P pairs
    # "couples" is a matrix, columns store indices of parents in couple, 2*P/2=P columns
    parent_ind_shuffled <- sample(parent_ind, length(parent_ind), replace=FALSE)
    couples <- matrix(parent_ind_shuffled, nrow=2, ncol=length(parent_ind)/2)

    # Using indices for pairs of couples and population to do genetic operation for
    # creating the next generation
    children <- genetic_op(couple_indices=couples, population=pop)
    #if(dim(children)!=dim(populaition)) stop("Recombination function should return matrix of the same dimension as population.")

    # Mutating children with mutation probability "mut_prob"
    children_mut <- mutated(population=children, prob=mut_prob)

    # Updating current elite
    if (elitism) {
      max_ind <- which.max(scores)
      if (current_elite$fitness < max(scores)) {
        current_elite$individual <- pop[,max_ind]
        current_elite$fitness <- scores[max_ind]
      }
    }

    if(is.infinite(current_elite$fitness))  stop("Fitness is infinite!")

    pop <- children_mut
    convergence_report[i] <- max(scores)
  }

  # Calculating fitness scores for this final generation before returning everything
  scores <- apply(pop, 2, function(bool) {
    x_ind <- which(bool)
    mod <- glm.fit(dat[,x[x_ind]], dat[,y], family=glm_family)

    # Returning score using user-supplied "fit_func" function (default is "-1*aic")
    if (is.character(fit_func)) {   # Default argument is string "aic"
      return(-1*(mod$aic))
    } else {
      # Calling "fit_func" with glm object to score model
      return(fit_func(mod))
    }

  })

  # Including last elite in final result if it has the best fitness (replacing
  # least fit individual)
  if (elitism) {
    max_ind <- which.max(scores)
    if (current_elite$fitness > max(scores)) {
      pop[,which.min(scores)] <- current_elite$individual
      scores[which.min(scores)] <- current_elite$fitness
    }
  }

  convergence_report[num_iter+1] <- max(scores)

  # Returning list with entire final generation, fitness scores, identified
  # fittest individual, and vector of max fitness by generation
  return(list(final_gen=pop, scores=scores, fittest=pop[,which.max(scores)],
              fitness_by_gen=convergence_report))
}
