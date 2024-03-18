#' Compute ECDF (empirical cumulative distribution function)
#' 
#' @param posterior numeric vector of posterior probabilities over the prior support
#' 
#' @return numeric vector with empirical cumulative distribution function 
#' (cumulative sum of \code{posterior})
#' 
compute_ecdf <- function(posterior) {
  ecdf <- cumsum(posterior)
  
  return(ecdf)
}


#' Compute normalization constant
#' 
#' @param counts integer vector of counts 
#' @param n_start start of prior support range
#' @param n_end end of prior support range
#' @param f_product product of (1-\code{fractions})
#' 
#' @return normalization constant to compute posterior density
#' 
compute_normalization_constant <- function(counts, n_start, n_end, f_product) {
  compute_sum(counts, n_start, f_product) - compute_sum(counts, n_end + 1, f_product)
}


#' Compute single term (function F, Comoglio et al.)
#' 
#' @inheritParams compute_normalization_constant
#' @param counts integer vector of counts 
#' @param n number of objects
#' @param t index vector
#'
#' @return single term of function F
#'
compute_term <- function(counts, n, f_product, t) {	
  
  # number of measurements
  n_measurements <- length(t)
  
  # cumulative sum of t
  t_cumulative <- cumsum(t)	
  
  # total t
  t_sum <- sum(t)
  
  # compute term of F
  term <- prod(choose(n + c(0, t_cumulative)[-(n_measurements + 1)], counts - t)) * 
    prod(choose(t_cumulative, t)) * 
    f_product ^ (n + t_sum) / 
    (1 - f_product) ^ (1 + t_sum)
  
  return(term)
}


#' Compute sum of terms (function F, Comoglio et al.)
#' 
#' @inheritParams compute_term
#'
#' @return sum of terms in function F
#' 
compute_sum <- function(counts, n, f_product) {
  
  # store indices for summation
  index_list <- list()
  
  # number of measurements
  n_measurements <- length(counts)
  
  # generate indices for summation
  for (i in seq_len(n_measurements)) {
    index_list[[i]] <- 0 : counts[i]
  }
  
  # compute cartesian product
  sum_indices <- expand.grid(index_list)
  
  # compute sum of terms
  sum_terms <- sum(apply(sum_indices, 1, compute_term, 
                         counts = counts, n = n, f_product = f_product))
  
  return(sum_terms)
}


#' Compute posterior probability with replacement
#' 
#' @inheritParams compute_normalization_constant
#' @param n integer for which to compute the posterior
#' @param denominator normalization constant returned by \code{compute_normalization_constant}
#' 
#' @return posterior probability of \code{n}
#' 
#' @seealso \link{compute_normalization_constant}
#' 
compute_posterior_with_replacement <- function(n, counts, f_product, denominator) {
  numerator <- f_product ^ n * prod(choose(n, counts))
  posterior <- numerator / denominator

  return(posterior)
}


#' Compute posterior probability using a Gamma-Poisson model (Clough et al.)
#' 
#' @param object object of class \code{Counts}
#' @param n_start start of prior support range
#' @param n_end end of prior support range
#' @param a prior shape parameter of the gamma distribution used to compute the posterior with Clough. Default to 1
#' @param b prior rate parameter of the gamma distribution used to compute the posterior with Clough. Default to 1e-10
#' 
#' @return vector of posterior probabilities
#' 
#' @note if support range spans more than 100k values, the posterior is not
#' computed
#' 
gamma_poisson_clough <- function(object, n_start, n_end, a = 1, b = 1e-10) {
  
  # unpack 
  counts <- object@counts
  fractions <- object@fractions
  
  # if range start not provided
  if (missing(n_start)) {
    # get it from object
    n_start <- object@n_start
  } else {
    # set range start
    object@n_start <- n_start
  }
  
  # if range end not provided
  if (missing(n_end)) {
    # get it from object
    n_end <- object@n_end
  } else {
    # set range end
    object@n_end <- n_end
  }

  # set flag
  object@gamma <- TRUE
  
  # compute support length
  support_length <- n_end - n_start + 1
  
  # compute posterior if less than 1e5 values to compute
  if(support_length <= 1e5) {
    
    s <- n_start : n_end
    posterior <- dgamma(s, a + sum(counts), b + sum(fractions))
    
    return(posterior)
    
  }
  # otherwise do not compute
  else {  
    return(NULL)
  }
}
