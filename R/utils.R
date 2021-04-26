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
get_normalization_constant <- function(counts, n_start, n_end, f_product) {
  
  compute_sum_terms(counts, n_start, f_product) - compute_sum_terms(counts, n_end + 1, f_product)

}


#' Compute posterior probability with replacement
#' 
#' @inheritParams get_normalization_constant
#' @param n integer for which to compute the posterior
#' @param denominator normalization constant returned by \code{get_normalization_constant}
#' 
get_posterior_with_replacement <- function(n, counts, f_product, denominator) {
  
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
gamma_poisson_clough <- function(object, n_start, n_end, a = 1, b = 1e-10) {
  
  # unpack 
  k_vec <- object@counts
  r_vec <- object@fractions
  
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

  # compute support length
  support_length <- n_end - n_start + 1
  
  # compute posterior if less than 1e5 values to compute
  if(support_length <= 1e5) {
    
    s <- n_start : n_end
    posterior <- dgamma(s, a + sum(k_vec), b + sum(r_vec))
    
    return(posterior)
    
  }
  
  # set flag otherwise
  else {  
    
    object@gamma <- TRUE
    
    return(NULL)
    
  }
}


#' 
#' 
#k=counts, n=total number of objects, x = 1-r, t = an index vector
get_term <- function(counts, n, x, t) {	
  
  # number of measurements
  n_measurements <- length(t)
  
  # cumulative sum of t
  t_cumulative <- cumsum(t)	
  
  # total t
  t_sum <- sum(t)
  
  # compute term of F
	term <- prod(choose(n + c(0, t_cumulative)[-(n_measurements + 1)], counts - t)) * 
	  prod(choose(t_cumulative, t)) * 
	  x ^ (n + t_sum) / 
	  (1 - x) ^ (1 + t_sum)
	
	return(term)
	
}


#' 
#' 
#k=counts, n=total number of objects, x = 1-r
compute_sum_terms <- function(counts, n, x) {
  
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
	sum_terms <- sum(apply(sum_indices, 1, get_term, counts, n = n, x = x))
	
	return(sum_terms)

}
