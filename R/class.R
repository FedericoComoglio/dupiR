#' An S4 class to store measurements (count data, sampling fractions),
#' prior support and posterior parameters
#'
#' @slot counts integer vector of counts (required)
#' @slot fractions numeric vector of sampling fractions (required)
#' @slot n_start start of prior support range. If omitted and total \code{counts} greater than zero,
#' computed as 0.5 * \code{mle}, where \code{mle} is the maximum likelihood estimate of the population size
#' @slot n_end end of prior support range. If omitted and total \code{counts} greater than zero,
#' computed as 2 * \code{mle}, where \code{mle} is the maximum likelihood estimate of the population size
#' @slot f_product product of (1-\code{fractions})
#' @slot mle maximum likelihood estimate of the population size (ratio between total counts and total sampling fraction)
#' @slot norm_constant normalization constant
#' @slot posterior numeric vector of posterior probabilities over the prior support
#' @slot map_p maximum of \code{posterior} probability
#' @slot map_index index of prior support corresponding to the maximum a posteriori
#' @slot map maximum a posteriori of population size
#' @slot q_low lower bound of the credible interval
#' @slot q_low_p probability of the lower bound of the credible interval
#' @slot q_low_index index of the prior support corresponding to \code{q_low}
#' @slot q_low_cum_p cumulative posterior probability from \code{n_start} to \code{q_low} (left tail)
#' @slot q_up upper bound of the credible interval
#' @slot q_up_p probability of the upper bound of the credible interval
#' @slot q_up_index index of the prior support corresponding to \code{q_high}
#' @slot q_up_cum_p cumulative posterior probability from \code{q_high} to \code{n_end} (right tail)
#' @slot gamma logical, TRUE if posterior computed using a Gamma approximation
#'
#' @note The \code{posterior} slot contains either the PMF or a logical value used to 
#' compute posterior parameters with a Gamma approximation (see reference for details)
#' @note Lower and upper bounds of the credibile interval are computed at a default
#' confidence level of 95%
#' @note For more details on the normalization constant, see Corollary 1 in reference
#' 
#' @references Comoglio F, Fracchia L and Rinaldi M (2013) 
#' Bayesian inference from count data using discrete uniform priors. 
#' \href{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0074388}{PLoS ONE 8(10): e74388}
#'
#' @author Federico Comoglio
#' 
#' @seealso \link{compute_posterior}, \link{get_posterior_param}
#'
#' @examples 
#' # constructor:
#' # create an object of class 'Counts'
#' new_counts(counts = c(30, 35), fractions = c(0.075, 0.1))
#'
#' # same, using new
#' new("Counts", counts = c(30, 35), fractions = c(0.075, 0.1))
#'
#' @keywords class
#'
#' @exportClass Counts
#'
setClass(

  Class = "Counts",

  representation(
    counts = "integer",
    fractions = "numeric",
    n_start = "numeric",
    n_end = "numeric",
    f_product = "numeric",
    mle = "numeric",
    norm_constant = "numeric",
    posterior = "ANY",
    map_p = "numeric",
    map_index = "numeric",
    map = "numeric",
    q_low = "numeric",
    q_low_p = "numeric",
    q_low_index = "integer",
    q_low_cum_p = "numeric",
    q_up = "numeric",
    q_up_p = "numeric",
    q_up_index = "integer",
    q_up_cum_p = "numeric",
    gamma = "logical"
  ),

  validity = function(object) {

    # number of counts must be same as number of fractions
    if (!(length(object@counts) == length(object@fractions))) {
      stop("The number of measurements does not match the number of fractions")
    }

    # fractions must be strictly positive and <= 1
    if (!(all(object@fractions <= 1 & object@fractions > 0))) {
      stop("At least one fraction not in (0,1]")
    }
    
    # counts must be positive
    if (any(object@counts < 0)) {
      stop("At least one count is negative")
    }

    # pass
    return(TRUE)
  }
)


#' Initialize \code{Counts} class
#'
#' @param .Object an object of class "Counts"
#' @param counts integer vector of counts
#' @param fractions numeric vector of sampling fractions
#' 
setMethod(
  f = "initialize",
  signature = "Counts",
  definition = function(.Object, counts, fractions) {
    
    # if counts not provided, create dummy
    if(missing(counts)) {
      counts <- integer()
    }
    
    # if counts are numeric but not integer, cast to integer
    if(!is.integer(counts) && is.numeric(counts)) {
      counts <- as.integer(counts)
      message("Counts converted to integer")
      
    }
    
    # set counts
    .Object@counts <- counts
    
    # if fractions not provided, create dummy
    if(missing(fractions)) {
      fractions <- numeric()
    }
    
    # set fractions
    .Object@fractions <- fractions
    
    # compute and set product of fractions
    .Object@f_product <- prod(1 - fractions)
    
    # compute and set mle
    .Object@mle <- round(sum(counts) / sum(fractions))
    
    # compute and set prior support
    .Object@n_start <- round(0.5 * .Object@mle)
    .Object@n_end <- ifelse(2 * .Object@mle == 0, round(1 / min(fractions)), 2 * .Object@mle)
    
    # set gamma to default
    .Object@gamma <- FALSE
    
    # validate
    validObject(.Object)
    
    return(.Object)
    
  }
)


#' Constructor for \code{Counts} class
#' 
#' @param counts integer vector of counts
#' @param fractions numeric vector of sampling fractions
#' 
#' @return An object of the \code{Counts} class
#' 
#' @export
#' 
new_counts <- function(counts, fractions) {
    new(Class = "Counts", counts = counts, fractions = fractions)
}