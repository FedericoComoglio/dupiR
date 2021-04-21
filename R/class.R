#' An S4 class to represent measurements (count data, sampling fractions)
#' and inferred population sizes
#'
#' @slot counts integer vector of counts (required)
#' @slot fractions numeric vector of sampling fractions (required)
#' @slot n_start start of prior support range. If omitted and total `counts` greater than zero,
#' computed as 0.5 * `mle`, where `mle` is the maximum likelihood estimate of the population size
#' @slot n_end end of prior support range. If omitted and total `counts` greater than zero,
#' computed as 2 * `mle`, where `mle` is the maximum likelihood estimate of the population size
#' @slot f_product product of (1-`fractions`)
#' @slot mle maximum likelihood estimate of the population size (ratio between total counts and total sampling fraction)
#' @slot norm_constant normalization constant
#' @slot posterior numeric vector of posterior probabilities over the prior support
#' @slot map_p maximum of `posterior` probability
#' @slot map_index index of prior support corresponding to the maximum a posteriori
#' @slot map maximum a posteriori of population size
#' @slot q_low lower bound of the credible interval
#' @slot q_low_p probability of the lower bound of the credible interval
#' @slot q_low_index index of the prior support corresponding to `q_low`
#' @slot q_low_cum_p cumulative posterior probability from `n1` to `q_low` (left tail)
#' @slot q_up upper bound of the credible interval
#' @slot q_up_p probability of the upper bound of the credible interval
#' @slot q_up_index index of the prior support corresponding to `q_high`
#' @slot q_up_cum_p cumulative posterior probability from `q_high` to `n2` (right tail)
#' @slot gamma logical, TRUE if posterior computed using a Gamma approximation
#'
#' @note The `posterior` slot contains either the PMF or a logical value used to 
#' compute posterior parameters with a Gamma approximation (see reference for details)
#' @note Lower and upper bounds of the credibile interval are computed at a default
#' confidence level of 95%
#' @note For more details on the normalization constant, see Corollary 1 in reference
#' 
#' @references Comoglio F, Fracchia L and Rinaldi M (2013) 
#' Bayesian inference from count data using discrete uniform priors. 
#' [PLoS ONE 8(10): e74388](http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0074388)
#'
#' @author Federico Comoglio
#'
#' @seealso [dupiR::new_counts()]
#'
#' @examples 
#' constructor:
#' # create an object of class 'Counts'
#' new_counts(counts = c(30, 35), fractions = c(0.075, 0.1))
#'
#' # same, using `new`
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
    qlow_index = "integer",
    q_low_cum_p = "numeric",
    q_up = "numeric",
    q_up_p = "numeric",
    q_up_index = "integer",
    qup_cum = "numeric",
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

    # counts must be integer
    if (!(is.integer(object@counts))) {
      
      stop("Counts are not integer")
      
    }
    
    # counts must be positive
    if (any(object@counts < 0)) {

      stop("At least one count is negative")

    }

    # pass
    return(TRUE)

  }
)


#' Initialize `Counts` class
#'
setMethod(
  f = "initialize",
  signature = "Counts",
  definition = function(.Object, counts, fractions) {
    
    # require counts and fractions
 #   if (not(missing(counts)) & not(missing(fractions))) {
      
      # set counts
      .Object@counts <- counts

      # set fractions
      .Object@fractions <- fractions
      
      # compute product of fractions
      .Object@f_product <- prod(1 - fractions)
      
      # compute mle
      mle <- round(sum(counts) / sum(fractions))
      
      # compute prior support
      n_start <- round(0.5 * mle)
      n_end <- ifelse(2 * mle == 0, round(1 / min(fractions)), 2 * mle)
      
      # set prior support
      .Object@n_start <- n_start
      .Object@n_end <- n_end
      .Object@mle <- mle
      .Object@gamma <- FALSE

      # validate
      validObject(.object)
      
 #   }
    
    return(.object)
    
  }
)


#' Constructor for `Counts` class
#' 
#' @export
#' 
new_counts <- function(counts, fractions) {

  if (not(missing(counts)) & not(missing(fractions))) {

    new(Class = "Counts", counts = counts, fractions = fractions)

  }

}
