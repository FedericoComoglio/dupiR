\name{Counts}
\docType{class}
\alias{Counts}
\alias{Counts-class}
\alias{summary,Counts-method}


\title{Class "Counts" -- a container for measurements and dupiR inference results}
\description{Definition of an object of this class requires a set of measurements, i.e. a collection of counts and sampling fractions. Inference of the posterior distribution by dupiR (\code{computePosterior}) and subsequent call to \code{getPosteriorParam} will fill all additional slots.}

\section{Objects from the Class}{
Objects should be created with calls to \code{\link{newCounts}}.
}

\section{Slots}{
  \describe{
    \item{\code{counts}:}{Object of class \code{"integer"}. An integer vector of positive numbers (counts). Mandatory for object initialization.}
    \item{\code{fractions}:}{Object of class \code{"numeric"}. A numeric vector of the corresponding sampling fractions (real numbers in (0,1]). Mandatory for object initialization.}
    \item{\code{n1}:}{Object of class \code{"numeric"}. Left endpoint of the prior support interval. If not provided and total counts are not zero, computed using the maximum likelihood estimate (mle) of the population size as 0.5 * mle.}
    \item{\code{n2}:}{Object of class \code{"numeric"}. Right endpoint of the prior support interval. If not provided and total counts are not zero, computed using the maximum likelihood estimate (mle) of the population size as 2 * mle.}
    \item{\code{X}:}{Object of class \code{"numeric"}. The product of (1 - fractions).}
    \item{\code{mle}:}{Object of class \code{"numeric"}. The maximum likelihood estimate of the population size, computed as K/R, where K is the total counts and R is the total sampling fraction.}
    \item{\code{nconst}:}{Object of class \code{"numeric"}. The normalization constant (see Corollary 1 in the reference for details).}
    \item{\code{posterior}:}{Object of class \code{"ANY"}. A vector of posterior probabities over the prior support. It containes either the PMF or a logical value used to obtain posterior parameters with a Gamma approximation (see reference for details).}
    \item{\code{map.p}:}{Object of class \code{"numeric"}. The maximum posterior probability.}
    \item{\code{map.idx}:}{Object of class \code{"numeric"}. The index of the prior support corresponding to the maximum a posteriori (MAP), i.e. MAP - n1 + 1.}
    \item{\code{map}:}{Object of class \code{"numeric"}. The MAP of the population size.}
    \item{\code{qlow.p}:}{Object of class \code{"numeric"}. The probability of the left endpoint (q1) of the credible interval. Default confidence level 95\%.}
    \item{\code{qlow.idx}:}{Object of class \code{"integer"}. The index of the prior support corresponding to q1.}
    \item{\code{qlow}:}{Object of class \code{"numeric"}. The left endpoint (q1) of the credible interval.}
    \item{\code{qlow.cum}:}{Object of class \code{"numeric"}. The cumulative posterior probability from n1 to q1, i.e. the left tail.}
    \item{\code{qup.p}:}{Object of class \code{"numeric"}. The probability of the right endpoint (q2) of the credible interval. Default confidence level 95\%.}
    \item{\code{qup.idx}:}{Object of class \code{"integer"}. The index of the prior support corresponding to q2.}
    \item{\code{qup}:}{Object of class \code{"numeric"}. The right endpoint (q2) of the credible interval.}
    \item{\code{qup.cum}:}{Object of class \code{"numeric"}. The cumulative posterior probability from q2 to n2, i.e. the right tail.}
    \item{\code{gamma}:}{Object of class \code{"logical"}. TRUE if the posterior was computed using a Gamma approximation (see reference for details).}    
  }
}

\references{
Comoglio F, Fracchia L and Rinaldi M (2013) Bayesian inference from count data using discrete uniform priors. PLOS ONE, to appear
}

\author{
   Federico Comoglio, federico.comoglio@bsse.ethz.ch
}

\seealso{
\code{\link{newCounts}}
}

\examples{
# create an object of class 'Counts' by using new
new('Counts', counts = c(30, 35), fractions = c(0.075, 0.1))

#or by means of the constructor
newCounts(counts = c(30, 35), fractions = c(0.075, 0.1))
}

\keyword{class}