\name{computePosterior}
\docType{methods}
\alias{computePosterior-methods}
\alias{computePosterior,Counts-method}
\alias{computePosterior}

\title{Compute the posterior probability distribution of the population size}

\description{
Compute the posterior probability distribution of the population size using a discrete uniform prior and a binomial likelihood (DUP method). When applicable, an approximation using a Gamma prior and a Poisson likelihood is used instead (GP method, see Clough et al).
}

\usage{computePosterior(object, n1, n2, replacement = FALSE, b, alg = "DUP")}

\arguments{
  \item{object}{An object of class 'Counts'}
  \item{n1}{Left endpoint of the prior support interval (Optional). If not provided and total counts are not zero, computed using the maximum likelihood estimate (mle) of the population size as 0.5 * mle}
  \item{n2}{Right endpoint of the prior support interval (Optional). If not provided and total counts are not zero, computed using the maximum likelihood estimate (mle) of the population size as 2 * mle}
  \item{replacement}{Whether sampling has been performed with replacement. Default to FALSE.}
  \item{b}{Prior rate parameter of the gamma distribution used to compute the posterior with Clough. Default is 1e-10.}
  \item{alg}{Algorithm to be used to perform computations. Default to DUP.}
}

\section{Methods}{
\describe{

\code{signature(object = "Counts")}}{
an object of class \code{Counts}.
}}

\section{Value}{
Returns an object of class \code{Counts}.
}

\references{
Comoglio F, Fracchia L and Rinaldi M (2013) Bayesian inference from count data using discrete uniform priors. PLOS ONE, to appear

Clough HE et al. (2005) Quantifying Uncertainty Associated with Microbial Count Data: A Bayesian Approach. Biometrics 61: 610-616
}

\seealso{
\code{\link{Counts}, \link{plotPosterior}}
}

\author{
   Federico Comoglio, federico.comoglio@bsse.ethz.ch
}

\examples{
K <- newCounts( counts = c(20,30), fractions = c(0.075, 0.10))

#using default parameters (DUP, sampling without replacement and default prior support)
K.dup <- computePosterior(K)

#using custom prior support (DUP)
K.cust <- computePosterior(K, n1 = 0, n2 = 1e3)

#using a Gamma prior (GP method)
K.gp <- computePosterior(K, alg = 'GP')

#plot the results (compare DUP with GP)
plotPosterior(K.dup, type = 'l', lwd = 3, col = 'blue3', low = 0.025, up = 0.975)
lines(K.gp@posterior, lwd = 3, col = 'red3')

#for sampling with replacement:
computePosterior(K, replacement = TRUE)
}   

\keyword{methods}

   