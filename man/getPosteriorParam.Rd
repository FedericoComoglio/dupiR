\name{getPosteriorParam}
\docType{methods}
\alias{getPosteriorParam-methods}
\alias{getPosteriorParam,Counts-method}
\alias{getPosteriorParam}

\title{Compute posterior probability distribution parameters}

\description{Obtain statistical parameters from the posterior probability distribution. Particularly, this function computes credible intervals at a given confidence level (default to 95\%).}

\usage{getPosteriorParam(object, low = 0.025, up = 0.975, ...)}

\arguments{
  \item{object}{An object of class 'Counts'}
  \item{low}{The left tail posterior probability}
  \item{up}{1 - the right tail posterior probability}
  \item{...}{Additional arguments, for compatibility with \code{plotPosterior}}
}

\section{Methods}{
\describe{

\code{signature(object = "Counts")}}{
an object of class \code{Counts}.
}}

\value{An object of class Counts.}

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

getPosteriorParam(K.dup)

}   

\keyword{methods}