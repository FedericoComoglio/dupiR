\name{plotPosterior}
\docType{methods}
\alias{plotPosterior-methods}
\alias{plotPosterior,Counts-method}
\alias{plotPosterior}
\alias{plot-method}
\alias{plot,Counts-method}

\title{Plot posterior probability distributions}

\description{
Produces publication-level plots of posterior probability distributions computed using \code{computePosterior}. A data summary, credible intervals at a given confidence level, maximum a posteriori (and more) are indicated.
}

\usage{plotPosterior(object, low = 0.025, up = 0.975, xlab, step, ...)}

\arguments{
  \item{object}{An object of class 'Counts'}
  \item{low}{The left tail posterior probability. Default to 0.025.}
  \item{up}{1 - the right tail posterior probability. Default to 0.975.}
  \item{xlab}{The x-axis label. If not provided, default to 'n'.}
  \item{step}{An integer defining the increment for x-axis labels (i.e. the separation between two consecutive tick marks).}
  \item{...}{Additional arguments to be passed to the \code{plot} function}
}

\section{Methods}{
\describe{\code{signature(object = "Counts")}{
an object of class Counts.
%%  ~~describe this method here~~
}
}}

\value{Called for its effects.}

\seealso{
\code{\link{Counts-class}, \link{computePosterior}}
}

\author{
   Federico Comoglio, federico.comoglio@bsse.ethz.ch
}

\examples{
K <- newCounts( counts = c(20,30), fractions = c(0.075, 0.10))

#using default parameters (DUP, sampling without replacement and default prior support)
K.dup <- computePosterior(K)

plotPosterior(K.dup, type = 'l', lwd = 3, col = 'blue3', low = 0.025, up = 0.975)
}   

\keyword{methods}

   