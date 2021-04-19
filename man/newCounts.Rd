\name{newCounts}
\alias{newCounts}

\title{Construct an object of class Counts}

\description{
Construct an object of class Counts
}

\usage{newCounts(counts, fractions)}

\arguments{
  \item{counts}{An integer vector of positive numbers (counts). Mandatory for object initialization.}
  \item{fractions}{A numeric vector of the corresponding sampling fractions (real numbers in (0,1]). Mandatory for object initialization.}
}

\section{Value}{
Returns an object of class \code{Counts}.
}

\references{
Comoglio F, Fracchia L and Rinaldi M (2013) Bayesian inference from count data using discrete uniform priors. PLOS ONE, to appear
}

\seealso{
\code{\link{Counts}}
}

\author{
   Federico Comoglio, federico.comoglio@bsse.ethz.ch
}

\examples{
K <- newCounts( counts = c(20,30), fractions = c(0.075, 0.10))
K
#or (a little bit more informative):
summary(K)
}   

\keyword{functions}

   