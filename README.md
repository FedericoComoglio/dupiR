## dupiR: an R package for Bayesian inference using discrete uniform priors 

[![](https://www.r-pkg.org/badges/version/dupiR?color=orange)](https://cran.r-project.org/package=dupiR)
[![](https://img.shields.io/badge/devel%20version-1.2.1-blue.svg)](https://github.com/FedericoComoglio/dupiR)
[![](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![](http://cranlogs.r-pkg.org/badges/grand-total/dupiR?color=blue)](https://cran.r-project.org/package=dupiR)
[![](https://img.shields.io/badge/doi-10.1371/journal.pone.0074388-yellow.svg)](https://doi.org/10.1371/journal.pone.0074388)

This R package implements a Bayesian approach to infer population sizes from count data. The package takes a set of sample counts obtained by sampling fractions of a finite volume containing an homogeneously dispersed population of identical objects and returns the posterior probability distribution of the population size. The algorithm makes use of a binomial likelihood and non-conjugate, discrete uniform priors. `dupiR` can be applied to both sampling with or without replacement.

Further details on the statistical framework can be found in:

- Comoglio F, Fracchia L, Rinaldi M (2013) Bayesian Inference from Count Data Using Discrete Uniform Priors. [PLoS ONE 8(10): e74388](https://journals.plos.org:443/plosone/article?id=10.1371/journal.pone.0074388)

Please cite this article if you are using `dupiR` for your research.

## Installation

You can install the latest package release from
[CRAN](https://cran.r-project.org/package=dupiR):

``` r
install.packages("dupiR")
```

or the development version from
[GitHub](https://github.com/FedericoComoglio/dupiR):

``` r
# install.packages("devtools")
devtools::install_github("FedericoComoglio/dupiR")
```
