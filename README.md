## dupiR: an R package for Bayesian inference using discrete uniform priors 

[![](https://www.r-pkg.org/badges/version/dupiR?color=orange)](https://cran.r-project.org/package=dupiR)
[![](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![](http://cranlogs.r-pkg.org/badges/grand-total/dupiR?color=blue)](https://cran.r-project.org/package=dupiR)
[![](https://img.shields.io/badge/doi-10.1371/journal.pone.0074388-yellow.svg)](https://doi.org/10.1371/journal.pone.0074388)

This R package implements a Bayesian approach to infer population sizes from count data. The package takes a set of sample counts obtained by sampling fractions of a finite volume containing an homogeneously dispersed population of identical objects and returns the posterior probability distribution of the population size. The algorithm makes use of a binomial likelihood and non-conjugate, discrete uniform priors. `dupiR` can be applied to both sampling with or without replacement.

Further details on the statistical framework can be found in:

- Comoglio F, Fracchia L, Rinaldi M (2013) Bayesian Inference from Count Data Using Discrete Uniform Priors. [PLoS ONE 8(10): e74388](http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0074388)

Please cite this article if you are using `dupiR` for your research.
