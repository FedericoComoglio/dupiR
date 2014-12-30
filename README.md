README
======

##dupiR is an R package for Bayesian inference using discrete uniform priors. 

This package implements a Bayesian approach to infer population sizes from count data. The package takes a set of sample counts obtained by sampling fractions of a finite volume containing an homogeneously dispersed population of identical objects and returns the posterior probability distribution of the population size. The algorithm makes use of a binomial likelihood and non-conjugate, discrete uniform priors. dupiR can be applied to both sampling with or without replacement.

I developed dupiR in collaboration with Maurizio Rinaldi (University of Piemonte Orientale). This GitHub repository contains stable and devel versions of the package (current version is 1.2). We welcome external contributions.

Feel free to contact us at:

<federico.comoglio@bsse.ethz.ch>

<maurizio.rinaldi@pharm.unipmn.it>

Further details on the method can be found in:

Comoglio F, Fracchia L, Rinaldi M (2013) Bayesian Inference from Count Data Using Discrete Uniform Priors. PLoS ONE 8(10): e74388. doi:10.1371/journal.pone.0074388

http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0074388