# getters ------------------------------------------------------------

#' Get \code{counts} slot for an object of class \code{Counts}
#' 
#' @param object object of class \code{Counts}
#' 
setGeneric(name = "get_counts", def = function(object) standardGeneric("get_counts"))


#' @describeIn Counts Returns counts from a \code{Counts} object
#' 
#' @export
#' 
setMethod(
  f = "get_counts",
  signature = "Counts",
  definition = function(object) {
    
    return(object@counts)
    
  }
)


#' Get \code{fractions} slot for an object of class \code{Counts}
#' 
#' @inheritParams get_counts
#' 
setGeneric(name = "get_fractions", def = function(object) standardGeneric("get_fractions"))


#' @describeIn Counts Returns fractions from a \code{Counts} object
#' 
#' @export
#' 
setMethod(
  f = "get_fractions",
  signature = "Counts",
  definition = function(object) {
    
    return(object@fractions)
    
  }
)


# setters ------------------------------------------------------------

#' Set \code{counts} slot for an object of class \code{Counts}
#' 
#' @param object object of class \code{Counts}
#' @param value numeric vector of counts
#' 
setGeneric(name = "set_counts<-", def = function(object, value) standardGeneric("set_counts<-"))


#' @describeIn Counts Replaces counts of a \code{Counts} object with the provided values
#' 
#' @export
#' 
setReplaceMethod(
  f = "set_counts",
  signature = "Counts",
  definition = function(object, value) {
    
    # set counts
    object@counts <- as.integer(value)
    
    # validate
    validObject(object)
    
    return(object)
    
  }
)


#' Set \code{fractions} slot for an object of class \code{Counts}
#' 
#' @param object object of class \code{Counts}
#' @param value numeric vector of sampling fractions
#'
setGeneric(name = "set_fractions<-", def = function(object, value) standardGeneric("set_fractions<-"))


#' 
#' @describeIn Counts Replaces fractions of a \code{Counts} object with the provided values
#' 
#' @export
#' 
setReplaceMethod(
  f = "set_fractions",
  signature = "Counts",
  definition = function(object, value) {
    
    # set fractions
    object@fractions <- value
    
    # validate
    validObject(object)
    
    return(object)
    
  }
)


# print and summary ------------------------------------------------------------

#' Print method for \code{Counts} class
#' 
#' @inheritParams get_counts
#' 
#' @export
#' 
setMethod(
  f = "show",
  signature = "Counts",
  definition = function(object) {
    
    cat("# A 'Counts' object", "\n")
    cat("| counts: ", object@counts, "\n")
    cat("| fractions: ", object@fractions, "\n")
    cat("| prior support: ", paste0("[", object@n_start, ":", object@n_end, "]"), "\n")
    
    # if posterior computed
    if (length(object@posterior) > 0) {
      
      cat("| posterior:", head(object@posterior, 3), "...", tail(object@posterior, 3), "\n")
      
    }
    
    if (length(object@map) > 0) {
      
      cat("| MAP: ", object@map, "\n")
    
    }
    
    if (length(object@map_p) > 0) {
      
      cat("| maximum posterior probability: ", object@map_p, "\n")
    
    }
    
    if (length(object@q_low) > 0 && length(object@q_up) > 0) {
      
      cat("| credible interval (", 100 * signif(object@q_up_cum - object@q_low_cum, 3), "% level): ", 
          paste0("[", object@q_low, ":", object@q_up, "]"), "\n", sep = "")
    
    }
    
  }
)


#' Summary method for \code{Counts} class
#' 
#' @inheritParams get_counts
#' @param ... additional parameters affecting the summary produced
#' 
#' @export
#' 
setMethod(
  f = "summary",
  signature = "Counts",
  definition = function(object, ...) {
    
    cat("# A 'Counts' object", "\n")
    cat("| counts: ", object@counts, "\n")
    cat("| fractions: ", object@fractions, "\n")
    cat("| prior support: ", paste0("[", object@n_start, ":", object@n_end, "]"), "\n")
    
    has_posterior <- ifelse(!identical(object@posterior, numeric(0)), TRUE, FALSE)
    
    cat("| posterior available: ", has_posterior, "\n")
  
  }
)


# plot ------------------------------------------------------------

#' Plot method for \code{Counts} class
#' 
#' @param x object of class \code{Counts}
#' @param ... additional parameters to be passed to \link{plot_posterior}
#' 
#' @export
#' 
setMethod(
  f = "plot",
  signature = "Counts",
  definition = function(x, ...) {
    
    # if posterior computed
    if (length(x@posterior) > 0 || x@gamma == TRUE) {
    
      # plot posterior density  
      plot_posterior(x, ...)
    
    }
    else {
      
      stop("No posterior available. Please use `compute_posterior` to compute it")
    
    }
    
  }
)


# posterior ------------------------------------------------------------


#' Compute the posterior probability distribution of the population size 
#' for an object of class \code{Counts}
#' 
#' @description Compute the posterior probability distribution of the population size 
#' using a discrete uniform prior and a binomial likelihood ("dup" algorithm, Comoglio et al.). 
#' An approximation using a Gamma prior and a Poisson likelihood is used when 
#' applicable ("gamma" algorithm) method (see Clough et al. for details)
#' 
#' @inheritParams get_counts
#' @param n_start start of prior support range
#' @param n_end end of prior support range
#' @param value numeric vector of counts
#' @param replacement was sampling performed with replacement? Default to FALSE
#' @param b prior rate parameter of the gamma distribution used to compute the posterior with Clough. Default to 1e-10
#' @param alg algorithm to be used to compute posterior. One of ... . Default to "dup" () 
#' 
#' @return an object of class \code{Counts}
#' 
#' @references Comoglio F, Fracchia L and Rinaldi M (2013) 
#' Bayesian inference from count data using discrete uniform priors. 
#' \href{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0074388}{PLoS ONE 8(10): e74388}
#' 
#' @references Clough HE et al. (2005) 
#' Quantifying Uncertainty Associated with Microbial Count Data: A Bayesian Approach. 
#' \href{https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1541-0420.2005.030903.x}{Biometrics 61: 610-616}
#' 
#' @author Federico Comoglio
#'
#' @examples 
#' counts <- new_counts(counts = c(20,30), fractions = c(0.075, 0.10))
#' 
#' # default parameters ("dup" algorithm, sampling without replacement, default prior support)
#' posterior <- compute_posterior(counts)
#' 
#' # custom prior support ("dup" algorithm)
#' posterior <- compute_posterior(counts, n_start = 0, n_end = 1e3)
#' 
#' # gamma prior ("gamma" algorithm)
#' posterior <- compute_posterior(counts, alg = "gamma")
#' 
#' # sampling with replacement
#' posterior <- compute_posterior(counts, replacement = TRUE)
#' 
setGeneric(name = "compute_posterior", 
           def = function(object, n_start, n_end, replacement = FALSE, b = 1e-10, alg = "dup") {
             
             standardGeneric("compute_posterior")
             
           })
          

#' 
#' @describeIn Counts Compute the posterior probability distribution of the population size
#' 
#' @export
#' 
setMethod(
  f = "compute_posterior",
  signature = "Counts",
  definition = function(object, n_start, n_end, replacement = FALSE, b, alg = "dup") {
    
    # validate input data type
    if(!is(object, "Counts")) {
      
      stop("Input object not of class `Counts`")
      
    }
    
    # validate algorithm key
    valid_alg <- c("dup", "gamma")
    
    if(!alg %in% valid_alg) {
      
      stop("Invalid algorithm name. Please provide one of `dup` or `gamma` to argument `alg`")
      
    }
    
    # unpack
    k_vec <- object@counts
    r_vec <- object@fractions
    X <- object@f_product
    
    # compute total counts and sum of sampling fractions
    K <- sum(k_vec)
    R <- sum(r_vec)
    
    # if range start not provided
    if (missing(n_start)) {
      
      # get it from object
      n_start <- object@n_start
      
    } else {
      
      # set range start
      object@n_start <- n_start
      
    }
    
    # if range end not provided
    if (missing(n_end)) {
      
      # get it from object
      n_end <- object@n_end
      
    } else {
      
      # set range end
      object@n_end <- n_end
      
    }
    
    # compute posterior
    switch(alg,
      "dup" = {
        
        # with replacement, using Clough et al.
        if (R < 1 / 32) { 
          
          message("Effect of replacement negligible, used Gamma approximation")
          posterior <- gamma_poisson_clough(object, n_start, n_end, b = b)
          object@posterior <- posterior
          object@gamma <- TRUE
          
          return(object)
          
        }
        
        s <- n_start : n_end
        
        # with replacement
        if (replacement) { 
          
          denominator <- get_normalization_constant(X, k_vec, n_start, n_end)
          posterior <- sapply(s, getPwithR, k_vec, X, denominator)
          object@nconst <- denominator
          object@posterior <- posterior
          
          return(object)
          
        }
        
        # without replacement
        else {
          
          posterior <- dnbinom(s - K, K + 1, R)
          object@posterior <- posterior
          
          return(object)
        }
      },
      
      # with gamma-poisson, using Clough et al.
      "gamma" = {
        posterior <- gamma_poisson_clough(object, n_start, n_end, b = b)
        object@posterior <- posterior
        object@gamma <- TRUE
        
        return(object)
      }
    )
  }
)











setGeneric(name = "getPosteriorParam", def = function(object, low = 0.025, up = 0.975, ...) standardGeneric("getPosteriorParam"))
setMethod(
  f = "getPosteriorParam",
  signature = "Counts",
  definition = function(object, low = 0.025, up = 0.975, ...) {
    k.vec <- object@counts
    r.vec <- object@fractions
    K <- sum(k.vec)
    R <- sum(r.vec)
    posterior <- object@posterior
    if (!is.null(posterior)) { # not computed with a Gamma
      n1 <- object@n1
      n2 <- object@n2
      s <- n1:n2
      map.idx <- which.max(posterior)
      map.p <- posterior[map.idx]
      map <- s[map.idx]
      ecdf <- getECDF(posterior)
      tmp <- which((ecdf <= low) == TRUE)
      if (length(tmp) == 0) {
        qlow.idx <- as.integer(1)
        qlow.p <- posterior[qlow.idx]
        qlow.cum <- 0
        qlow <- 0
      }
      else {
        qlow.idx <- tmp[length(tmp)]
        qlow.p <- posterior[qlow.idx]
        qlow.cum <- ecdf[qlow.idx]
        qlow <- s[qlow.idx]
      }
      tmp <- which((ecdf >= up) == TRUE)
      qup.idx <- tmp[1]
      qup.p <- posterior[qup.idx]
      qup.cum <- ecdf[qup.idx]
      qup <- s[qup.idx]
    }
    else { # computed with a Gamma
      a <- 1
      b <- 1e-10
      object@gamma <- TRUE
      # quantiles
      qlow <- round(qgamma(low, a + K, b + R))
      qup <- round(qgamma(up, a + K, b + R))
      # update n1,n2
      n1 <- round(0.9 * qlow)
      n2 <- round(1.1 * qup)
      map <- round(K / (R + b))
      map.idx <- ifelse(n1 == 0, map, map - n1 + 1)
      map.p <- dgamma(map, a + K, b + R)
      qlow.idx <- as.integer(ifelse(n1 == 0, qlow, qlow - n1 + 1))
      qlow.p <- dgamma(qlow, a + K, b + R) #
      qlow.cum <- pgamma(qlow, a + K, b + R)
      qup.idx <- as.integer(ifelse(n1 == 0, qup, qup - n1 + 1))
      qup.p <- dgamma(qup, a + K, b + R)
      qup.cum <- pgamma(qup, a + K, b + R)
    }
    object@n1 <- n1
    object@n2 <- n2
    object@map.p <- map.p
    object@map.idx <- map.idx
    object@map <- map
    object@qlow.p <- qlow.p
    object@qlow.idx <- qlow.idx
    object@qlow.cum <- qlow.cum
    object@qlow <- qlow
    object@qup.p <- qup.p
    object@qup.idx <- qup.idx
    object@qup.cum <- qup.cum
    object@qup <- qup
    return(object)
  }
)

setGeneric(name = "plotPosterior", def = function(object, low = 0.025, up = 0.975, xlab, step, ...) standardGeneric("plotPosterior"))
setMethod(
  f = "plotPosterior",
  signature = "Counts",
  definition = function(object, low = 0.025, up = 0.975, xlab, step, ...) {
    stopifnot(is(object, "Counts"))
    k.vec <- object@counts
    r.vec <- object@fractions
    posterior <- object@posterior

    tmp <- getPosteriorParam(object, low, up) # returns an object (tmp)
    n1 <- tmp@n1
    n2 <- tmp@n2
    s <- n1:n2
    a <- 1
    b <- 1e-10
    main.text <- paste("Posterior probability distribution \n ", "K=", sum(k.vec), "; ", "R=", sum(r.vec), sep = "")
    if (!is.null(posterior)) {
      l <- length(s)
      plot(posterior,
        xaxt = "n", pch = 19, cex = 0.5,
        main = main.text, xlab = ifelse(missing(xlab), "n", xlab), ylab = "density", ylim = c(0, 1.05 * max(posterior)), ...
      )
      if (missing(step)) {
        axis(side = 1, at = seq(1, l, by = round(l / 15)), labels = seq(n1, n2, by = round(l / 15)))
      }
      else {
        at <- which(s %% step == 0)
        axis(side = 1, at = at, labels = s[at])
      }
      abline(v = tmp@map.idx, lwd = 1.5, col = "blue3")
      lines(c(tmp@qlow.idx, tmp@qlow.idx), c(0, tmp@qlow.p), lwd = 1.5, lty = 2, col = "gray50")
      lines(c(tmp@qup.idx, tmp@qup.idx), c(0, tmp@qup.p), lwd = 1.5, lty = 2, col = "gray50")
      rect(tmp@qlow.idx, 0, tmp@qup.idx, 1 / 30 * tmp@map.p, col = "gray70")
    }
    else {
      l <- n2 - n1 + 1
      x <- NULL
      curve(dgamma(x, a + sum(k.vec), b + sum(r.vec)),
        from = n1, to = n2,
        xaxt = "n", pch = 19, cex = 0.5,
        main = main.text, xlab = ifelse(missing(xlab), "n", xlab), ylab = "density", ...
      )
      if (missing(step)) {
        axis(side = 1, at = seq(1, l, by = round(l / 15)), labels = seq(n1, n2, by = round(l / 15)))
      }
      else {
        at <- which(s %% step == 0)
        axis(side = 1, at = at, labels = s[at])
      }
      abline(v = tmp@map, lwd = 1.5, col = "blue3")
      lines(c(tmp@qlow, tmp@qlow), c(0, tmp@qlow.p), lwd = 1.5, lty = 2, col = "gray50")
      lines(c(tmp@qup, tmp@qup), c(0, tmp@qup.p), lwd = 1.5, lty = 2, col = "gray50")
      rect(tmp@qlow, 0, tmp@qup, 1 / 30 * tmp@map.p, col = "gray70")
    }
    leg <- legend("topright",
      legend = c(
        paste("MAP: ", tmp@map, ", (p=", signif(tmp@map.p, 3), ")", sep = ""),
        paste("CI: [", s[tmp@qlow.idx], ",", s[tmp@qup.idx], "]", sep = ""),
        paste("CL: ", signif(1 - (signif(tmp@qup.cum, 3) - signif(tmp@qlow.cum, 3)), 3), sep = ""),
        paste("Tails: [", signif(tmp@qlow.cum, 3), ",", 1 - signif(tmp@qup.cum, 3), "]", sep = "")
      ),
      col = c("blue3", NA, NA, "gray50"), lty = c(1, 0, 0, 2), lwd = c(2, 0, 0, 2),
      fill = c(NA, "gray70", "gray70", NA), bty = "n", border = rep("white", 4), plot = TRUE
    )
    # add counts table
    D <- cbind(k.vec, r.vec)
    colnames(D) <- c("Counts", "Fractions")
    rownames(D) <- 1:nrow(D)
    addtable2plot(leg$rect$left + leg$rect$w / 3, tmp@map.p * 0.85,
      xjust = 0, yjust = 0, D, bty = "o",
      display.rownames = FALSE, hlines = FALSE
    )
  }
)
