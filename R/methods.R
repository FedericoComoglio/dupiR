#' Print method for `Counts` class
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

#' Summary method for `Counts` class
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
    
    cat("| posterior computed: ", has_posterior, "\n")
  
  }
)

setMethod(
  f = "plot",
  signature = "Counts",
  definition = function(x, ...) {
    if (length(x@posterior) > 0 || x@gamma == TRUE) { # it has been computed
      plotPosterior(x, ...)
    }
    else {
      stop("Slot posterior empty, need to compute posterior density first (see computePosterior).")
    }
  }
)

# Getters
setGeneric(name = "getCounts", def = function(object) standardGeneric("getCounts"))
setMethod(
  f = "getCounts",
  signature = "Counts",
  definition = function(object) {
    return(object@counts)
  }
)

setGeneric(name = "getFractions", def = function(object) standardGeneric("getFractions"))
setMethod(
  f = "getFractions",
  signature = "Counts",
  definition = function(object) {
    return(object@fractions)
  }
)

# Setters
setGeneric(name = "setCounts<-", def = function(object, value) standardGeneric("setCounts<-"))
setReplaceMethod(
  f = "setCounts",
  signature = "Counts",
  definition = function(object, value) {
    object@counts <- as.integer(value)
    validObject(object)
    return(object)
  }
)

setGeneric(name = "setFractions<-", def = function(object, value) standardGeneric("setFractions<-"))
setReplaceMethod(
  f = "setFractions",
  signature = "Counts",
  definition = function(object, value) {
    object@fractions <- value
    validObject(object)
    return(object)
  }
)

setGeneric(name = "computePosterior", def = function(object, n1, n2, replacement = FALSE, b = 1e-10, alg = "DUP") standardGeneric("computePosterior"))
setMethod(
  f = "computePosterior",
  signature = "Counts",
  definition = function(object, n1, n2, replacement = FALSE, b, alg = "DUP") {
    stopifnot(is(object, "Counts"))
    # init vars
    k.vec <- object@counts
    r.vec <- object@fractions
    X <- object@X
    K <- sum(k.vec) # total counts
    R <- sum(r.vec) # total sampling fractions
    # choose support
    if (missing(n1) & missing(n2)) {
      n1 <- object@n1
      n2 <- object@n2
    }
    object@n1 <- n1
    object@n2 <- n2
    switch(alg,
      "DUP" = {
        if (R < 1 / 32) { # compute with replacement, use Clough
          message("Notice: Effect of replacement negligible, used faster algorithm (Gamma approximation).")
          posterior <- Clough(object, n1, n2, b = b)
          object@posterior <- posterior
          object@gamma <- TRUE
          return(object)
        }
        s <- n1:n2
        if (replacement) { # with replacement
          denominator <- normalizeConstant(X, k.vec, n1, n2)
          posterior <- sapply(s, getPwithR, k.vec, X, denominator)
          object@nconst <- denominator
          object@posterior <- posterior
          return(object)
        }
        else { # without replacement
          posterior <- dnbinom(s - K, K + 1, R)
          object@posterior <- posterior
          return(object)
        }
      },
      "GP" = {
        posterior <- Clough(object, n1, n2, b = b)
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
