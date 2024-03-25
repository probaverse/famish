#' Pearson Type III distribution quantities
#'
#' p/d/q/r functions for the Pearson Type III distribution.
#' @param x,q Vector of quantiles/magnitudes.
#' @param n Number of observations to draw.
#' @param p Vector of probabilities.
#' @param mean,sd,skew Parameters
#' @rdname pearson3
#' @author For now, the function bodies are copied from the USGS smwrBase
#' package because it's not available for newer R versions. This will
#' be changed in the future, definitely before CRAN submission. (These
#' functions will also be moved to somewhere else, like distionary).
#' @export
dpearson3 <- function(x, mean, sd, skew) {
  if (length(x) == 0) return(x)
  # USGS code:
  Nout <- max(length(x), length(mean), length(sd), length(skew))
  x <- rep(x, length.out=Nout); mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- stats::dnorm(x, mean, sd)
  }
  if(all(skeworg == 0))
    return(ret0)
  shape <- 4/skew^2
  rate <- sqrt(shape/sd^2)
  mn <- shape/rate
  x <- ifelse(skew > 0, x + mn - mean, mn - x + mean)
  rets <- stats::dgamma(x, shape, rate)
  if(any(ckskew)) {
    rets[ckskew] <- (ret0[ckskew]*((1e-6)-abs(skeworg[ckskew])) +
                       rets[ckskew]*abs(skeworg[ckskew]))/1e-6
  }
  return(rets)
}

#' @rdname pearson3
#' @export
qpearson3 <- function(p, mean, sd, skew) {
  # USGS code:
  Nout <- max(length(p), length(mean), length(sd), length(skew))
  p <- rep(p, length.out=Nout); mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- stats::qnorm(p)
  }
  shape <- 4/skew^2
  rets <- ifelse(skew > 0, (stats::qgamma(p, shape) - shape)/sqrt(shape),
                 (shape - stats::qgamma(1 - p, shape))/sqrt(shape))
  if(any(ckskew)) {
    rets[ckskew] <- (ret0[ckskew]*((1e-6)-abs(skeworg[ckskew])) +
                       rets[ckskew]*abs(skeworg[ckskew]))/1e-6
  }
  return(rets * sd + mean)
}

#' @rdname pearson3
#' @export
ppearson3 <- function(q, mean, sd, skew) {
  if (length(q) == 0) return(q)
  # USGS code
  Nout <- max(length(q), length(mean), length(sd), length(skew))
  q <- rep(q, length.out=Nout); mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- stats::pnorm(q, mean, sd)
  }
  shape <- 4/skew^2
  rate <- sqrt(shape/sd^2)
  mn <- shape/rate
  q <- ifelse(skew > 0, q + mn - mean, mn + mean - q)
  rets <- stats::pgamma(q, shape, rate)
  ## Adjust for negative skew
  rets <- ifelse(skew > 0, rets, 1-rets)
  ## Adjust for near 0 skew
  if(any(ckskew)) {
    rets[ckskew] <- (ret0[ckskew]*((1e-6)-abs(skeworg[ckskew])) +
                       rets[ckskew]*abs(skeworg[ckskew]))/1e-6
  }
  return(rets)
}

#' @rdname pearson3
#' @export
rpearson3 <- function(n, mean, sd, skew) {
  # USGS code
  Nout <- if(length(n) == 1) n else length(n)
  mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- stats::rnorm(n)
  }
  shape <- 4/skew^2
  rets <- (stats::rgamma(n, shape) - shape)/sqrt(shape)
  rets[skew < 0] <- -rets[skew < 0]
  if(any(ckskew))
    rets[ckskew] <- ret0[ckskew]
  return(rets * sd + mean)
}

#' Pearson Type III distribution
#'
#' @inheritParams dpearson3
#' @export
dst_pearson3 <- function(mean, sd, skew) {
  distionary::dst_parametric("pearson3", mean = mean, sd = sd, skew = skew,
                             .variable = "continuous")
}

#' Fit a Pearson Type III Distribution
#'
#' @inheritParams fit_dst_norm
#' @export
fit_dst_pearson3 <- function(x, method = c("mge", "lmom")) {
  if (length(x) == 0) return(distionary::dst_null())
  method <- rlang::arg_match(method)
  if (method == "mge") {
    mu <- mean(x)
    sd <- stats::sd(x)
    skew <- mean(((x - mu) / sd)^3)
    fit <- suppressWarnings(try(fitdistrplus::fitdist(
      x, distr = "pearson3",
      start = list(mean = mu, sd = sd, skew = skew),
      method = method
    ), silent = TRUE))
    if (inherits(fit, "try-error")) {
      return(distionary::dst_null())
    }
    params <- fit$estimate
    if (any(is.na(params))) {
      return(distionary::dst_null())
    }
    return(distionary::dst_parametric("pearson3",
                                      !!!params, .variable = "continuous"))
  }
  if (method == "lmom") {
    moments <- lmom::pelpe3(lmom::samlmu(x))
    return(dst_pearson3(moments[[1]], moments[[2]], moments[[3]]))
  }
  stop("That method has not been implemented yet.")
}
