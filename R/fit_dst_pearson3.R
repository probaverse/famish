#' Pearson Type III distribution quantities
#'
#' p/d/q/r functions for the Pearson Type III distribution.
#' @param x,q Vector of quantiles/magnitudes.
#' @param n Number of observations to draw.
#' @param p Vector of probabilities.
#' @param mean,sd,skew Parameters
#' @rdname pearson3
#' @export
dpearson3 <- function(x, mean, sd, skew) {
  if (length(x) == 0) return(x)
  smwrBase::dpearsonIII(x, mean = mean, sd = sd, skew = skew)
}

#' @rdname pearson3
#' @export
qpearson3 <- function(p, mean, sd, skew) {
  smwrBase::qpearsonIII(p, mean = mean, sd = sd, skew = skew)
}

#' @rdname pearson3
#' @export
ppearson3 <- function(q, mean, sd, skew) {
  if (length(q) == 0) return(q)
  smwrBase::ppearsonIII(q, mean = mean, sd = sd, skew = skew)
}

#' @rdname pearson3
#' @export
rpearson3 <- function(n, mean, sd, skew) {
  smwrBase::rpearsonIII(n, mean = mean, sd = sd, skew = skew)
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
