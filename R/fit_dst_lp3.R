#' LP3 distribution quantities
#'
#' p/d/q/r functions for the Log Pearson Type III distribution.
#' @param x,q Vector of quantiles/magnitudes.
#' @param n Number of observations to draw.
#' @param p Vector of probabilities.
#' @param mean,sd,skew Parameters
#' @rdname LP3
#' @export
dlpearson3 <- function(x, meanlog, sdlog, skew) {
  smwrBase::dlpearsonIII(x, meanlog = meanlog, sdlog = sdlog, skew = skew)
}

#' @rdname LP3
#' @export
qlpearson3 <- function(p, meanlog, sdlog, skew) {
  smwrBase::qlpearsonIII(p, meanlog = meanlog, sdlog = sdlog, skew = skew)
}

#' @rdname LP3
#' @export
plpearson3 <- function(q, meanlog, sdlog, skew) {
  if (length(q) == 0) return(q)
  if_else(
    q < 0, 0, smwrBase::plpearsonIII(q, meanlog = meanlog, sdlog = sdlog, skew = skew)
  )
}

#' @rdname LP3
#' @export
rlpearson3 <- function(n, meanlog, sdlog, skew) {
  smwrBase::rlpearsonIII(n, meanlog = meanlog, sdlog = sdlog, skew = skew)
}

#' Log Pearson Type III distribution
#'
#' @inheritParams dlpearson3
#' @export
dst_lp3 <- function(meanlog, sdlog, skew) {
  distionary::dst_parametric("lpearson3", meanlog = meanlog, sdlog = sdlog,
                             skew = skew, .variable = "continuous")
}

#' Fit a Log Pearson Type III (LP3) Distribution
#'
#' @inheritParams fit_dst_norm
#' @export
fit_dst_lp3 <- function(x, method = c("mge", "lmom")) {
  method <- rlang::arg_match(method)
  logx <- log(x)
  if (method == "mge") {
    if (any(x <= 0)) {
      warning("Cannot fit a Log Pearson III distribution to non-positive data. ",
              "Returning NULL")
      return(NULL)
    }
    if (method != "mge") {
      stop("That method is not implemented yet.")
    }
    mu <- mean(logx)
    sd <- stats::sd(logx)
    skew <- mean(((logx - mu) / sd)^3)
    fit <- suppressWarnings(try(fitdistrplus::fitdist(
      logx, distr = "pearson3",
      start = list(mean = mu, sd = sd, skew = skew),
      method = method
    ), silent = TRUE))
    if (inherits(fit, "try-error")) {
      warning("The fitdist function threw an error. Returning NULL.")
      return(NULL)
    }
    params <- fit$estimate
    if (any(is.na(params))) {
      warning("The fitdist function resulted in NA parameters. Returning NULL.")
      return(NULL)
    }
    return(dst_lp3(params[["mean"]], params[["sd"]], params[["skew"]]))
  }
  if (method == "lmom") {
    moments <- lmom::pelpe3(lmom::samlmu(logx))
    return(dst_lp3(moments[[1]], moments[[2]], moments[[3]]))
  }
  stop("That method has not been implemented yet.")
}
