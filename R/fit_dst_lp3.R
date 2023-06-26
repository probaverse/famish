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
  dlpearsonIII(x, meanlog = meanlog, sdlog = sdlog, skew = skew)
}

#' @rdname LP3
#' @export
qlpearson3 <- function(p, meanlog, sdlog, skew) {
  qlpearsonIII(p, meanlog = meanlog, sdlog = sdlog, skew = skew)
}

#' @rdname LP3
#' @export
plpearson3 <- function(q, meanlog, sdlog, skew) {
  if (length(q) == 0) return(q)
  if_else(
    q < 0, 0, plpearsonIII(q, meanlog = meanlog, sdlog = sdlog, skew = skew)
  )
}

#' @rdname LP3
#' @export
rlpearson3 <- function(n, meanlog, sdlog, skew) {
  rlpearsonIII(n, meanlog = meanlog, sdlog = sdlog, skew = skew)
}

#' Fit a Log Pearson Type III (LP3) Distribution
#'
#' @inheritParams fit_dst_norm
fit_dst_lp3 <- function(x, method = "mge") {
  if (any(x <= 0)) {
    warning("Cannot fit a Log Pearson III distribution to non-positive data. ",
            "Returning NULL")
    return(NULL)
  }
  if (method != "mge") {
    stop("That method is not implemented yet.")
  }
  logx <- log(x)
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
  distionary::dst_parametric(
    "lpearson3", meanlog = params[["mean"]], sdlog = params[["sd"]],
    skew = params[["skew"]], .variable = "continuous"
  )
}
