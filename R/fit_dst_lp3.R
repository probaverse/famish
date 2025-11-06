#' Fit a Log Pearson Type III (LP3) Distribution
#'
#' @inheritParams fit_dst_norm
#' @export
fit_dst_lp3 <- function(x, method = c("mge", "lmom"), ...) {
  fit_dst(family = "lp3", x = x, method = method, ...)
}

.fit_dst_family_lp3 <- function(x, method, ...) {
  ellipsis::check_dots_empty()
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
      return(distionary::dst_null())
    }
    params <- fit$estimate
    if (any(is.na(params))) {
      return(distionary::dst_null())
    }
    return(dst_lp3(params[["mean"]], params[["sd"]], params[["skew"]]))
  }
  if (method == "lmom") {
    moments <- lmom::pelpe3(lmom::samlmu(logx))
    return(dst_lp3(moments[[1]], moments[[2]], moments[[3]]))
  }
  stop("That method has not been implemented yet.")
}
