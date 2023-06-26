#' Fit a Normal Distribution
#'
#' @param x Numeric vector from which to fit the distribution.
#' @param method Character; method used to fit the distribution. In the
#' future, this may be allowed to be a fitting function when the estimation
#' method requires specification, like the composite quantile estimator.
#' @return A distplyr distribution.
#' @export
fit_dst_norm <- function(x, method = c("mle", "lmom", "mom", "mge")) {
  method <- match.arg(method)
  if (method == "mle") {
    distionary::dst_norm(mean(x), stats::var(x))
  } else {
    stop("That method is not implemented yet.")
  }
}
