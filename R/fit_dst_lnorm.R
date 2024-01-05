library(distionary)

#' Fit a Log-Normal Distribution
#'
#' @inheritParams fit_dst_norm
#' @return A distplyr distribution.
#' @export
fit_dst_lnorm <- function(x, method = c("mle", "lmom", "mom", "mge")) {
  if (length(x) == 0) return(distionary::dst_null())
  method <- rlang::arg_match(method)
  if (method != "mle") {
    stop("That method is not implemented yet.")
  }
  if (any(x <= 0)) {
    warning("Cannot fit a Lognormal distribution to non-positive data. ",
            "Returning NULL")
    return(NULL)
  }
  x <- log(x)
  distionary::dst_lnorm(mean(x), stats::var(x))
}
