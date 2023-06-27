#' Quantile score function
#'
#' Evaluates the quantile score for a given observation against a quantile
#' estimate.
#'
#' @param x Vector of observed data values.
#' @param xhat Vector of estimated quantiles.
#' @param tau Quantile level (non-exceedance probability).
#' @return The evaluated quantile score for each entry in the input vector(s).
#' @note The quantile score is based on a loss function that goes by many
#' names -- the "asymmetric absolute
#' deviation function", the "stick function", the "check function",
#' or the "pinball loss".
#' @export
quantile_score <- function(x, xhat, tau) {
  z <- vctrs::vec_recycle_common(x, xhat, tau)
  stick_function(z[[1]] - z[[2]], z[[3]])
}

#' Quantile loss function
#'
#' @param x Vector of values to evaluate the loss function at.
#' @param tau Vector of non-exceedance probabilities.
stick_function <- function(x, tau) {
  (tau - (x < 0)) * x
}
