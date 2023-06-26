#' Quantile loss function
#'
#' Evaluates the quantile loss function.
#'
#' @param x Vector of observed values to evaluate.
#' @param tau Quantile level. Vectorized.
#' @return The evaluated loss function for each entry in the input vector(s).
#' @note This function is sometimes also called the "asymmetric absolute
#' deviation function", or the "stick function", or the "check function".
#'
#' This is a lightweight function that does not check its inputs.
#' @export
quantile_loss <- function(x, tau) {
  (tau - (x < 0)) * x
}
