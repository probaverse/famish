#' Quantile score (pinball loss)
#'
#' Computes the asymmetric absolute loss commonly used to assess quantile
#' forecasts. Lower scores indicate a better match between the estimated
#' quantile and the observed value at level `tau`.
#'
#' @param x Numeric vector of observed values.
#' @param xhat Numeric vector of estimated quantiles.
#' @param tau Numeric vector of quantile levels in `(0, 1)`.
#'
#' @return Numeric vector of quantile scores corresponding to each element of
#'   the recycled inputs.
#'
#' @details
#' The score minimises to zero when the observation equals the estimated
#' quantile, so that smaller scores indicate a better fitting model.
#' Positive residuals are penalised by a factor of `tau`, and negative
#' residuals by `tau - 1`. This loss is also known as the stick function, check
#' loss, asymmetric absolute deviation, or pinball loss.
#'
#' For observation `x`, estimate `x_hat`, and level `tau`, the score
#' (c.f. Gneiting, 2011) is
#'
#' \deqn{
#' S_\tau(x, \hat{x}) =
#' \begin{cases}
#'   \tau |x - \hat{x})|, & x \ge \hat{x}, \\
#'   (1 - \tau)|x - \hat{x}|, & x < \hat{x}.
#' \end{cases}
#' }
#' 
#' Vector recycling of all three arguments follows the rules in
#' `vctrs::vec_recycle_common()`.
#'
#' @references
#' Gneiting, T. (2011). Making and evaluating point forecasts. *Journal of the
#' American Statistical Association*, 106(494), 746–762.
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