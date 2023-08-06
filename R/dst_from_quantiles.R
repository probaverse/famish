#' Select a Distribution from Quantiles
#'
#' Select the distribution that most closely follows the specified quantiles,
#' in terms of maximum likelihood. This is useful if you have a list of
#' quantiles from a distribution, but don't know the parameters; or, if
#' you have elicited quantiles by expert judgement, and want the closest
#' distribution.
#'
#' @param quantiles Vector of quantiles.
#' @param probs Vector of non-exceedance probabilities corresponding to
#' the quantiles.
#' @param data Optional; data frame containing the quantiles and
#' probabilities. If supplied, `quantiles` and `probs` can remain unquoted.
#' @param family Name of the distribution to fit.
#' @param n Number of data points to use in the fitting.
#' @param start Optional; named list of starting parameters used in
#' the fitting procedure.
#' @return A probability distribution.
#' @details
#' Parameters are selected resulting in the largest likelihood by creating
#' a censored dataset. Interval-censored data are created between two
#' consecutive quantiles, and left- and right-censored data are created for
#' the smallest and largest quantiles. A total of `n` censored observations
#' are created.
#'
#' If the `start`ing parameters are not specified, they are found by fitting
#' a distribution as if the quantiles are data.
#'
#' If `NA`s are found, they are removed, along with the corresponding `probs`
#' or `quantile` entry.
#' @examples
#' d1 <- distionary::dst_gev(0, 1, 1)
#' qf <- distionary::enframe_quantile(d1, at = c(0.1, 0.5, 0.65),
#'                                    arg_name = "prob")
#'
#' ## Retrieve the GEV parameters
#' matching <- dst_from_quantiles(quantile, prob, data = qf, family = "gev")
#' distionary::parameters(matching)
#'
#' ## Find the closest Log Pearson Type III distribution having these quantiles
#' closest_lp3 <- dst_from_quantiles(quantile, prob, data = qf, family = "lp3")
#' distionary::parameters(closest_lp3)
#'
#' ## Elicit quantiles from expert judgement, and fit a Pearson Type III
#' judgement <- data.frame(prob = c(0.25, 0.5, 0.75),
#'                         quantile = c(40, 100, 200))
#' fit <- dst_from_quantiles(quantile, prob, data = judgement,
#'                           family = "pearson3")
#' distionary::parameters(fit)
#' @export
dst_from_quantiles <- function(quantiles, probs, data, family, n = 1000,
                               start) {
  if (family %in% c("norm", "lnorm", "gpd")) {
    stop("Sorry, the ", family, " distribution isn't working yet.")
  }
  quantiles <- rlang::enquo(quantiles)
  probs <- rlang::enquo(probs)
  if (missing(data)) data <- NULL
  quantiles <- rlang::eval_tidy(quantiles, data = data)
  probs <- rlang::eval_tidy(probs, data = data)
  qp <- as.data.frame(vctrs::vec_recycle_common(quantiles = quantiles,
                                                probs = probs))
  there <- vctrs::vec_detect_complete(qp)
  qp <- qp[there, , drop = FALSE]
  qp <- qp[order(qp$probs), , drop = FALSE]
  qp <- qp[!duplicated(qp), , drop = FALSE]
  if (any(diff(qp$quantiles) < 0)) {
    stop("Quantiles should be non-decreasing. Functionality for ",
         "non-monotonic quantiles is not available at this time.")
  }
  dups <- duplicated(qp$probs)
  if (any(dups)) {
    stop("At least two probabilities were specified as having different ",
         "quantiles; this is an inconsistent property of probability ",
         "distributions.")
  }
  weights <- diff(c(0, qp$probs, 1))
  each <- weights * n
  quantiles <- qp$quantiles
  low <- rep(c(NA, quantiles), times = each)
  high <- rep(c(quantiles, NA), times = each)
  dat <- data.frame(left = low, right = high)
  if (missing(start)) {
    dst_start <- rlang::exec(paste0("fit_dst_", family), quantiles)
    start <- distionary::parameters(dst_start)
  }
  res <- fitdistrplus::fitdistcens(dat, family, start = start)
  params <- res$estimate
  if (family == "norm") {
    params[["variance"]] <- params[["sd"]]^2
    params <- params[c("mean", "variance")]
  }
  rlang::exec(paste0("dst_", family), !!!params)
}
