#' Composite quantile and expectile estimators
#'
#' Configure a composite estimation method for use with [fit_dst()]. These
#' estimators fit a distribution family by matching its quantile function
#' (`cqe()`) or expectile function (`cee()`) to the data over a range of
#' levels, rather than at a single summary like the mean or the likelihood.
#'
#' @param tau_cutoff Lower limit of the level range to fit over; a single
#'   number in `[0, 1)`. The estimator integrates the loss over levels from
#'   `tau_cutoff` to 1, so a larger value concentrates the fit on the upper
#'   tail. Defaults to `0` (the whole range).
#' @param weight A weight function applied across levels, taking a numeric
#'   vector of levels in `(0, 1)` and returning a numeric vector of weights.
#'   Defaults to a constant weight of 1.
#'
#' @details
#' Composite quantile estimation (`cqe()`) chooses the family parameters
#' `theta` that minimise
#'
#' \deqn{\frac{1}{n} \sum_{i=1}^n \int_{\tau_0}^1 w(\tau)\,
#'   \rho_\tau\!\left(y_i - Q(\tau; \theta)\right) d\tau,}
#'
#' where \eqn{\rho_\tau} is the quantile (pinball) loss, \eqn{w} is the weight
#' function, \eqn{\tau_0} is the cutoff, and \eqn{Q(\cdot; \theta)} is the
#' family's quantile function. Composite expectile estimation (`cee()`) is
#' identical except the expectile loss \eqn{\eta_\tau(t) =
#' |\tau - \mathbf{1}(t < 0)|\, t^2} replaces \eqn{\rho_\tau}, and the family's
#' expectile function replaces its quantile function. Where matching quantiles
#' is a tail analogue of matching ranks, matching expectiles is a tail analogue
#' of the method of moments.
#'
#' Pass the result to [fit_dst()] as the `method` argument. The strings
#' `"cqe"` and `"cee"` are shortcuts for `cqe()` and `cee()` with their
#' defaults.
#'
#' Composite expectile estimation requires the family's expectile function,
#' which is only defined for distributions with a finite mean; it is currently
#' supported for continuous families.
#'
#' @returns An estimator object of class `"famish_estimator"` to be passed to
#'   [fit_dst()].
#' @seealso [fit_dst()]
#' @examples
#' set.seed(1)
#' x <- rgamma(100, shape = 2, rate = 1)
#' # Composite quantile estimation over the whole range:
#' fit_dst("gamma", x, method = cqe())
#' # Concentrate the fit on the upper tail:
#' fit_dst("gamma", x, method = cqe(tau_cutoff = 0.8))
#' # Composite expectile estimation:
#' fit_dst("gamma", x, method = cee())
#' @name composite_estimators
#' @rdname composite_estimators
#' @export
cqe <- function(tau_cutoff = 0, weight = function(tau) 1) {
  new_composite_estimator("cqe", tau_cutoff = tau_cutoff, weight = weight)
}

#' @rdname composite_estimators
#' @export
cee <- function(tau_cutoff = 0, weight = function(tau) 1) {
  new_composite_estimator("cee", tau_cutoff = tau_cutoff, weight = weight)
}

# Construct a composite estimator object. `type` is "cqe" or "cee", which
# determines the loss function and the distributional representation to match.
new_composite_estimator <- function(type, tau_cutoff, weight) {
  checkmate::assert_choice(type, c("cqe", "cee"))
  checkmate::assert_number(tau_cutoff, lower = 0, upper = 1)
  checkmate::assert_function(weight)
  if (type == "cqe") {
    representation <- "quantile"
    loss <- function(residual, tau) stick_function(residual, tau)
  } else {
    representation <- "expectile"
    loss <- function(residual, tau) abs(tau - (residual < 0)) * residual^2
  }
  structure(
    list(
      type = type,
      tau_cutoff = tau_cutoff,
      weight = weight,
      representation = representation,
      loss = loss
    ),
    class = c(type, "famish_estimator")
  )
}

# Resolve the `method` argument of `fit_dst()` to an estimator object, or
# return NULL if it names a built-in (string) method.
as_estimator <- function(method) {
  if (inherits(method, "famish_estimator")) {
    return(method)
  }
  if (is.character(method) && length(method) == 1L) {
    if (method == "cqe") {
      return(cqe())
    }
    if (method == "cee") {
      return(cee())
    }
  }
  NULL
}

#' @export
print.famish_estimator <- function(x, ...) {
  full <- c(
    cqe = "Composite Quantile Estimator (CQE)",
    cee = "Composite Expectile Estimator (CEE)"
  )[[x$type]]
  cat(full, "\n")
  cat("  Level range: [", format(x$tau_cutoff), ", 1)\n", sep = "")
  cat("  Matches the", x$representation, "function over this range.\n")
  invisible(x)
}
