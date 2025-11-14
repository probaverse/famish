#' Fit Distributions by Family
#'
#' Convenience wrappers around `fit_dst()` for each distribution family
#' supported by the package.
#'
#' @param x Numeric vector of observations to fit.
#' @param method Estimation method to use. Available options depend on the
#'   distribution family and are enforced via `rlang::arg_match()`.
#' @param ... Additional arguments passed on to `fit_dst()`.
#'
#' @details
#' Each helper simply forwards to `fit_dst()` with the associated `family`
#' value indicated by the function suffix.
#'
#' Some families do not have a unique fitting method where it is not applicable.
#' These are the 'finite' ones (including 'degenerate' and 'empirical'), and
#' the 'Null' distribution.
#' 
#' ## Missing Combinations
#' 
#' Some combinations of families and methods are not supported,
#' when one might think that they should be. These combinations are:
#' 
#' - 'gp' fit by 'mge'
#' - 'lp3' fit by 'mle' and 'mge'
#' - 'pearson3' fit by 'mle' and 'mge'
#' - 'gev' fit by 'mge'
#' 
#' They are not included because of how the `fitdistrplus::fitdist()` function
#' looks for those distributional representations (i.e., `plp3()`, `dlp3()`,
#' etc.): since we cannot guarantee that it will find the correct ones, these
#' combinations are omitted for safety.
#' 
#' To elaborate, the current version of the wrapped `fitdistrplus::fitdist()`
#' function looks for these representations starting from the
#' `namespace:fitdistrplus` environment. `famish` cannot control what's 
#' found in that search path
#' until the global environment, but that's too late because that's where
#' behaviour becomes conditional on the user's actions. This is not a problem
#' for other distributions because these are found in the `namespace:base`
#' environment before the global environment is reached.
#'
#' @returns A distribution object made by the 'distionary' package.
#' @seealso [fit_dst()]
#' @name fit_dst_family_wrappers
#' @examples
#' # Calls can be quite simple.
#' fit_dst_norm(1:10)
#' fit_dst_gumbel(2:6)
#' 
#' # Still have access to the functionality available through `fit_dst()`
#' x <- c(1, 4, 3, NA, 5)
#' fit_dst_lnorm(x, method = "lmom", na_action = "null")
#' fit_dst_lnorm(x, method = "lmom", na_action = "drop")
#' 
#' # Fitting by l-moments on the log-scale not the same as original scale.
#' fit_dst_lnorm(x, method = "lmom-log", na_action = "drop")
#' @export
fit_dst_bern <- function(x, method = c("mle", "lmom"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "bern", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_beta <- function(x, method = c("mle", "mge", "mme"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "beta", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_cauchy <- function(x, method = c("mle", "mge"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "cauchy", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_chisq <- function(x, method = c("mle", "mge", "lmom"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "chisq", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_degenerate <- function(x, method = "mle", ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "degenerate", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_empirical <- function(x, ...) {
  fit_dst(family = "empirical", x = x, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_exp <- function(x, method = c("mle", "mge", "mme", "lmom"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "exp", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_f <- function(x, method = c("mle", "mge"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "f", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_finite <- function(x, ...) {
  fit_dst(family = "finite", x = x, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_gamma <- function(x, method = c("mle", "mge", "mme", "lmom"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "gamma", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_geom <- function(x, method = c("mle", "mme", "lmom"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "geom", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_gev <- function(x, method = c("mle", "lmom"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "gev", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_gp <- function(x, method = c("mle", "lmom"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "gp", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_gumbel <- function(x, method = c("mle", "lmom"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "gumbel", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_lnorm <- function(x,
                          method = c("mle", "mge", "mme", "lmom", "lmom-log"),
                          ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "lnorm", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_lp3 <- function(x, method = "lmom-log", ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "lp3", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_nbinom <- function(x, method = c("mle", "mme"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "nbinom", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_norm <- function(x, method = c("mle", "mge", "mme", "lmom"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "norm", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_null <- function(x, ...) {
  fit_dst(family = "null", x = x, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_pearson3 <- function(x, method = "lmom", ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "pearson3", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_pois <- function(x, method = c("mle", "mme", "lmom"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "pois", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_t <- function(x, method = c("mle", "mge"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "t", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_unif <- function(x, method = c("mle", "mge", "mme", "lmom"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "unif", x = x, method = method, ...)
}

#' @rdname fit_dst_family_wrappers
#' @export
fit_dst_weibull <- function(x, method = c("mle", "mge", "lmom"), ...) {
  method <- rlang::arg_match(method)
  fit_dst(family = "weibull", x = x, method = method, ...)
}
