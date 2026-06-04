#' Fit a distribution
#'
#' Estimation of probability distributions available in the 'distionary'
#' package. Wraps the 'lmom' package when fitting by L-moments,
#' the 'ismev' package when fitting the GP/GEV/Gumbel by MLE, and the
#' 'fitdistrplus' package for other combinations.
#'
#' @param family Name of the target distribution family, such as `"norm"`,
#'   `"gev"`, `"pois"`. See details. Character vector of length 1.
#' @param x Numeric vector containing the observations to fit.
#' @param method Estimation method to use. Valid choices include `"mle"`,
#'   `"mge"`, `"mme"`, `"lmom"`, and `"lmom-log"`. The default is `"mle"`,
#'   although beware that not all families support the `"mle"` method yet
#'   (pearson3 and lp3).
#' @param na_action Strategy for dealing with `NA` values in `x`.
#'   `"null"` returns a Null distribution (`distionary::dst_null()`);
#'   `"drop"` silently removes missing observations before fitting; and
#'   `"fail"` aborts with an error.
#' @param on_unres Behaviour when no distribution can be resolved for the
#'   supplied inputs. `"null"` (default) yields a Null distribution
#'   (`distionary::dst_null()`) distribution with a warning,
#'   whereas `"fail"` propagates an error.
#'
#' @details
#' The `fit_dst()` function is currently a lightweight fitting wrapper,
#' with pre-specified behaviour for certain `family` / `method` combinations.
#' A full list of families and their compatible estimation methods is available
#' via specific family wrappers, such as `fit_dst_norm()`, `fit_dst_pois()`,
#' etc.
#'
#' Here is how fitting is implemented.
#'
#' \itemize{
#'   \item For `method = "lmom"` and distribution families 'gamma', 'gev', 'gp',
#'     'gumbel', 'lnorm', 'norm', 'pearson3', and 'weibull',
#'     the 'lmom' package is wrapped by first calling
#'     `lmom::samlmu()` on the data vector `x` to calculate the L-moments,
#'     then the relevant `lmom::pel*()` function is called to estimate
#'     parameters.
#'   \item For `method = "lmom"` and distribution families
#'     'exp', 'pois', 'bern', 'geom', 'chisq', and 'unif', the method of
#'     L-moments is manually implemented. All of these families except 'unif'
#'     have a single parameter for which only the mean is needed (and thus is
#'     equivalent to the 'mme' method). The 'unif' family has minimum and
#'     maximum parameter values calculated as `l1 - 3 * l2` and  `l1 + 3 * l2`,
#'     where `l1` and `l2` are the first and second L-moments
#'     (see Hosking, 1990, Table 1).
#'   \item For `method = "lmom-log"`, only the 'lnorm' and 'lp3' families are
#'     supported, otherwise no distribution will be resolved. The method fits
#'     the distributions via the 'lmom' method on the log scale. That is,
#'     'norm' and 'pearson3' distributions are fit on the log of
#'     the data, for which the respective 'lnorm' or 'lp3' distribution is
#'     obtained. For 'lp3', the fitted parameters from `lmom::pelpe3()` are
#'     passed through directly as
#'     `distionary::dst_lp3(meanlog = mu, sdlog = sigma, skew = gamma)`.
#'   \item For `method = "mle"` and distribution families 'gev', 'gp', or
#'     'gumbel', the 'ismev' package is used to fit the distribution by maximum
#'     likelihood estimation. This is done by invoking the functions
#'     `ismev::gev.fit()`, `ismev::gpd.fit()` (with `threshold = 0`),
#'     and `ismev::gum.fit()` (respectively).
#'   \item For `method = "mle"` and distribution family 'bern' and 'degenerate',
#'     the MLE is calculated manually. For 'bern', the parameter
#'     is estimated as the mean of the 0-1 data; for 'degenerate', the
#'     unique data value.
#'   \item For `method = "mme"` and `"lmom"`, the 'cauchy' family fails to fit
#'     because Cauchy distributions don't have finite moments (Feller, 1971).
#'   \item For families 'empirical' and 'finite', the empirical distribution
#'     is fit to the supplied data.
#'   \item For the 'null' family, a Null distribution is returned.
#'   \item For any other combination of `family` and `method`, the
#'     `fitdistrplus::fitdist()` function is called by inserting the data `x`,
#'     the `family` name, and the `method`. Some distributions require
#'     starting values for the parameters. For the families 't',
#'     'f', and 'chisq', this is done by moment matching ('mme'). 
#'     For 'gev', 'gp', and 'gumbel', the MLE is used as
#'     starting values (through `method = "mle"`).
#' }
#'
#' To understand what the distribution families are, see the documentation
#' in the 'distionary' package through the `dst_*()` functions.
#' For example, the 'lp3' family can be found at `?distionary::dst_lp3()`.
#' Note that the Gumbel distribution is not available yet in 'distionary',
#' but is simply the 'gev' family with `shape = 0`.
#'
#' To understand the estimation methods, see the 'lmom' package for the
#' `"lmom"` method. For the `"lmom-log"` method, it is the same as `"lmom"`, but
#' via the log of the data and the corresponding log-transformed distributions.
#' For all others, see the 'fitdistrplus' package documentation.
#'
#' ## Handling of missing or unresolvable data
#'
#' When `na_action = "drop"`, the function operates on the subset of `x`
#' without missing values (via `x <- x[!is.na(x)]`). This takes priority over
#' behaviour indicated in `on_unres`.
#'
#' If fitting fails, a Null distribution is output if `on_unres = "null"` (the
#' default), or an error is thrown if `on_unres = "fail"`. Fitting can fail
#' due to not having enough data, not being able to isolate a single
#' distribution, or various other reasons that would typically otherwise
#' result in an error or `NA` parameters in the wrapped fitting method.
#' @references
#' Hosking, J. R. M. (1990). L-moments: Analysis and estimation of distributions
#' using linear combinations of order statistics. *Journal of the Royal
#' Statistical Society: Series B (Methodological)*, 52(1), 105–124.
#'
#' Feller, W. (1971). *An Introduction to Probability Theory and Its
#' Applications* (Vol. 2, 2nd ed.). Wiley.
#' @returns A distribution object of class "dst" encapsulating the fitted
#' distribution.
#'
#' @seealso `fit_dst_*()` helpers such as [`fit_dst_norm()`].
#'
#' @examples
#' fit_dst("norm", x = 1:10, method = "mle")
#' fit_dst("gev", x = c(1, 4, 3, NA, 5), method = "lmom", na_action = "drop")
#' fit_dst("pois", x = c(1, 4, 3, NA, 5), na_action = "null")
#'
#' # "lnorm" with "lmom-log" shares parameters with "norm" fit by "lmom".
#' fit_dst("lnorm", x = 1:10, method = "lmom-log")
#' fit_dst("norm", x = log(1:10), method = "lmom")
#' @export
fit_dst <- function(family,
                    x,
                    method = c("mle", "mge", "mme", "lmom", "lmom-log"),
                    na_action = c("null", "drop", "fail"),
                    on_unres = c("null", "fail")) {
  checkmate::assert_character(family, len = 1)
  checkmate::assert_numeric(x)
  method <- rlang::arg_match(method)
  na_action <- rlang::arg_match(na_action)
  on_unres <- rlang::arg_match(on_unres)
  
  ## START Failure handling
  ## Step 1: Missing data.
  if (anyNA(x)) {
    if (na_action == "null") {
      return(distionary::dst_null())
    }
    if (na_action == "fail") {
      stop("Missing data encountered and `na_action = 'fail'`.")
    }
    x <- x[!is.na(x)]
  }
  
  ## Step 2: Cannot resolve a single distribution. Define unresolved behaviour.
  if (on_unres == "fail") {
    unresolved <- function() {
      stop("Failed to resolve a distribution.")
    }
  }
  if (on_unres == "null") {
    unresolved <- function() {
      warning(
        "Failed to resolve a distribution. Returning a Null distribution."
      )
      distionary::dst_null()
    }
  }
  
  ## Warn if the method is not supported for the family.
  supported <- supported_combination(family = family, method = method)
  if (!supported) {
    warning(
      paste0(
        "The method '", method, "' is not supported for the family '",
        family, "'. Continuing the fitting attempt regardless."
      )
    )
  }
  
  ## BEGIN special dispatching
  if (family == "null") {
    return(distionary::dst_null())
  }
  if (length(x) == 0) { # Quick win
    return(unresolved())
  }
  if (family %in% c("empirical", "finite")) {
    return(distionary::dst_empirical(x))
  }
  if (family == "degenerate" && method == "mle") {
    x <- unique(x)
    if (length(x) == 1) {
      return(distionary::dst_degenerate(x))
    } else {
      return(unresolved())
    }
  }
  if (family %in% c("gp", "gev", "gumbel") && method == "mle") {
    res <- try(wrapper_ismev(family, x = x), silent = TRUE)
    if (inherits(res, "try-error")) {
      return(unresolved())
    }
    return(res)
  }
  if (family == "cauchy" && method == "mme") {
    return(unresolved())
  }
  if (method == "lmom") {
    res <- try(wrapper_lmom(family, x = x), silent = TRUE)
    if (inherits(res, "try-error")) {
      return(unresolved())
    }
    return(res)
  }
  if (method == "lmom-log") {
    if (family == "lp3") {
      lmom_params <- try(lmom::pelpe3(lmom::samlmu(log(x))), silent = TRUE)
      if (inherits(lmom_params, "try-error")) {
        return(unresolved())
      }
      return(distionary::dst_lp3(
        meanlog = lmom_params[["mu"]],
        sdlog = lmom_params[["sigma"]],
        skew = lmom_params[["gamma"]]
      ))
    }
    if (family == "lnorm") {
      res <- try(wrapper_lmom(family = "norm", x = log(x)), silent = TRUE)
      if (inherits(res, "try-error")) {
        return(unresolved())
      }
      params <- distionary::parameters(res)
      params <- list(meanlog = params[["mean"]], sdlog = params[["sd"]])
      return(distionary::dst_lnorm(
        meanlog = params[["meanlog"]],
        sdlog = params[["sdlog"]]
      ))
    }
    return(unresolved())
  }
  if (method == "mle") {
    ## Sometimes, fitdistrplus returns a distribution for MLE when the MLE
    ## does not exist. This happens when no members of the distribution family
    ## have a support that can accommodate the data.
    ## Here, this failure is triggered before fitdistrplus is called to avoid
    ## such issues.
    consistent_support <- family_supports_data(x = x, family = family)
    if (!is.null(consistent_support) && !consistent_support) {
      return(unresolved())
    }
  }
  ## Default to fitdistrplus wrapping
  res <- try(
    wrapper_fitdistrplus(family = family, x = x, method = method),
    silent = TRUE
  )
  if (inherits(res, "try-error")) {
    return(unresolved())
  }
  res
}
