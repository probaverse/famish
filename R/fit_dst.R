#' Fit a distribution
#'
#' Estimation of probability distributions available in the 'distionary'
#' package. Wraps the 'lmom' package when fitting by L-moments, 
#' the 'ismev' package when fitting the GP/GEV/Gumbel by MLE, and the
#' 'fitdistrplus' package for other combinations.
#'
#' @param family Name of the target distribution family, such as `"norm"`,
#'   `"gev"`, `"pois"`, or any other entry recognised by
#'   \code{available_methods()}. Custom families supplied via additional
#'   `fit_dst_*()` helpers are also supported.
#' @param x Numeric vector containing the observations to fit. Missing values
#'   are handled according to `na_action`.
#' @param method Estimation method to use. Valid choices include `"mle"`,
#'   `"mge"`, `"mme"`, `"lmom"`, and `"lmom-log"`. The default is to match
#'   against this set and use the first compatible option for the specified
#'   `family`.
#' @param na_action Strategy for dealing with `NA` or otherwise invalid values
#'   in `x`. `"null"` returns `distionary::dst_null()`, `"drop"` silently removes
#'   missing observations before fitting, and `"fail"` aborts with an error.
#' @param on_unres Behaviour when no distribution can be resolved for the
#'   supplied inputs. `"null"` (default) yields a `distionary::dst_null()`
#'   distribution with a warning, whereas `"fail"` propagates an error.
#'
#' @details
#' The fitting workflow proceeds in the following order:
#' \enumerate{
#'   \item Input validation and missing-value handling governed by `na_action`.
#'   \item A compatibility check via \code{supported_combination()} to warn
#'     about unsupported `family` / `method` pairs.
#'   \item Specialised dispatch for known fast paths, including empirical and
#'     finite distributions, degenerate MLE fits, extreme-value families backed
#'     by `ismev`, and L-moment workflows (including log-L-moment transforms).
#'   \item A final fallback to \code{wrapper_fitdistrplus()} for general-purpose
#'     maximum-likelihood or moment-based estimation.
#' }
#' If every attempt fails, the function either returns `dst_null()` or raises an
#' error depending on `on_unres`.
#'
#' @section Supported combinations:
#' A full list of families and their compatible estimation methods is available
#' via \code{available_methods()}. The helper respects bespoke wrappers
#' contained in `fit_dst-family_wrappers.R` and will automatically pick them up
#' when new families are registered.
#'
#' @section Missing data and unresolved fits:
#' When `na_action` is `"drop"`, the function operates on the subset of `x`
#' without missing values. For empty inputs or combinations that cannot be
#' resolved, `on_unres` determines whether a `distionary::dst_null()` object is
#' returned or an error is thrown. This behaviour is particularly relevant for
#' heavy-tailed families (e.g. `"cauchy"`) where certain methods are known to
#' fail.
#'
#' @return A `distionary::distribution` object encapsulating the fitted model
#'   and its parameters.
#'
#' @seealso \code{available_methods()}, \code{supported_combination()},
#'   `fit_dst_*()` helpers, \code{distionary::dst_null()},
#'   \code{distionary::dst_empirical()}
#'
#' @examples
#' fit_dst("norm", x = 1:10, method = "mle")
#' fit_dst("gev", x = c(1, 4, 3, NA, 5), method = "lmom", na_action = "drop")
#' fit_dst("pois", x = c(1, 4, 3, NA, 5), na_action = "null")
#'
#' # Force an error when an unresolved fit would otherwise return a Null dst.
#' try(fit_dst("cauchy", x = 1:10, method = "lmom", on_unres = "fail"))
#'
#' # Inspect the methods supported for a given family.
#' available_methods()[["gev"]]
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
      res <- try(wrapper_lmom(family = "pearson3", x = log(x)), silent = TRUE)
      if (inherits(res, "try-error")) {
        return(unresolved())
      }
      theta <- distionary::parameters(res)
      location <- theta[["location"]]
      scale <- theta[["scale"]]
      shape <- theta[["shape"]]
      params <- list(
        meanlog = location + scale * shape,
        sdlog = scale * sqrt(shape),
        skew = 2 / sqrt(shape)
      )
      return(distionary::dst_lp3(
        meanlog = params[["meanlog"]],
        sdlog = params[["sdlog"]],
        skew = params[["skew"]]
      ))
    }
    if (family == "lnorm") {
      res <- try(wrapper_lmom(family = "norm", x = log(x)), silent = TRUE)
      if (inherits(res, "try-error")) {
        return(unresolved())
      }
      params <- distionary::parameters(res)
      params <- list(meanlog = params[["mean"]], sdlog = params[["sd"]])
      return(distionary::dst_lnorm(params[[1]], params[[2]]))
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
