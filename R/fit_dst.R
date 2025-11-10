#' Fit a distribution
#'
#' Fits a probability distribution by calling a specific `fit_dst_*()`
#' function. Still a quick-and-dirty implementation.
#'
#' @param x Numeric vector to fit the distribution to.
#' @param family Name of the distribution, like `"norm"` or `"gev"`.
#' @param method Estimation method to use, like `"mle"` or `"lmom"`.
#' For now, you'll have to look at the documentation of the specific fitting
#' functions to see examples.
#' @param na_action How to resolve missing or invalid observations in `x`.
#' One of `"null"`, `"drop"`, or `"fail"`.
#' @param on_unres Behaviour when fitting does not resolve to a single
#' distribution. One of `"null"` or `"fail"`.
#' @param ... Other arguments to pass to the specific `fit_dst_*()`
#' function.
#' @return A probability distribution.
#' @examples
#' fit_dst("norm", 1:10, "mle")
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
  if (length(x) == 0) { # Quick win
    return(unresolved())
  }
  if (family == "null") {
    return(distionary::dst_null())
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
