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
                    method = c("mle", "lmom", "mme", "qme", "mge", "mse"),
                    na_action = c("null", "drop", "fail"),
                    on_unres = c("null", "fail"),
                    ...) {
  checkmate::assert_character(family, len = 1)
  checkmate::assert_numeric(x)
  method <- rlang::arg_match(method)
  na_action <- rlang::arg_match(na_action)
  on_unres <- rlang::arg_match(on_unres)
  
  ## START Failure handling
  ## Step 1: missing data.
  if (anyNA(x)) {
    if (na_action == "null") {
      return(distionary::dst_null())
    }
    if (na_action == "fail") {
      stop("Missing data encountered and `na_action = 'fail'`.")
    }
    x <- x[!is.na(x)]
  }
  
  ## Failure handling, Step 2: cannot resolve a single distribution.
  ## -> Define unresolved behaviour.
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
  ## -> Start with an early exit for empty data; otherwise, failure to resolve
  ##    a single distribution will be handled after the fitting attempt.
  if (length(x) == 0) {
    return(unresolved())
  }
  ## -> Then check that distributions exist having support that can
  ##    accommodate the data `x`.
  consistent_support <- x_consistent_with_support(x = x, family = family)
  if (!consistent_support) {
    return(unresolved())
  }
  ## END special failure handling (remainder occurs after fitting attempt).
  
  ## BEGIN special dispatching
  ## -> Null distribution is easy.
  if (family == "null") {
    return(distionary::dst_null())
  }
  ## -> Finite and degenerate distributions
  if (family %in% c("empirical", "finite")) {
    return(distionary::dst_empirical(x))
  }
  if (family == "degenerate") {
    x <- unique(x)  # Already must be of length 1 due to support check.
    return(distionary::dst_degenerate(x))
  }
  ## -> Use of `lmom` package.
  if (method == "lmom") {
    return(fit_dst_by_lmom(family, x = x))
  }
  ## Dispatch special distribution-method combinations.
  special_distributions <- list(
    gev = "mle",
    gp = "mle"
  )
  case_is_special <- family %in% names(special_distributions) && 
    method %in% special_distributions[["family"]]
  if (case_is_special) {
    fit_fun_name <- paste0(".fit_dst_", family, "_", method)
    # if (length(dots)) {
    #   allowed_args <- names(formals(fit_fun))
    #   unknown_args <- setdiff(names(dots), c(allowed_args, ""))
    #   if (length(unknown_args)) {
    #     stop(
    #       sprintf(
    #         "Not all dots are accepted downstream: %s",
    #         paste(unknown_args, collapse = ", ")
    #       ),
    #       call. = FALSE
    #     )
    #   }
    # }
    fit <- try(rlang::exec(fit_fun_name, x, method = method), silent = TRUE)
    if (inherits(fit, "try-error")) {
      warning("Fitting algorithm failed. Returning a Null distribution.")
      return(distionary::dst_null())
    }
    return(fit)
  }
  ## END special dispatching
  
  ## fitdistrplus wrapping
  res <- try(
    fitdistrplus::fitdist(x, distr = family, method = method),
    silent = TRUE
  )
  if (inherits(res, "try-error")) {
    return(unresolved())
  }
  dst_fun <- paste0("distionary::dst_", family)
  params <- fitdistrplus_params_to_distionary(family, res$estimate)
  rlang::exec(dst_fun, !!!params)
}
