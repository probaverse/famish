#' Fit GEV distribution
#'
#' @inheritParams fit_dst_norm
#' @param diagnostics Logical; print out diagnostic plots of the fit?
#' @param ... Unused; included here for extensibility.
#' @return A distplyr distribution.
#' @export
fit_dst_gev <- function(x, method = c("mle", "lmom", "mom", "mge"),
                        diagnostics = FALSE, ...) {
  ellipsis::check_dots_empty()
  if (length(x) == 0) return(distionary::dst_null())
  method <- rlang::arg_match(method)
  if (method == "mle") {
    fit_ismev <- suppressWarnings(try(
      ismev::gev.fit(x, show = FALSE),
      silent = TRUE
    ))
    if (inherits(fit_ismev, "try-error")) {
      warning("Distribution failed to fit. Returning a NULL distribution.")
      return(distionary::dst_null())
    }
    if (diagnostics) {
      ismev::gev.diag(fit_ismev)
    }
    return(distionary::dst_gev(fit_ismev$mle[1], fit_ismev$mle[2],
                               fit_ismev$mle[3]))
  }
  if (method == "lmom") {
    params <- suppressWarnings(try(
      lmom::pelgev(lmom::samlmu(x)),
      silent = TRUE
    ))
    if (inherits(params, "try-error")) {
      warning("Distribution failed to fit. Returning a NULL distribution.")
      return(distionary::dst_null())
    }
    xi <- -params[[3]]
    if (xi > 0.9) {
      warning("Data may be too heavy-tailed to rely on the method of moments ",
              "(the mean may not exist).")
    }
    return(distionary::dst_gev(params[[1]], params[[2]], xi))
  }
  stop("That method has not been implemented yet.")
}
