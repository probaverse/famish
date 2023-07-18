#' Fit GEV distribution
#'
#' @inheritParams fit_dst_norm
#' @param diagnostics Logical; print out diagnostic plots of the fit?
#' @return A distplyr distribution.
#' @export
fit_dst_gev <- function(x, method = c("mle", "lmom", "mom", "mge"),
                        diagnostics = FALSE, ...) {
  method <- rlang::arg_match(method)
  if (method == "mle") {
    fit_ismev <- ismev::gev.fit(x, show = FALSE)
    if (diagnostics) {
      ismev::gev.diag(fit_ismev)
    }
    return(distionary::dst_gev(fit_ismev$mle[1], fit_ismev$mle[2],
                               fit_ismev$mle[3]))
  }
  if (method == "lmom") {
    params <- lmom::pelgev(lmom::samlmu(x))
    xi <- -params[[3]]
    if (xi > 0.9) {
      warning("Data may be too heavy-tailed to rely on the method of moments ",
              "(the mean may not exist).")
    }
    return(distionary::dst_gev(params[[1]], params[[2]], xi))
  }
  stop("That method has not been implemented yet.")
}
