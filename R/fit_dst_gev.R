library(ismev)
library(rlang)
library(distionary)

#' Fit GEV distribution
#'
#' Fits a GEV distribution via maximum likelihood,
#' with `ismev::gev.fit()`, and returns
#' a distplyr distribution.
#'
#' @inheritParams fit_dst_norm
#' @param diagnostics Logical; print out diagnostic plots of the fit?
#' @return A distplyr distribution.
fit_dst_gev <- function(x, method = c("mle", "lmom", "mom", "mge"),
                        diagnostics = FALSE, ...) {
  method <- match.arg(method)
  if (method == "mle") {
    fit_ismev <- gev.fit(x, show = FALSE)
    if (diagnostics) {
      gev.diag(fit_ismev)
    }
    return(distionary::dst_gev(fit_ismev$mle[1], fit_ismev$mle[2],
                               fit_ismev$mle[3]))
  }
  if (method == "lmom") {
    params <- lmom::pelgev(lmom::samlmu(x))
    distionary::dst_gev(params[[1]], params[[2]], -params[[3]])
  }
  stop("That method has not been implemented yet.")
}
