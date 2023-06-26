library(ismev)
library(rlang)
library(distionary)

#' Make dst_gumbel
#' @param location,scale Parameters of the Frechet distribution;
#' scale must be positive.
#' @export
dst_gumbel <- local({
  pgumbel <- function(q, location, scale) distionary::pgev(q, location, scale, shape = 0)
  qgumbel <- function(p, location, scale) distionary::qgev(p, location, scale, shape = 0)
  dgumbel <- function(x, location, scale) distionary::dgev(x, location, scale, shape = 0)
  function(location, scale) distionary::dst_parametric(
    "gumbel", location = location, scale = scale, .variable = "continuous"
  )
})

#' Fit Gumbel distribution
#'
#' Fits a Gumbel distribution via maximum likelihood,
#' with `ismev::gum.fit()`, and returns
#' a distplyr distribution.
#'
#' @inheritParams fit_dst_norm
#' @param diagnostics Logical; print out diagnostic plots of the fit?
#' @return A distplyr distribution.
#' @export
fit_dst_gumbel <- function(x, method = c("mle", "lmom", "mom", "mge"),
                           diagnostics = FALSE) {
  method <- match.arg(method)
  if (method != "mle") {
    stop("That method is not implemented yet.")
  }
  fit_ismev <- ismev::gum.fit(x, show = FALSE)
  if (diagnostics) {
    print(ismev::gum.diag(fit_ismev))
  }
  distionary::dst_gumbel(fit_ismev$mle[1], fit_ismev$mle[2])
}
