wrapper_ismev <- function(family, x) {
  checkmate::assert_character(family, len = 1)
  checkmate::assert_numeric(x, any.missing = FALSE)
  if (family == "gev") {
    fit <- ismev::gev.fit(x, show = FALSE)
    theta <- fit[["mle"]]
    return(distionary::dst_gev(theta[1], theta[2], theta[3]))
  }
  if (family == "gp") {
    fit <- ismev::gpd.fit(x, threshold = 0, show = FALSE)
    theta <- fit[["mle"]]
    return(distionary::dst_gp(theta[1], theta[2]))
  }
  if (family == "gumbel") {
    fit <- ismev::gum.fit(x, show = FALSE)
    theta <- fit[["mle"]]
    return(distionary::dst_gev(theta[1], theta[2], 0))
  }
  stop(
    "Fitting by ismev is either not implemented for family '", family,
    "' or not available in the ismev package."
  )
}