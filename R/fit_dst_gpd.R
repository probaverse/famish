

.fit_dst_family_gpd <- function(x, method) {
  if (method == "mle") {
    m <- min(x, na.rm = TRUE)
    fit_ismev <- suppressWarnings(try(
      ismev::gpd.fit(x, threshold = threshold, show = FALSE),
      silent = TRUE
    ))
    if (inherits(fit_ismev, "try-error")) {
      warning("Distribution failed to fit. Returning a NULL distribution.")
      return(distionary::dst_null())
    }
    if (diagnostics) {
      ismev::gpd.diag(fit_ismev)
    }
    return(distionary::dst_gp(
      location = threshold,
      scale = fit_ismev$mle[1],
      shape = fit_ismev$mle[2])
    )
  }
  stop("That method has not been implemented yet.")
}
