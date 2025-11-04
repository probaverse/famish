#' Fit GPD distribution
#'
#' @inheritParams fit_dst_norm
#' @param diagnostics Logical; print out diagnostic plots of the fit?
#' @param ... Unused; included here for extensibility.
#' @return A Generalized Pareto Distribution
#' @details A threshold of zero is used for MLE, and is estimated in "lmom".
#' @export
fit_dst_gp <- function(x, 
					   method = c("mle", "lmom", "mom", "mge"),
					   diagnostics = FALSE,
					   ...) {
  threshold <- 0
  if (length(x) == 0) return(distionary::dst_null())
  method <- rlang::arg_match(method)
  if (method == "mle") {
    m <- min(x, na.rm = TRUE)
    if (threshold > m) {
      return(distionary::dst_null())
    }
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
  if (method == "lmom") {
    params <- lmom::pelgpa(lmom::samlmu(x), bound = 0)
    xi <- -params[[3]]
    if (xi > 0.9) {
      warning("Data may be too heavy-tailed to rely on the method of moments ",
              "(the mean may not exist).")
    }
    return(distionary::dst_gpd(params[[2]], xi))
  }
  stop("That method has not been implemented yet.")
}
