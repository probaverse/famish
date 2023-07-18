#' Fit GPD distribution
#'
#' @inheritParams fit_dst_norm
#' @param diagnostics Logical; print out diagnostic plots of the fit?
#' @param threshold Single numeric indicating the left-endpoint of the
#' distribution.
#' @return A distplyr distribution.
#' @export
fit_dst_gpd <- function(x, method = c("mle", "lmom", "mom", "mge"),
                        diagnostics = FALSE, threshold, ...) {
  method <- rlang::arg_match(method)
  if (method == "mle") {
    if (missing(threshold)) {
      stop("`threshold` parameter is required when fitting via MLE.")
    }
    m <- min(x, na.rm = TRUE)
    if (threshold > m) {
      stop(glue("There are data below the specified GPD threshold of ",
                "{threshold}. Either set the threshold at {m} or below, ",
                "or truncate the data below {threshold}."))
    }
    fit_ismev <- ismev::gpd.fit(x, threshold = threshold, show = FALSE)
    if (diagnostics) {
      ismev::gpd.diag(fit_ismev)
    }
    return(distionary::dst_gpd(threshold, fit_ismev$mle[1], fit_ismev$mle[2]))
  }
  if (method == "lmom") {
    if (!missing(threshold)) {
      stop("Sorry, but the current implementation of l-moments requires the ",
           "threshold to not be specified (it will be estimated).")
    }
    params <- lmom::pelgpa(lmom::samlmu(x))
    xi <- -params[[3]]
    if (xi > 0.9) {
      warning("Data may be too heavy-tailed to rely on the method of moments ",
              "(the mean may not exist).")
    }
    return(distionary::dst_gpd(params[[1]], params[[2]], xi))
  }
  stop("That method has not been implemented yet.")
}
