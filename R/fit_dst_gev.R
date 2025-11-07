
.fit_dst_family_gev <- function(x, method) {
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
