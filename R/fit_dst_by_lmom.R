# Internal function for fitting a distribution family by L-moments.
# `x` is expected to not have NA.
fit_dst_by_lmom <- function(family, x) {
  checkmate::assert_character(family, len = 1)
  checkmate::assert_numeric(x, any.missing = FALSE)
  sam <- lmom::samlmu(x)
  if (family == "exp") {
    return(distionary::dst_exp(1 / sam[[1]]))
  }
  if (family == "pois") {
    return(distionary::dst_pois(sam[[1]]))
  }
  if (family == "bern") {
    return(distionary::dst_bern(sam[[1]]))
  }
  if (family == "geom") {
    prob <- 1 / (sam[[1]] + 1)
    return(distionary::dst_geom(prob))
  }
  if (family == "chisq") {
    return(distionary::dst_chisq(sam[[1]]))
  }
  if (family == "unif") {
    mu <- sam[[1]]
    l2 <- sam[[2]]
    a <- mu - 3 * l2
    b <- mu + 3 * l2
    return(distionary::dst_unif(a, b))
  }
  if (family == "cauchy") {
    stop(
      "The mean of a Cauchy distribution does not exist; cannot fit by ",
      "L-moments."
    )
  }
  mapping <- list(
    gamma = list(
      name = "pelgam",
      args = list(),
      param_map = unname
    ),
    gev = list(
      name = "pelgev",
      args = list(),
      param_map = function(p) list(p[[1]], p[[2]], -p[[3]])
    ),
    gp = list(
      name = "pelgpa",
      args = list(bound = 0),
      param_map = function(p) list(p[[2]], -p[[3]])
    ),
    lnorm = list(
      name = "peln3",
      args = list(bound = 0),
      param_map = function(p) list(p[[2]], p[[3]])
    ),
    norm = list(
      name = "pelnor",
      args = list(),
      param_map = unname
    ),
    pearson3 = list(
      name = "pelpe3",
      args = list(),
      param_map = unname
    ),
    weibull = list(
      name = "pelwei",
      args = list(bound = 0),
      param_map = function(p) list(p[[2]], p[[3]])
    )
  )
  fam_lmom <- mapping[[family]]
  if (is.null(fam_lmom)) {
    stop("Fitting by L-moments not implemented for family '", family, "'.")
  }
  lmom_params <- rlang::exec(fam_lmom$name, sam, !!!fam_lmom$args)
  dst_params <- fam_lmom$param_map(lmom_params)
  dst_fun <- paste0("distionary::dst_", family)
  return(rlang::exec(dst_fun, !!!dst_params))
}