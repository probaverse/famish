# Internal wrapper for fitting a distribution family by L-moments.
# `x` is expected to not have NA.
wrapper_lmom <- function(family, x) {
  checkmate::assert_character(family, len = 1)
  checkmate::assert_numeric(x, any.missing = FALSE)
  sam <- lmom::samlmu(x)
  rng <- range(x)
  ## Families not recognized my lmom package but that have an easy solution.
  if (family == "exp") {
    if (sam[[1]] <= 0) {
      stop("L-moments invalid.")
    }
    return(distionary::dst_exp(1 / sam[[1]]))
  }
  if (family == "pois") {
    if (sam[[1]] <= 0) {
      stop("L-moments invalid.")
    }
    return(distionary::dst_pois(sam[[1]]))
  }
  if (family == "bern") {
    if (sam[[1]] < 0 || sam[[1]] > 1) {
      stop("L-moments invalid.")
    }
    return(distionary::dst_bern(sam[[1]]))
  }
  if (family == "geom") {
    prob <- 1 / (1 + sam[[1]])
    if (prob <= 0 || prob > 1) {
      stop("L-moments invalid.")
    }
    return(distionary::dst_geom(prob))
  }
  if (family == "chisq") {
    if (sam[[1]] <= 0) {
      stop("L-moments invalid.")
    }
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
      "The moments of a Cauchy distribution do not exist; cannot fit by ",
      "L-moments."
    )
  }
  ## Families recognized by lmom. Define mapping:
  ## - name: `pel*()` function name in lmom package
  ## - args: additional arguments to `pel*()` function relevant for probaverse
  ## - param_map: function to map lmom parameters to distionary parameters
  mapping <- list(
    gamma = list(
      name = "pelgam",
      args = list(),
      param_map = unname
    ),
    gev = list(
      name = "pelgev",
      args = list(),
      param_map = \(p) list(p[[1]], p[[2]], -p[[3]])
    ),
    gp = list(
      name = "pelgpa",
      args = list(bound = 0),
      param_map = \(p) list(p[[2]], -p[[3]])
    ),
    gumbel = list(
      name = "pelgum",
      args = list(),
      param_map = unname
    ),
    lnorm = list(
      name = "pelln3",
      args = list(bound = 0),
      param_map = \(p) list(p[[2]], p[[3]])
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
      param_map = \(p) list(p[[2]], p[[3]])
    )
  )
  fam_lmom <- mapping[[family]]
  if (is.null(fam_lmom)) {
    stop("Fitting by L-moments not implemented for family '", family, "'.")
  }
  lmom_params <- rlang::exec(fam_lmom$name, sam, !!!fam_lmom$args)
  dst_params <- fam_lmom$param_map(lmom_params)
  dst_fun <- paste0("dst_", family)
  if (family == "gumbel") {
    dst_fun <- "dst_gev"
    dst_params <- append(dst_params, shape = 0)
  }
  cll <- rlang::call2(dst_fun, !!!dst_params, .ns = "distionary")
  exec(cll)
}