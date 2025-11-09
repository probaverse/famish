# Internal function to check whether a vector of data values is valid with
# the support of the given distribution.
# Returns TRUE if so, FALSE if not.
# `x` is not allowed to contain NA.
# See also `family_supports_data()` for a softer check based on a distribution
# family rather than a specific distribution.
distribution_supports_data <- function(x, distribution) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_class(distribution, "dst")
  nm <- distionary::pretty_name(distribution)
  
  ## Some families have the same support for all distribution members.
  ## Dispatch `family_supports_data()` for these.
  ## NB: currently need to manually map the `dst_` function name because
  ## distionary does not make it easy to know what the dst_ call was from
  ## a distribution object.
  same <- c(
    "Bernoulli" = "bern",
    "Beta" = "beta",
    "Geometric" = "geom",
    "Negative Binomial" = "nbinom",
    "Poisson" = "pois",
    "Chi Squared" = "chisq",
    "Exponential" = "exp",
    "F" = "f",
    "Gamma" = "gamma",
    "Log Normal" = "lnorm",
    "Weibull" = "weibull",
    "Cauchy" = "cauchy",
    "Normal" = "norm",
    "Student t" = "t",
    "Null" = "null"
  )
  if (nm %in% names(same)) {
    return(family_supports_data(x = x, family = same[[nm]]))
  }
  ## Special checks
  ## -> Integer-valued (with finite support) are indeed integer-valued.
  if (nm %in% c("Binomial", "Hypergeometric") && any(x != floor(x))) {
    return(FALSE)
  }
  ## -> Finite/Empirical distributions cover all values in `x`.
  if (nm == "Finite") {
    supp <- distionary::parameters(distribution)[["outcomes"]]
    if (all(x %in% supp)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  ## -> If distribution is not continuous, and not a distionary-provided
  ##    discrete distribution, then support is currently not available.
  good_discretes_remaining <- c("Degenerate", "Binomial", "Hypergeometric")
  v <- distionary::vtype(distribution)
  if (v != "continuous" && !nm %in% good_discretes_remaining) {
    stop(
      "Checking data compatibility with distribution '", nm, 
      "' is currently not available."
    )
  }
  ## Now it suffices to check that data are within range.
  rng <- range(distribution)
  if (all(x >= rng[1] & x <= rng[2])) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}