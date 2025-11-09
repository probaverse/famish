# Internal function to check whether a vector of data values is valid with
# the potential support of the given distribution family.
# Returns TRUE if so, FALSE if not.
# `x` is not allowed to contain NA.
# See also `distribution_supports_data()` for a harder check based on a 
# distribution rather than a broader distribution family.
family_supports_data <- function(x, family) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_character(family, len = 1)
  ## Bernoulli first: {0, 1}
  if (family == "bern") {
    if (any(!(x %in% c(0, 1)))) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  ## Beta next: [0, 1]
  if (family == "beta") {
    if (any(x < 0) || any(x > 1)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  ## Degenerate next: {constant}
  if (family == "degenerate") {
    if (length(unique(x)) > 1) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  ## Integer-valued next: {0, 1, 2, ...}
  integer_valued <- c("binom", "geom", "hyper", "nbinom", "pois")
  if (family %in% integer_valued) {
    if (any(x != floor(x)) || any(x < 0)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  ## Positive-valued continuous next: [0, Inf)
  positive_valued <- c(
    "chisq", "exp", "f", "gamma", "gp", "lnorm", "lp3", "weibull"
  )
  if (family %in% positive_valued) {
    if (any(x < 0)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  ## Real-valued continuous next. Other distributions not listed here are
  ## not yet supported, so throw an error if encountered.
  ## (Including Null distribution here because it doesn't care about x).
  real_valued <- c(
    "cauchy", "gev", "norm", "pearson3", "t", "unif", "empirical", "finite", 
    "null"
  )
  if (family %in% real_valued) {
    return(TRUE)
  }
  stop(
    "Checking data compatibility with the '", family, 
    "' family of distributions is currently not available."
  )
}