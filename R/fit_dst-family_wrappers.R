fit_dst_pearson3 <- function(x, method = c("mge", "lmom")) {
  method <- rlang::arg_match(method)
  fit_dst(family = "pearson3", x = x, method = method)
}

fit_dst_lp3 <- function(x, method = c("mge", "lmom")) {
  method <- rlang::arg_match(method)
  fit_dst(family = "lp3", x = x, method = method)
}

fit_dst_gev <- function(x, method = c("mle", "lmom", "mom", "mge")) {
  method = rlang::arg_match(method)
  fit_dst(family = "gev", x = x, method = method)
}

fit_dst_gpd <- function(x, method = c("mle", "lmom", "mom", "mge")) {
  fit_dst(family = "gpd", x = x, method = method,
          diagnostics = diagnostics, threshold = threshold, ...)
}

fit_dst_lnorm <- function(x, method = c("mle", "lmom", "mom", "mge")) {
  method <- rlang::arg_match(method)
  fit_dst(family = "lnorm", x = x, method = method)
}

fit_dst_norm <- function(x, method = c("mle", "lmom", "mom", "mge")) {
  method <- rlang::arg_match(method)
  fit_dst(family = "norm", x = x, method = method)
}
