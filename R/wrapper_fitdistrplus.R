# Internal wrapper for fitting a distribution family using fitdistrplus.
# `x` is expected to not have NA.
# `method` is passed to `fitdistrplus::fitdist()`.
wrapper_fitdistrplus <- function(family, x, method) {
  checkmate::assert_character(family, len = 1)
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_character(method, len = 1)
  fit <- fitdistrplus::fitdist(
    data = x,
    distr = family,
    method = method
  )
  params <- fit$estimate
  dst_fun <- paste0("dst_", family)
  rlang::exec(dst_fun, !!!params, .env = as.environment("package:distionary"))
}