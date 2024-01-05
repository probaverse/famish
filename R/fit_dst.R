#' Fit a distribution
#'
#' Fits a probability distribution by calling a specific `fit_dst_*()`
#' function. Still a quick-and-dirty implementation.
#'
#' @param x Numeric vector to fit the distribution to.
#' @param name Name of the distribution, like `"norm"` or `"gev"`.
#' @param method Estimation method to use, like `"mle"` or `"lmom"`.
#' For now, you'll have to look at the documentation
#' of the specific fitting functions to see examples.
#' @param ... Other arguments to pass to the specific `fit_dst_*()`
#' function.
#' @return A probability distribution.
#' @examples
#' fit_dst(1:10, "norm", "mle")
#' @export
fit_dst <- function(x, name, method, ...) {
  if (length(x) == 0) return(distionary::dst_null())
  dots <- rlang::list2(...)
  if (name == "empirical") {
    return(distionary::dst_empirical(x))
  }
  fit_fun <- paste0("fit_dst_", name)
  # A hack to account for some fit_dst functions not allowing for ...
  if (!length(names(dots))) {
    rlang::exec(fit_fun, x, method = method)
  } else if (names(dots) %in% names(formals(fit_fun))) {
    rlang::exec(fit_fun, x, method = method, !!!dots)
  } else {
    stop("Not all dots are accepted downstream.")
  }
}
