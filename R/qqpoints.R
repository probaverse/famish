#' QQ plot pairs
#'
#' @param x,y distionary distribution, or numeric vector.
#' @param n minimum number of points to include in the plot.
#' @param xname,yname Names of the x and y columns.
#' @return A data frame of pairs, named by `xname` and `yname`.
#' @details The quantile functions of `x` and `y` are evaluated on a grid
#' of `n` quantile levels.
#' @export
qqpoints <- function(x, y, xname = "x_quantile", yname = "y_quantile", n = 1000) {
  if (is.numeric(x)) {
    n <- max(n, length(x))
    x <- distionary::dst_empirical(x)
  }
  if (is.numeric(y)) {
    n <- max(n, length(y))
    y <- distionary::dst_empirical(y)
  }
  if (!distionary::is_distribution(x) || !distionary::is_distribution(y)) {
    stop("`x` and `y` must be either numeric vectors or distplyr distributions")
  }
  tau <- 1:n / (n + 1)
  res <- data.frame(
    x = distionary::eval_quantile(x, at = tau),
    y = distionary::eval_quantile(y, at = tau)
  )
  stats::setNames(res, c(xname, yname))
}
