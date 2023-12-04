#' Frechet distribution
#'
#' @param location,scale,shape Parameters of the Frechet distribution;
#' scale and shape must be positive.
#' @export
dst_frechet <- local({
  pfrechet <- distionary::pgev
  qfrechet <- distionary::qgev
  dfrechet <- distionary::dgev
  function(location, scale, shape) {
    if (shape < 0) {
      stop("`shape` parameter must be positive.
           Perhaps you would like to use `dst_gev()` instead?")
    }
    distionary::dst_parametric(
      "frechet", location = location, scale = scale, shape = shape,
      .variable = "continuous"
    )
  }
})

#' Fit a Frechet (EV2) Distribution
#'
#' @inheritParams fit_dst_norm
#' @param maxit For the `optim()` function.
#' @note I believe I took this code from the ismev package. If I end up
#' publishing this package, I'll figure something out in terms of referencing/
#' permissions. Until then...
#' @export
fit_dst_frechet <- function (xdat, method = c("mle", "lmom", "mom", "mge"),
                             maxit = 10000) {
  method <- rlang::arg_match(method)
  if (method != "mle") {
    stop("That method is not implemented yet.")
  }
  z <- list()
  z$trans <- FALSE
  in2 <- sqrt(6 * stats::var(xdat))/pi
  in1 <- mean(xdat) - 0.57722 * in2
  mumat <- rep(1, length(xdat))
  muinit <- in1
  sigmat <- rep(1, length(xdat))
  siginit <- in2
  shmat <- rep(1, length(xdat))
  shinit <- 0.1
  init <- c(muinit, siginit, shinit)
  gev.lik <- function(a) {
    mu <- mumat * a[1]
    sc <- sigmat * a[2]
    xi <- shmat * a[3]
    y <- 1 + xi * (xdat - mu) / sc
    if (any(y <= 0) || any(sc <= 0) || any(xi <= 0))
      return(10^6)
    sum(log(sc)) + sum(y^(-1 / xi)) + sum(log(y) * (1 / xi + 1))
  }
  x <- stats::optim(init, gev.lik, hessian = TRUE, method = "Nelder-Mead",
             control = list(maxit = maxit))
  z$conv <- x$convergence
  mu <- mumat * x$par[1]
  sc <- sigmat * x$par[2]
  xi <- shmat * x$par[3]
  z$nllh <- x$value
  z$data <- xdat
  if (z$trans) {
    z$data <- -log((1 + (xi * (xdat - mu)) / sc)^(-1 / xi))
  }
  z$mle <- x$par
  z$cov <- solve(x$hessian)
  z$se <- sqrt(diag(z$cov))
  z$vals <- cbind(mu, sc, xi)
  distionary::dst_frechet(z$mle[1], z$mle[2], z$mle[3])
}
