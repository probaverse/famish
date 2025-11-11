# Internal wrapper for fitting a distribution family using fitdistrplus.
# `x` is expected to not have NA.
# `method` is passed to `fitdistrplus::fitdist()`.
wrapper_fitdistrplus <- function(family, x, method) {
  checkmate::assert_character(family, len = 1)
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_character(method, len = 1)
  ## ------- Special cases -------
  if (family == "bern" && method == "mle") {
    # The MLE of Bernoulli is just the mean of the 0-1 data.
    if (!all(unique(x) %in% c(0, 1))) {
      stop(
        "Data for Bernoulli distribution must consist only of 0 and 1 values."
      )
    }
    return(distionary::dst_bern(mean(x)))
  }
  ## ------- Starting values -------
  start <- NULL
  lower <- -Inf
  upper <- Inf
  if (family == "t") {
    v <- stats::var(x)
    start <- 2 * v / (v - 1)
    if (start < 0) {
      start <- 100
    }
    start <- list(df = start)
    lower <- 0
  }
  if (family == "pearson3") {
    mu <- mean(x)
    sigma <- stats::sd(x)
    skew <- mean(((x - mu) / sigma)^3)
    shape <- 4 / skew^2
    scale <- sigma / sqrt(shape)
    location <- mu - scale * shape
    if (location > min(x)) {
      location <- min(x) - 1e-10
    }
    start <- list(location = location, scale = scale, shape = shape)
    lower <- c(-Inf, 0, 0)
  }
  if (family == "lp3") {
    logx <- log(x)
    mu <- mean(logx)
    sd <- stats::sd(logx)
    skew <- mean(((logx - mu) / sd)^3)
    start <- list(meanlog = mu, sdlog = sd, skew = skew)
    lower <- c(-Inf, 0, 0)
  }
  if (family == "f") {
    mu <- mean(x)
    v <- stats::var(x)
    d1 <- (2 * mu^2) / (v * (2 - mu) - mu^2 * (mu - 1))
    d2 <- 2 * mu / (mu - 1)
    if (d2 < 0) {
      d2 <- 100
    }
    if (d1 < 0) {
      d1 <- 100
    }
    start <- list(df1 = d1, df2 = d2)
    lower <- c(0, 0)
  }
  if (family == "chisq") {
    mu <- mean(x)
    start <- list(df = mu)
    lower <- 0
  }
  if (family %in% c("gev", "gp", "gumbel")) {
    prefit <- wrapper_ismev(family = family, x = x)
    start <- distionary::parameters(prefit)
    if (family == "gumbel") {
      start[["shape"]] <- NULL
    }
  }
  # ------- Wrapper -------
  ## Mappings to distionary parameters
  mappings <- list(
    nbinom = function(p) {
      size <- p[["size"]]
      mu <- p[["mu"]]
      c(size = size, prob = size / (size + mu))
    }
  )
  fit <- suppressWarnings(fitdistrplus::fitdist(
    data = x,
    distr = family,
    method = method,
    start = start,
    lower = lower
  ))
  params <- fit$estimate
  if (anyNA(params)) {
    stop("Fitting resulted in NA parameters, and therefore failed to fit.")
  }
  pmap <- mappings[[family]]
  if (!is.null(pmap)) {
    params <- pmap(params)
  }
  if (family == "gumbel") {
    family <- "gev"
    params <- append(params, c(shape = 0))
  }
  dst_fun <- paste0("dst_", family)
  cll <- rlang::call2(dst_fun, !!!params, .ns = "distionary")
  eval(cll)
}