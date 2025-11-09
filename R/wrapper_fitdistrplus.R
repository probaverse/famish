# Internal wrapper for fitting a distribution family using fitdistrplus.
# `x` is expected to not have NA.
# `method` is passed to `fitdistrplus::fitdist()`.
wrapper_fitdistrplus <- function(family, x, method) {
  checkmate::assert_character(family, len = 1)
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_character(method, len = 1)
  start <- NULL
  if (family == "t") {
    v <- stats::var(x)
    start <- 2 * v / (v - 1)
    if (start < 0) {
      start <- 100
    }
    start <- list(df = start)
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
  }
  if (family == "lp3") {
    logx <- log(x)
    mu <- mean(logx)
    sd <- stats::sd(logx)
    skew <- mean(((logx - mu) / sd)^3)
    start <- list(meanlog = mu, sdlog = sd, skew = skew)
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
  }
  if (family == "chisq") {
    mu <- mean(x)
    start <- list(df = mu)
  }
  if (family == "bern") {
    # The MLE of Bernoulli is just the mean of the 0-1 data.
    return(distionary::dst_bern(mean(x)))
  }
  if (family %in% c("gev", "gp", "gumbel")) {
    prefit <- wrapper_ismev(family = family, x = x)
    start <- distionary::parameters(prefit)
    if (family == "gumbel") {
      start[["shape"]] <- NULL
    }
  }
  # Mapping between fitdistrplus and distionary
  mappings <- list(
    nbinom = function(p) {
      size <- p[["size"]]
      mu <- p[["mu"]]
      c(size = size, prob = size / (size + mu))
    }
  )
  
  fit <- fitdistrplus::fitdist(
    data = x,
    distr = family,
    method = method,
    start = start
  )
  params <- fit$estimate
  pmap <- mappings[[family]]
  if (!is.null(pmap)) {
    params <- pmap(params)
  }
  if (family == "gumbel") {
    family <- "gev"
    params <- append(params, shape = 0)
  }
  dst_fun <- paste0("dst_", family)
  cll <- rlang::call2(dst_fun, !!!params, .ns = "distionary")
  eval(cll)
}