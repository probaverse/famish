# Density, distribution and quantile functions for the Pearson type III
# and log Pearson type III distribution parameterised by location, scale and
# shape (copied, for now, from the distionary package), to make them
# available to `fitdistrplus::fitdist()` when fitting that family.
ppearson3 <- function(q,
                      location,
                      scale,
                      shape,
                      lower.tail = TRUE,
                      log.p = FALSE) {
  stats::pgamma(
    q - location,
    scale = scale,
    shape = shape,
    lower.tail = lower.tail,
    log.p = log.p
  )
}

dpearson3 <- function(x, location, scale, shape, log = FALSE) {
  stats::dgamma(x - location, scale = scale, shape = shape, log = log)
}

qpearson3 <- function(p,
                      location,
                      scale,
                      shape,
                      lower.tail = TRUE,
                      log.p = FALSE) {
  location + stats::qgamma(
    p,
    shape = shape,
    scale = scale,
    lower.tail = lower.tail,
    log.p = log.p
  )
}

plp3 <- function(q, meanlog, sdlog, skew) {
  shape <- 4 / skew^2
  scale <- sdlog / sqrt(shape)
  location <- meanlog - scale * shape
  stats::pgamma(
    log(pmax(0, q)) - location,
    shape = shape, scale = scale
  )
}

dlp3 <- function(x, meanlog, sdlog, skew) {
  shape <- 4 / skew^2
  scale <- sdlog / sqrt(shape)
  location <- meanlog - scale * shape
  res <- stats::dgamma(
    log(pmax(0, x)) - location,
    shape = shape, scale = scale
  ) / x
  res[x == 0] <- 0
  res
}

qlp3 <- function(p, meanlog, sdlog, skew) {
  shape <- 4 / skew^2
  scale <- sdlog / sqrt(shape)
  location <- meanlog - scale * shape
  exp(stats::qgamma(p, shape = shape, scale = scale) + location)
}

pgumbel <- function(q, location, scale) {
  distionary::pgev(q, location, scale, shape = 0)
}

dgumbel <- function(x, location, scale) {
  distionary::dgev(x, location, scale, shape = 0)
}

qgumbel <- function(p, location, scale) {
  distionary::qgev(p, location, scale, shape = 0)
}