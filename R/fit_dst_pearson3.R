.fit_dst_family_pearson3 <- function(x, method) {
  if (method == "mge") {
    mu <- mean(x)
    sd <- stats::sd(x)
    skew <- mean(((x - mu) / sd)^3)
    fit <- fitdistrplus::fitdist(
      x, distr = "pearson3",
      start = list(mean = mu, sd = sd, skew = skew),
      method = method
    )
    params <- fit$estimate
    return(distionary::dst_parametric("pearson3",
                                      !!!params, .variable = "continuous"))
  }
}
