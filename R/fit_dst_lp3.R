.fit_dst_family_lp3 <- function(x, method) {
  logx <- log(x)
  if (method == "mge") {
    mu <- mean(logx)
    sd <- stats::sd(logx)
    skew <- mean(((logx - mu) / sd)^3)
    fit <- fitdistrplus::fitdist(
      logx, distr = "pearson3",
      start = list(mean = mu, sd = sd, skew = skew),
      method = method
    )
    params <- fit$estimate
    return(dst_lp3(params[["mean"]], params[["sd"]], params[["skew"]]))
  }
  if (method == "lmom") {
    moments <- lmom::pelpe3(lmom::samlmu(logx))
    return(dst_lp3(moments[[1]], moments[[2]], moments[[3]]))
  }
  stop("That method has not been implemented yet.")
}
