fitdistrplus_params_to_distionary <- function(family, params) {
  # Mapping from fitdistrplus parameter names to distionary parameter names.
  mapping_list <- list(
    norm = c(mean = "mean", sd = "sd"),
    lnorm = c(meanlog = "meanlog", sdlog = "sdlog"),
    exp = c(rate = "rate"),
    gamma = c(shape = "shape", rate = "rate"),
    weibull = c(shape = "shape", scale = "scale"),
    poisson = c(lambda = "lambda"),
    binom = c(size = "size", prob = "prob"),
    geom = c(prob = "prob"),
    hyper = c(m = "m", n = "n", k = "k"),
    chisq = c(df = "df"),
    f = c(df1 = "df1", df2 = "df2"),
    t = c(df = "df"),
    uniform = c(min = "min", max = "max")
  )
  
  if (!family %in% names(mapping_list)) {
    stop(paste("No parameter mapping defined for distribution:", family))
  }
  
  mapping <- mapping_list[[family]]
  
  distionary_params <- setNames(
    as.list(params[names(mapping)]),
    unname(mapping)
  )
  
  return(distionary_params)
}