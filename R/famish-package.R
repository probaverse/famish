#' @section Overview:
#' The \pkg{famish} package tunes probability distribution families so that they
#' align with empirical data or expert-specified targets while remaining
#' compatible with the
#' \href{https://probaverse.com}{probaverse ecosystem}.
#' With \pkg{famish}, you can:
#' \enumerate{
#'   \item Fit distribution families with a single interface that wraps
#'         established estimation routines.
#'   \item Obtain results as `distionary` objects for downstream manipulation
#'         and comparison.
#'   \item Access empirical ranking and quantile score as diagnostics.
#' }
#'
#' The package acts as the estimation layer of probaverse, providing a bridge
#' from distribution specifications to calibrated models.
#'
#' @section Fitting Distribution Families:
#' Use [`fit_dst()`] to fit any supported family by specifying the distribution
#' family name and estimation method. Convenience wrappers such as
#' [`fit_dst_gev()`], [`fit_dst_gp()`], and other `fit_dst_*()` functions expose
#' documented combinations of families and fitting approaches.
#' Supported methods include maximum likelihood, maximum goodness-of-fit
#' method, method of moments, and
#' L-moments (including log-scale variants for selected families). For
#' references to these different methods, see the `fitdistrplus` package, or
#' the `lmom` package for estimation by L-moments.
#'
#' @section Getting Started:
#' New users can begin with:
#' \itemize{
#'   \item the README for an overview of package goals and examples,
#'   \item `vignette("fitting", package = "famish")` for a tutorial on fitting
#'         workflows.
#' }
#'
#' @examples
#' library(distionary)
#'
#' set.seed(2024)
#' sample_data <- rgamma(100, shape = 2, rate = 0.4)
#'
#' fitted_gamma <- fit_dst("gamma", x = sample_data, method = "mle")
#' fitted_gamma
#'
#' parameters(fitted_gamma)
#' enframe_return(fitted_gamma, at = c(2, 5, 10))
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
