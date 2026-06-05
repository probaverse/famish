# Fit a distribution family by composite quantile or expectile estimation.
#
# `family` is a distionary family name (e.g., "gev"), `x` is the NA-free data,
# and `estimator` is a `famish_estimator` object from `cqe()` or `cee()`.
# `n_grid` sets the number of nodes used to approximate the loss integral.
#
# The fit starts from the family's default built-in estimate and refines the
# parameters by minimising the composite loss. Trial parameter values that do
# not yield a usable distribution are penalised, so the search stays within the
# valid region around the starting estimate.
fit_composite <- function(family, x, estimator, n_grid = 50L) {
  checkmate::assert_character(family, len = 1)
  checkmate::assert_numeric(x, any.missing = FALSE, min.len = 1)
  checkmate::assert_class(estimator, "famish_estimator")

  start_method <- composite_start_method(family)
  d0 <- fit_dst(
    family,
    x = x,
    method = start_method,
    na_action = "fail",
    on_unres = "fail"
  )
  if (
    estimator$representation == "expectile" &&
      distionary::vtype(d0) != "continuous"
  ) {
    stop(
      "Composite expectile estimation is only supported for continuous ",
      "families, but '", family, "' is not continuous."
    )
  }
  theta0 <- unlist(distionary::parameters(d0))
  if (length(theta0) == 0) {
    stop("No parameters to optimise for family '", family, "'.")
  }
  par_names <- names(theta0)

  cutoff <- estimator$tau_cutoff
  loss <- estimator$loss
  eval_repr <- switch(
    estimator$representation,
    quantile = distionary::eval_quantile,
    expectile = distionary::eval_expectile
  )
  # Midpoint quadrature nodes over (cutoff, 1), avoiding the endpoints where
  # quantiles and expectiles can diverge.
  tau <- cutoff + (1 - cutoff) * (seq_len(n_grid) - 0.5) / n_grid
  wt <- vctrs::vec_recycle(estimator$weight(tau), n_grid)
  tau_mat <- matrix(tau, nrow = length(x), ncol = n_grid, byrow = TRUE)
  scale_const <- (1 - cutoff) / n_grid / length(x)
  penalty <- sqrt(.Machine$double.xmax)

  build <- function(theta) {
    rlang::eval_tidy(rlang::call2(
      paste0("dst_", family),
      !!!stats::setNames(as.list(theta), par_names),
      .ns = "distionary"
    ))
  }
  objective <- function(theta) {
    d <- tryCatch(build(theta), error = function(e) NULL)
    if (is.null(d)) {
      return(penalty)
    }
    g <- tryCatch(
      suppressMessages(eval_repr(d, tau)),
      error = function(e) NA_real_
    )
    if (length(g) != n_grid || any(!is.finite(g))) {
      return(penalty)
    }
    residual <- outer(x, g, "-")
    sum(wt * colSums(loss(residual, tau_mat))) * scale_const
  }

  # Nelder-Mead is derivative-free, so the hard penalty on invalid parameters
  # does not destabilise it, and it handles one- and multi-parameter families
  # alike. The warning it emits for one-parameter problems is silenced.
  opt <- suppressWarnings(
    stats::optim(theta0, objective, method = "Nelder-Mead")
  )
  build(stats::setNames(opt[["par"]], par_names))
}

# The built-in method used to seed the composite optimisation: the first method
# advertised by the family's `fit_dst_<family>()` wrapper.
composite_start_method <- function(family) {
  methods <- available_methods()[[family]]
  if (is.null(methods) || length(methods) == 0) {
    return("mle")
  }
  methods[[1]]
}
