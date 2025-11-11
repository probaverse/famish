library(distionary)

test_that("Estimated parameters converge to true parameters.", {
  verbose <- TRUE
  tol <- 0.5
  niter <- 20
  seeds <- 1:4
  ndraw <- 1000
  all_methods <- available_methods()
  # Make a list of distributions to try fitting. If a special
  # tolerance is needed for a distribution, specify it as a list with
  # the distribution as the first element and the tolerance as
  # the second element named "tolerance",
  # the third (optional) element named "method" if you only want it for a
  # specific method.
  test_distributions <- list(
    weibull = list(
      dst_weibull(5, 2),
      dst_weibull(2, 5)
    ),
    unif = list(
      dst_unif(-2, 5),
      dst_unif(-5, 0),
      dst_unif(0, 5)
    ),
    t = list(
      dst_t(3),
      list(dst_t(10), tolerance = 2)
    ),
    pois = list(
      dst_pois(1),
      dst_pois(5)
    ),
    pearson3 = list(
      dst_pearson3(20, 2, 0.5),
      dst_pearson3(30, 3, 1),
      dst_pearson3(10, 1, 2)
    ),
    norm = list(
      dst_norm(0, 1),
      dst_norm(-5, 3),
      dst_norm(5, 2)
    ),
    nbinom = list(
      dst_nbinom(5, 0.2),
      dst_nbinom(10, 0.6)
    ),
    lp3 = list(
      dst_lp3(10, 5, 0.2),
      dst_lp3(20, 2, 0.5),
      dst_lp3(30, 3, 1),
      dst_lp3(10, 1, 1.5)
    ),
    lnorm = list(
      dst_lnorm(-5, 1),
      dst_lnorm(2, 0.1)
    ),
    gp = list(
      dst_gp(1, 0),
      dst_gp(2, 0.5),
      dst_gp(3, 1),
      list(dst_gp(1, 2), tolerance = 7, method = "lmom") # moments don't exist
    ),
    gev = list(
      dst_gev(10, 5, 0),
      dst_gev(20, 2, 0.5),
      dst_gev(30, 3, 1),
      list(dst_gev(10, 1, 2), tolerance = 12, method = "lmom") # moments DNE
    ),
    gumbel = list(
      dst_gev(-10, 5, 0),
      dst_gev(10, 2, 0),
      dst_gev(0, 3, 0)
    ),
    geom = list(
      dst_geom(0.3),
      dst_geom(0.9)
    ),
    gamma = list(
      dst_gamma(5, 3),
      dst_gamma(3, 5)
    ),
    f = list(
      dst_f(5, 3),
      dst_f(3, 5)
    ),
    exp = list(
      dst_exp(3),
      dst_exp(9)
    ),
    degenerate = list(
      dst_degenerate(5),
      dst_degenerate(-2),
      dst_degenerate(0)
    ),
    chisq = list(
      dst_chisq(3),
      dst_chisq(10)
    ),
    cauchy = list(
      dst_cauchy(0, 5),
      dst_cauchy(-5, 1),
      dst_cauchy(5, 1)
    ),
    beta = list(
      dst_beta(5, 2),
      dst_beta(2, 5),
      dst_beta(0.2, 5),
      dst_beta(5, 0.2)
    ),
    bern = list(
      dst_bern(0.2),
      dst_bern(0.9)
    )
  )
  missing_fams <- sort(setdiff(names(all_methods), names(test_distributions)))
  if (verbose) {
    cat("Distributions not being tested found by `available_methods()`:\n")
    cat("--> ", paste(missing_fams, collapse = ", "))
    cat("\n\n")
  }
  expect_true(all(missing_fams == c("empirical", "finite", "null")))
  
  for (fam in names(test_distributions)) {
    if (verbose) {
      cat("==============", fam, "===============\n")
    }
    distributions <- test_distributions[[fam]]
    methods <- all_methods[[fam]]
    for (method in methods) {
      for (d in distributions) {
        if (!is_distribution(d)) {
          tol_method <- d[["method"]]
          if (is.null(tol_method) || tol_method == method) {
            dist_tol <- d[["tolerance"]]
          } else {
            dist_tol <- tol
          }
          d <- d[[1]]
        } else {
          dist_tol <- tol
        }
        if (verbose) {
          cat("Method: ", method, "\n")
          cat("Tolerance: ", dist_tol, "\n")
        }
        actual <- unlist(parameters(d))
        if (verbose) {
          print(actual)
        }
        num_nulls <- 0
        for (sd in seeds) {
          set.seed(sd)
          x <- numeric()
          i <- 0
          diff <- Inf
          temp_diff <- numeric()
          temp_para <- list()
          while (diff > dist_tol && i < niter) {
            i <- i + 1
            x <- append(x, realise(d, n = ndraw))
            fit <- suppressWarnings(fit_dst(fam, x, method = method))
            estim <- unlist(parameters(fit))
            # Second `abs` when calculating `diff` handles NA cases
            # (otherwise, diff = -Inf)
            diff <- suppressWarnings(abs(max(abs(actual - estim))))
            temp_diff[i] <- diff
            temp_para[[i]] <- estim
            if (verbose) {
              cat("|")
            }
          }
          is_null <- pretty_name(fit) == "Null"
          if (is_null) {
            # Lack of convergence not allowed. If an issue, 
            # (e.g., convergence not reached in time and the parameter is 
            # "difficult" to estimate), change the tolerance. 
            # This test is useful if, for example, there's a 
            # left-endpoint parameter that jumps left and right of the
            # data minimum, and possibly ends the loop to the right (with,
            # for example, 0 likelihood).
            expect_true(all(is.infinite(temp_diff)))
          } else {
            expect_lt(diff, dist_tol)
          }
          num_nulls <- num_nulls + is_null
          if (verbose) {
            cat("\n")
          }
        }
        if (verbose) {
          cat("\n")
          cat("Number of Null Distributions: ", num_nulls, "\n")
          cat("- - - - - - - - - - - - - - - - - - - -\n")
        }
        expect_lt(num_nulls, 4)
      }
    }
  }
})
