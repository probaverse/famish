test_that("Estimated parameters converge to true parameters.", {
  verbose <- FALSE
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
      distionary::dst_weibull(5, 2),
      distionary::dst_weibull(2, 5)
    ),
    unif = list(
      distionary::dst_unif(-2, 5),
      distionary::dst_unif(-5, 0),
      distionary::dst_unif(0, 5)
    ),
    t = list(
      distionary::dst_t(3),
      list(distionary::dst_t(10), tolerance = 2)
    ),
    pois = list(
      distionary::dst_pois(1),
      distionary::dst_pois(5)
    ),
    pearson3 = list(
      distionary::dst_pearson3(20, 2, 0.5),
      distionary::dst_pearson3(30, 3, 1),
      distionary::dst_pearson3(10, 1, 2)
    ),
    norm = list(
      distionary::dst_norm(0, 1),
      distionary::dst_norm(-5, 3),
      distionary::dst_norm(5, 2)
    ),
    nbinom = list(
      distionary::dst_nbinom(5, 0.2),
      distionary::dst_nbinom(10, 0.6)
    ),
    lp3 = list(
      distionary::dst_lp3(10, 5, 0.2),
      distionary::dst_lp3(20, 2, 0.5),
      distionary::dst_lp3(30, 3, 1),
      distionary::dst_lp3(10, 1, 1.5)
    ),
    lnorm = list(
      distionary::dst_lnorm(-5, 1),
      distionary::dst_lnorm(2, 0.1)
    ),
    gp = list(
      distionary::dst_gp(1, 0),
      distionary::dst_gp(2, 0.5),
      distionary::dst_gp(3, 1),
      list(
        # moments don't exist
        distionary::dst_gp(1, 2), tolerance = 7, method = "lmom"
      )
    ),
    gev = list(
      list(
        # slow convergence
        distionary::dst_gev(10, 5, 0), tolerance = 4, method = "mge"
      ),
      list(
        # slow convergence
        distionary::dst_gev(20, 2, 0.5), tolerance = 2, method = "mge"
      ),
      list(
        # slow convergence
        distionary::dst_gev(30, 3, 1), tolerance = 2, method = "mge"
      ),
      list(
        # moments do not exist
        distionary::dst_gev(10, 1, 2), tolerance = 12, method = c("lmom", "mge")
      ) 
    ),
    gumbel = list(
      distionary::dst_gev(-10, 5, 0),
      distionary::dst_gev(10, 2, 0),
      distionary::dst_gev(0, 3, 0)
    ),
    geom = list(
      distionary::dst_geom(0.3),
      distionary::dst_geom(0.9)
    ),
    gamma = list(
      distionary::dst_gamma(5, 3),
      distionary::dst_gamma(3, 5)
    ),
    f = list(
      distionary::dst_f(5, 3),
      distionary::dst_f(3, 5)
    ),
    exp = list(
      distionary::dst_exp(3),
      distionary::dst_exp(9)
    ),
    degenerate = list(
      distionary::dst_degenerate(5),
      distionary::dst_degenerate(-2),
      distionary::dst_degenerate(0)
    ),
    chisq = list(
      distionary::dst_chisq(3),
      distionary::dst_chisq(10)
    ),
    cauchy = list(
      distionary::dst_cauchy(0, 5),
      distionary::dst_cauchy(-5, 1),
      distionary::dst_cauchy(5, 1)
    ),
    beta = list(
      distionary::dst_beta(5, 2),
      distionary::dst_beta(2, 5),
      distionary::dst_beta(0.2, 5),
      distionary::dst_beta(5, 0.2)
    ),
    bern = list(
      distionary::dst_bern(0.2),
      distionary::dst_bern(0.9)
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
      cat("====================", fam, "=====================\n")
    }
    distributions <- test_distributions[[fam]]
    methods <- all_methods[[fam]]
    for (method in methods) {
      for (d in distributions) {
        if (!distionary::is_distribution(d)) {
          tol_method <- d[["method"]]
          if (is.null(tol_method) || method %in% tol_method) {
            dist_tol <- d[["tolerance"]]
          } else {
            dist_tol <- tol
          }
          d <- d[[1]]
        } else {
          dist_tol <- tol
        }
        if (verbose) {
          cat("Family: ", fam, "\n")
          cat("Method: ", method, "\n")
          cat("Tolerance: ", dist_tol, "\n")
        }
        actual <- unlist(distionary::parameters(d))
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
            x <- append(x, distionary::realise(d, n = ndraw))
            fit <- suppressWarnings(fit_dst(fam, x, method = method))
            estim <- unlist(distionary::parameters(fit))
            # Second `abs` when calculating `diff` handles NA cases
            # (otherwise, diff = -Inf)
            diff <- suppressWarnings(abs(max(abs(actual - estim))))
            temp_diff[i] <- diff
            temp_para[[i]] <- estim
            if (verbose) {
              cat("|")
            }
          }
          is_null <- distionary::pretty_name(fit) == "Null"
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
          cat("- - - - - - - - - - - - - - - - -\n")
        }
        expect_lt(num_nulls, 4)
        if (num_nulls >= 4 && verbose) {
          cat("FAILURE FOR FAMILY:", fam, "METHOD:", method, "\n")
        }
      }
    }
  }
})
