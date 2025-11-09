library(distionary)

test_that("convergence", {
  verbose <- FALSE
  tol <- 0.5
  niter <- 20
  seeds <- 1:4
  ndraw <- 1000
  # Make a list of distributions to try fitting. If a special
  # tolerance is needed for a distribution, specify it as a list with
  # the distribution as the first element and the tolerance as
  # the second element named "tolerance".
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
      dst_gp(1, 2)
    ),
    gev = list(
      dst_gev(10, 5, 0),
      dst_gev(20, 2, 0.5),
      dst_gev(30, 3, 1),
      dst_gev(10, 1, 2)
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
    ),
    gumbel = list(
      dst_gev(-10, 5, 0),
      dst_gev(10, 2, 0),
      dst_gev(0, 3, 0)
    )
  )
  for (fam in names(test_distributions)) {
    if (verbose) {
      print(paste("--------", fam, "--------"))
    }
    spec <- test_distributions[[fam]]
    for (d in spec) {
      if (!is_distribution(d)) {
        dist_tol <- d[["tolerance"]]
        d <- d[[1]]
      } else {
        dist_tol <- tol
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
          fit <- suppressWarnings(fit_dst(fam, x, method = "mle"))
          estim <- unlist(parameters(fit))
          # Second `abs` in calculating diff handles NA cases (o.w. diff = -Inf)
          diff <- suppressWarnings(abs(max(abs(actual - estim))))
          temp_diff[i] <- diff
          temp_para[[i]] <- estim
          if (verbose) {
            cat("|")
          }
        }
        if (verbose) {
          cat("\n")
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
      }
      if (verbose) {
        print(paste("Number of Null Distributions:", num_nulls))
        cat("\n")
      }
      expect_lt(num_nulls, 4)
    }
  }
})
