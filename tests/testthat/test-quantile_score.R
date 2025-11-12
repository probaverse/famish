test_that("Quantile Score handles unanticipated inputs appropriately.", {
  # Missing inputs throws an error.
  expect_error(quantile_score())
  expect_error(quantile_score(x = 0.5))
  expect_error(quantile_score(xhat = 0.5))
  expect_error(quantile_score(tau = 0.5))
  expect_error(quantile_score(x = 0.5, xhat = 0.5))
  expect_error(quantile_score(x = 0.5, tau = 0.5))
  expect_error(quantile_score(xhat = 0.5, tau = 0.5))
  # Incorrect inputs throws an error.
  x <- c(0.2, 0.5, 0.8)
  xhat <- c(0.3, 0.6, 0.9)
  expect_error(quantile_score(x = "0.5", xhat = xhat, tau = 0.5))
  expect_error(quantile_score(x = x, xhat = "0.5", tau = 0.5))
  expect_error(quantile_score(x = x, xhat = xhat, tau = "0.5"))
  # Can't recycle inputs of different lengths.
  expect_error(quantile_score(x = c(0.2, 0.5), xhat = xhat, tau = 0.5))
  expect_error(quantile_score(x = x, xhat = c(0.3, 0.6), tau = 0.5))
  expect_error(quantile_score(x = x, xhat = xhat, tau = c(0.4, 0.5)))
  # Length 0 is OK as long as other inputs are length 1 or 0
  expect_equal(quantile_score(x = numeric(0), xhat = 1, tau = 0.5), numeric(0))
  expect_equal(quantile_score(x = 1, xhat = numeric(0), tau = 0.5), numeric(0))
  expect_equal(quantile_score(x = 1, xhat = 1, tau = numeric(0)), numeric(0))
  expect_equal(
    quantile_score(x = numeric(0), xhat = numeric(0), tau = 0.5),
    numeric(0)
  )
  expect_equal(
    quantile_score(x = numeric(0), xhat = 1, tau = numeric(0)),
    numeric(0)
  )
  expect_equal(
    quantile_score(x = 1, xhat = numeric(0), tau = numeric(0)),
    numeric(0)
  )
  expect_equal(
    quantile_score(x = numeric(0), xhat = numeric(0), tau = numeric(0)),
    numeric(0)
  )
  expect_error(quantile_score(x = numeric(0), xhat = 1:2, tau = numeric(0)))
  expect_error(quantile_score(x = numeric(0), xhat = numeric(0), tau = 1:2 / 4))
  expect_error(quantile_score(x = 1:2, xhat = numeric(0), tau = numeric(0)))
  # NA gets propagated
  expect_equal(
    quantile_score(x = 1, xhat = c(1, NA), tau = 0.5),
    c(0, NA_real_)
  )
})

test_that("Quantile score gets calculated properly.", {
  # Lengths are recycled appropriately.
  expect_equal(length(quantile_score(1:3, 1, 0.5)), 3)
  expect_equal(length(quantile_score(1, 1:3, 0.5)), 3)
  expect_equal(length(quantile_score(1, 1, c(0.2, 0.5, 0.8))), 3)
  expect_equal(length(quantile_score(1:3, 1:3, 0.5)), 3)
  expect_equal(length(quantile_score(1:3, 1, c(0.2, 0.5, 0.8))), 3)
  expect_equal(length(quantile_score(1, 1:3, c(0.2, 0.5, 0.8))), 3)
  # Quantile score is smaller when the estimate is closer to the observation.
  close <- quantile_score(1, 0, 1:3 / 4)
  far <- quantile_score(3, 0, 1:3 / 4)
  expect_true(all(close < far))
  # Quantile score is 0 when the estimate equals the observation.
  expect_equal(quantile_score(2.5, 2.5, 1:3 / 4), c(0, 0, 0))
  # Quantile score is the absolute value / 2 when tau = 0.5
  expect_equal(quantile_score(c(1, 4, -3), 0, 0.5), abs(c(1, 4, -3)) / 2)
  # Quantile score leans left for tau > 0.5 and right when < 0.5
  expect_lt(quantile_score(-1, 0, 0.75), quantile_score(1, 0, 0.75))
  expect_lt(quantile_score(1, 0, 0.25), quantile_score(-1, 0, 0.25))
  # Quantile score is linear left and right of the estimate.
  expect_equal(diff(quantile_score(-4:-1, 0, 0.3)), rep(-0.7, 3))
  expect_equal(diff(quantile_score(-4:-1, 0, 0.7)), rep(-0.3, 3))
  expect_equal(diff(quantile_score(1:4, 0, 0.3)), rep(0.3, 3))
  expect_equal(diff(quantile_score(1:4, 0, 0.7)), rep(0.7, 3))
})
