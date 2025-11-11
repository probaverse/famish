library(distionary)
d <- dst_gev(10, 1, 2)
set.seed(1)
x <- realise(d, n = 1000)

test_that("foo", {
  # d <- fit_dst("gev", x, "mge")
  dee <- fit_dst("gev", x, "mge")
  expect_true(is_distribution(dee))
})
test_that("foo", {
  expect_equal(5 * 2, 10)
})