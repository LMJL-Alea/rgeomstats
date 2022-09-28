test_that("SPDMatrices constructor works", {
  spdm <- SPDMatrices$new(n = 3)
  expect_true("SPDMatrices" %in% class(spdm))
})

test_that("SPDMatrices method belongs() works", {
  spdm <- SPDMatrices$new(n = 3)
  A <- diag(1, 3)
  expect_true(spdm$belongs(A))
  B <- diag(-1, 3)
  expect_false(spdm$belongs(B))
})

test_that("SPDMatrices method cholesky_factor() works", {
  spdm <- SPDMatrices$new(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spdm$cholesky_factor(A))
})
