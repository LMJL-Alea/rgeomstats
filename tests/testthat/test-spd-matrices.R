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

test_that("SPDMatrices method differential_cholesky_factor() works", {
  spdm <- SPDMatrices$new(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spdm$differential_cholesky_factor(diag(1, 3), A))
})

test_that("SPDMatrices method differential_exp() works", {
  spdm <- SPDMatrices$new(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spdm$differential_exp(diag(1, 3), A))
})

test_that("SPDMatrices method differential_log() works", {
  spdm <- SPDMatrices$new(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spdm$differential_log(diag(1, 3), A))
})

test_that("SPDMatrices method differential_power() works", {
  spdm <- SPDMatrices$new(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spdm$differential_power(2, diag(1, 3), A))
})

test_that("SPDMatrices method expm() works", {
  spdm <- SPDMatrices$new(n = 3)
  expect_equal(spdm$expm(diag(0, 3)), diag(1, 3))
})

test_that("SPDMatrices method logm() works", {
  spdm <- SPDMatrices$new(n = 3)
  expect_equal(spdm$logm(diag(1, 3)), diag(0, 3))
})

test_that("SPDMatrices method powerm() works", {
  spdm <- SPDMatrices$new(n = 3)
  expect_equal(spdm$powerm(diag(1, 3), 2), diag(1, 3))
})

test_that("SPDMatrices method inverse_differential_exp() works", {
  spdm <- SPDMatrices$new(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spdm$inverse_differential_exp(diag(1, 3), A))
})

test_that("SPDMatrices method inverse_differential_log() works", {
  spdm <- SPDMatrices$new(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spdm$inverse_differential_log(diag(1, 3), A))
})

test_that("SPDMatrices method inverse_differential_power() works", {
  spdm <- SPDMatrices$new(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spdm$inverse_differential_power(2, diag(1, 3), A))
})

test_that("SPDMatrices method projection() works", {
  spdm <- SPDMatrices$new(n = 3)
  A <- matrix(1:9, 3, 3)
  expect_snapshot(spdm$projection(A))
})

test_that("SPDMatrices method random_point() works", {
  skip("Needs bug fix in gs")
  reticulate::py_set_seed(1234)
  spdm <- SPDMatrices$new(n = 3)
  spl <- spdm$random_point(10)
  expect_snapshot(spl)
})

test_that("SPDMatrices method random_tangent_vec() works", {
  reticulate::py_set_seed(1234)
  spdm <- SPDMatrices$new(n = 3)
  spl <- spdm$random_tangent_vec(diag(1, 3), 10)
  expect_snapshot(spl)
})
