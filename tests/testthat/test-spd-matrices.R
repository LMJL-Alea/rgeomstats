test_that("SPDMatrices constructor works", {
  spd3 <- SPDMatrix(n = 3)
  expect_true("SPDMatrices" %in% class(spd3))
})

test_that("SPDMatrices method belongs() works", {
  spd3 <- SPDMatrix(n = 3)
  A <- diag(1, 3)
  expect_true(spd3$belongs(A))
  B <- diag(-1, 3)
  expect_false(spd3$belongs(B))
})

test_that("SPDMatrices method cholesky_factor() works", {
  spd3 <- SPDMatrix(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spd3$cholesky_factor(A))
})

test_that("SPDMatrices method differential_cholesky_factor() works", {
  spd3 <- SPDMatrix(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spd3$differential_cholesky_factor(diag(1, 3), A))
})

test_that("SPDMatrices method differential_exp() works", {
  spd3 <- SPDMatrix(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spd3$differential_exp(diag(1, 3), A))
})

test_that("SPDMatrices method differential_log() works", {
  spd3 <- SPDMatrix(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spd3$differential_log(diag(1, 3), A))
})

test_that("SPDMatrices method differential_power() works", {
  spd3 <- SPDMatrix(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spd3$differential_power(2, diag(1, 3), A))
})

test_that("SPDMatrices method expm() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$expm(diag(0, 3)), diag(1, 3))
})

test_that("SPDMatrices method logm() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$logm(diag(1, 3)), diag(0, 3))
})

test_that("SPDMatrices method powerm() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$powerm(diag(1, 3), 2), diag(1, 3))
})

test_that("SPDMatrices method inverse_differential_exp() works", {
  spd3 <- SPDMatrix(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spd3$inverse_differential_exp(diag(1, 3), A))
})

test_that("SPDMatrices method inverse_differential_log() works", {
  spd3 <- SPDMatrix(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spd3$inverse_differential_log(diag(1, 3), A))
})

test_that("SPDMatrices method inverse_differential_power() works", {
  spd3 <- SPDMatrix(n = 3)
  V <- cbind(
    c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    c(sqrt(2) / 2, sqrt(2) / 2, 0),
    c(0, 0, 1)
  )
  A <- V %*% diag(1:3) %*% t(V)
  expect_snapshot(spd3$inverse_differential_power(2, diag(1, 3), A))
})

test_that("SPDMatrices method projection() works", {
  spd3 <- SPDMatrix(n = 3)
  A <- matrix(1:9, 3, 3)
  expect_snapshot(spd3$projection(A))
})

test_that("SPDMatrices method random_point() works", {
  skip("Needs bug fix in gs")
  reticulate::py_set_seed(1234)
  spd3 <- SPDMatrix(n = 3)
  spl <- spd3$random_point(10)
  expect_snapshot(spl)
})

test_that("SPDMatrices method random_tangent_vec() works", {
  reticulate::py_set_seed(1234)
  spd3 <- SPDMatrix(n = 3)
  spl <- spd3$random_tangent_vec(diag(1, 3), 10)
  expect_snapshot(spl)
})
