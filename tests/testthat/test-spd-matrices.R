V <- cbind(
  c(sqrt(2) / 2, -sqrt(2) / 2, 0),
  c(sqrt(2) / 2, sqrt(2) / 2, 0),
  c(0, 0, 1)
)
A <- V %*% diag(1:3) %*% t(V)
B <- V %*% diag(4:6) %*% t(V)
D <- diag(1, 3)
Z <- diag(0, 3)
S1 <- array(dim = c(2, 3, 3))
S1[1, , ] <- A
S1[2, , ] <- B
S2 <- array(dim = c(3, 3, 3))
S2[1, , ] <- A
S2[2, , ] <- B
S2[3, , ] <- D

test_that("SPDMatrices constructor works", {
  spd3 <- SPDMatrix(n = 3)
  expect_true("SPDMatrices" %in% class(spd3))
})

test_that("SPDMatrices method belongs() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_true(spd3$belongs(A))
  expect_false(spd3$belongs(diag(-1, 3)))
})

test_that("SPDMatrices method cholesky_factor() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$cholesky_factor(A))
})

test_that("SPDMatrices method differential_cholesky_factor() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$differential_cholesky_factor(D, A))
})

test_that("SPDMatrices method expm() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$expm(Z), D)
})

test_that("SPDMatrices method differential_exp() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$differential_exp(D, A))
})

test_that("SPDMatrices method inverse_differential_exp() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$inverse_differential_exp(D, A))
})

test_that("SPDMatrices method logm() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$logm(D), Z)
})

test_that("SPDMatrices method differential_log() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$differential_log(D, A))
})

test_that("SPDMatrices method inverse_differential_log() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$inverse_differential_log(D, A))
})

test_that("SPDMatrices method powerm() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$powerm(D, 2), D)
})

test_that("SPDMatrices method differential_power() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$differential_power(2, D, A))
})

test_that("SPDMatrices method inverse_differential_power() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$inverse_differential_power(2, D, A))
})

test_that("SPDMatrices method projection() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$projection(D), D)
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
  spl <- spd3$random_tangent_vec(D, 10)
  expect_snapshot(spl)
})
