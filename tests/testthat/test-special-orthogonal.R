# Setup -------------------------------------------------------------------

V <- cbind(
  c(sqrt(2) / 2, -sqrt(2) / 2, 0),
  c(sqrt(2) / 2,  sqrt(2) / 2, 0),
  c(0, 0, 1)
)
A <- V %*% diag(1:3) %*% t(V)
B <- V %*% diag(4:6) %*% t(V)
Id <- diag(1, 3)
Ze <- diag(0, 3)
S1 <- array(dim = c(2, 3, 3))
S1[1, , ] <- A
S1[2, , ] <- B
S2 <- array(dim = c(3, 3, 3))
S2[1, , ] <- A
S2[2, , ] <- B
S2[3, , ] <- Id

# Constructors ------------------------------------------------------------

test_that("SpecialOrthogonal constructors work", {
  so2_vector <- SpecialOrthogonal(n = 2, point_type = "vector")
  expect_true("SpecialOrthogonal" %in% class(so2_vector))
  expect_true("SpecialOrthogonal2Vectors" %in% class(so2_vector))
  expect_equal(so2_vector$default_point_type, "vector")
  so3_vector <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_true("SpecialOrthogonal" %in% class(so3_vector))
  expect_true("SpecialOrthogonal3Vectors" %in% class(so3_vector))
  expect_equal(so3_vector$default_point_type, "vector")
  so3 <- SpecialOrthogonal(n = 3)
  expect_true("SpecialOrthogonal" %in% class(so3))
  expect_true("SpecialOrthogonalMatrices" %in% class(so3))
  expect_equal(so3$default_point_type, "matrix")
})

# SpecialOrthogonalMatrices -----------------------------------------------

test_that("SpecialOrthogonalMatrices method belongs() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_true(so3$belongs(Id))
})

test_that("SpecialOrthogonalMatrices method intrinsic_to_extrinsic_coords() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_snapshot(so3$intrinsic_to_extrinsic_coords(Id), error = TRUE)
})

test_that("SpecialOrthogonalMatrices method extrinsic_to_intrinsic_coords() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_snapshot(so3$extrinsic_to_intrinsic_coords(Id), error = TRUE)
})

test_that("SpecialOrthogonalMatrices method projection() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_equal(so3$projection(Id), Id)
})

test_that("SpecialOrthogonalMatrices method exp() works", {
  skip("Needs bug fix in gs")
  so3 <- SpecialOrthogonal(n = 3)
  expect_equal(so3$exp(Ze), Id)
})

test_that("SpecialOrthogonalMatrices method log() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_equal(so3$log(Id), Ze)
})

test_that("SpecialOrthogonalMatrices method get_identity() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_equal(so3$get_identity(), Id)
})

test_that("SpecialOrthogonalMatrices method lie_bracket() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_equal(so3$lie_bracket(Ze, Id), Ze)
})

test_that("SpecialOrthogonalMatrices method tangent_translation_map() works", {
  so3 <- SpecialOrthogonal(n = 3)
  tangent_map <- so3$tangent_translation_map(Id)
  expect_equal(tangent_map(Id), Id)
})
