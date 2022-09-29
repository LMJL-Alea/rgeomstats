#' Manifold of Symmetric Positive Definite Matrices
#'
#' @description Class for the manifold of symmetric positive definite (SPD)
#'   matrices.
#'
#' @param tangent_vec A symmetric matrix specifying the tangent vector at
#'   base point.
#' @param base_point An SPD matrix specifying the base point.
#'
#' @author Yann Thanwerdas
#' @family discretized curves space
#'
#' @export
SPDMatrices <- R6::R6Class(
  classname = "SPDMatrices",
  public = list(
    #' @description The [`SPDMatrices`] class constructor.
    #'
    #' @param n An integer value representing the shape of the `n x n` matrices.
    #'
    #' @return A [`SPDMatrices`] [R6::R6Class] object.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spdm <- SPDMatrices$new(n = 3)
    #'   spdm
    #' }
    initialize = function(n) {
      n <- as.integer(n)
      private$m_PythonClass <- gs$geometry$spd_matrices$SPDMatrices(n = n)
    },

    #' @description Check if a matrix is symmetric with positive eigenvalues.
    #'
    #' @param mat A numeric matrix to be checked.
    #' @param atol A numeric value specifying the absolute tolerance for
    #'   checking. Defaults to `1e-12`.
    #'
    #' @return A boolean that tells whether the input matrix is SPD.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spdm <- SPDMatrices$new(n = 3)
    #'   A <- diag(1, 3)
    #'   spdm$belongs(A)
    #'   B <- diag(-1, 3)
    #'   spdm$belongs(B)
    #' }
    belongs = function(mat, atol = 1e-12) {
      private$m_PythonClass$belongs(mat = mat, atol = atol)
    },

    #' @description Computes Cholesky factor for a symmetric positive definite
    #'   matrix.
    #'
    #' @param mat An SPD matrix.
    #'
    #' @return The Cholesky factor represented as a numeric vector which stores
    #'   its lower triangular matrix including the diagonal elements in a
    #'   column-major fashion.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spdm <- SPDMatrices$new(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spdm$cholesky_factor(A)
    #' }
    cholesky_factor = function(mat) {
      M <- private$m_PythonClass$cholesky_factor(mat = mat)
      M[lower.tri(M, diag = TRUE)]
    },

    #' @description Computes the differential of the Cholesky factor map.
    #'
    #' @return The differential of the Cholesky factor map represented as a
    #'   numeric vector which stores its lower triangular matrix including the
    #'   diagonal elements in a column-major fashion.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spdm <- SPDMatrices$new(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spdm$differential_cholesky_factor(diag(1, 3), A)
    #' }
    differential_cholesky_factor = function(tangent_vec, base_point) {
      private$m_PythonClass$differential_cholesky_factor(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes the differential of the matrix exponential.
    #'
    #' @return A matrix storing the differential of the matrix exponential on
    #'   SPD matrices at `base_point` applied to `tangent_vec`.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spdm <- SPDMatrices$new(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spdm$differential_exp(diag(1, 3), A)
    #' }
    differential_exp = function(tangent_vec, base_point) {
      private$m_PythonClass$differential_exp(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes the differential of the matrix logarithm.
    #'
    #' @return A matrix storing the differential of the matrix logarithm on
    #'   SPD matrices at `base_point` applied to `tangent_vec`.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spdm <- SPDMatrices$new(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spdm$differential_log(diag(1, 3), A)
    #' }
    differential_log = function(tangent_vec, base_point) {
      private$m_PythonClass$differential_log(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes the differential of the matrix power function.
    #'
    #' @param power An integer scalar specifying the desired power.
    #'
    #' @return A matrix storing the differential of the power function on
    #'   \eqn{\mathrm{SPD}(n)}: \deqn{A^p = \exp(p \log(A))} at `base_point`
    #'   applied to `tangent_vec`.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spdm <- SPDMatrices$new(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spdm$differential_power(2, diag(1, 3), A)
    #' }
    differential_power = function(power, tangent_vec, base_point) {
      private$m_PythonClass$differential_power(
        power = as.integer(power),
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    }
  ),
  private = list(
    m_PythonClass = NULL
  )
)
