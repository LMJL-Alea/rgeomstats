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
      if (!self$belongs(mat))
        cli::cli_abort("The input matrix {.arg mat} should be SPD.")
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
      if (!self$belongs(base_point))
        cli::cli_abort("The input matrix {.arg base_point} should be SPD.")
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
    },

    #' @description Computes the matrix exponential for a symmetric matrix.
    #'
    #' @param mat A symmetric matrix.
    #'
    #' @return An SPD matrix storing the exponential of the input symmetric
    #'   matrix `mat`.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spdm <- SPDMatrices$new(n = 3)
    #'   spdm$expm(diag(-1, 3))
    #' }
    expm = function(mat) {
      private$m_PythonClass$expm(mat = mat)
    },

    #' @description Computes the matrix logarithm of an SPD matrix.
    #'
    #' @param mat An SPD matrix.
    #'
    #' @return A symmetric matrix storing the logarithm of the input symmetric
    #'   matrix `mat`.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spdm <- SPDMatrices$new(n = 3)
    #'   spdm$logm(diag(1, 3))
    #' }
    logm = function(mat) {
      if (!self$belongs(mat))
        cli::cli_abort("The input matrix {.arg mat} should be SPD.")
      private$m_PythonClass$logm(mat = mat)
    },

    #' @description Computes the matrix power of an SPD matrix.
    #'
    #' @param mat An SPD matrix.
    #' @param power A numeric scalar or vector specifying the desired power(s).
    #'
    #' @return An SPD matrix representing the matrix power of the input matrix
    #'   as: \deqn{A^p = \exp(p \log(A)).} If `power` is a vector, a list of
    #'   such matrices elevated at the corresponding powers.
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
    #'   spdm$powerm(diag(1, 3), 2)
    #' }
    powerm = function(mat, power) {
      if (!self$belongs(mat))
        cli::cli_abort("The input matrix {.arg mat} should be SPD.")
      private$m_PythonClass$powerm(
        mat = mat,
        power = power
      )
    },

    #' @description Computes the inverse of the differential of the matrix
    #'   exponential.
    #'
    #' @return A matrix storing the inverse of the differential of the matrix
    #'   exponential on SPD matrices at `base_point` applied to `tangent_vec`.
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
    #'   spdm$inverse_differential_exp(diag(1, 3), A)
    #' }
    inverse_differential_exp = function(tangent_vec, base_point) {
      private$m_PythonClass$inverse_differential_exp(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes the inverse of the differential of the matrix
    #'   logarithm.
    #'
    #' @return A matrix storing the inverse of the differential of the matrix
    #'   logarithm on SPD matrices at `base_point` applied to `tangent_vec`.
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
    #'   spdm$inverse_differential_log(diag(1, 3), A)
    #' }
    inverse_differential_log = function(tangent_vec, base_point) {
      private$m_PythonClass$inverse_differential_log(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes the inverse of the differential of the matrix power
    #'   function.
    #'
    #' @param power An integer scalar specifying the desired power.
    #'
    #' @return A matrix storing the inverse of the differential of the power
    #'   function on \eqn{\mathrm{SPD}(n)}: \deqn{A^p = \exp(p \log(A))} at
    #'   `base_point` applied to `tangent_vec`.
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
    #'   spdm$inverse_differential_power(2, diag(1, 3), A)
    #' }
    inverse_differential_power = function(power, tangent_vec, base_point) {
      private$m_PythonClass$inverse_differential_power(
        power = as.integer(power),
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Projects any square matrix to the space of SPD matrices.
    #'
    #' @details First the symmetric part of point is computed, then the
    #'   eigenvalues are floored to `gs.atol`.
    #'
    #' @param point A square matrix.
    #'
    #' @return An SPD matrix obtained by projecting the input square matrix onto
    #'   the space of SPD matrices.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spdm <- SPDMatrices$new(n = 3)
    #'   A <- matrix(1:9, 3, 3)
    #'   spdm$projection(A)
    #' }
    projection = function(point) {
      private$m_PythonClass$projection(point = point)
    },

    #' @description Samples in \eqn{\mathrm{SPD}(n)} from the log-uniform
    #'   distribution.
    #'
    #' @param n_samples An integer value specifying the sample size. Defaults to
    #'   `1L`.
    #' @param bound A numeric value specifying the bound of the interval in
    #'   which to sample in the tangent space. Defaults to `1.0`.
    #'
    #' @return A list of SPD matrices sampled in \eqn{\mathrm{SPD}(n)}.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spdm <- SPDMatrices$new(n = 3)
    #'   # spdm$random_point(10) # TO DO: uncomment when bug fixed in gs
    #' }
    random_point = function(n_samples = 1, bound = 1.0) {
      private$m_PythonClass$random_point(
        n_samples = as.integer(n_samples),
        bound = bound
      )
    },

    #' @description Samples on the tangent space of \eqn{\mathrm{SPD}(n)} from
    #'   the uniform distribution.
    #'
    #' @param n_samples An integer value specifying the sample size. Defaults to
    #'   `1L`.
    #'
    #' @return A list of symmetric matrices sampled on the tangent space of
    #'   \eqn{\mathrm{SPD}(n)} at `base_point`.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spdm <- SPDMatrices$new(n = 3)
    #'   spdm$random_tangent_vec(diag(1, 3), 10)
    #' }
    random_tangent_vec = function(base_point, n_samples = 1) {
      if (!self$belongs(base_point))
        cli::cli_abort("The input matrix {.arg base_point} should be SPD.")
      array_res <- private$m_PythonClass$random_tangent_vec(
        base_point = base_point,
        n_samples = as.integer(n_samples)
      )
      purrr::array_tree(array_res, margin = 1)
    }
  ),
  private = list(
    m_PythonClass = NULL
  )
)
