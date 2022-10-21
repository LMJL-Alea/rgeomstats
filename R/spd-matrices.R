#' Class for the Manifold of Symmetric Positive Definite Matrices
#'
#' @description Class for the manifold of symmetric positive definite (SPD)
#'   matrices.
#'
#' @param tangent_vec A symmetric matrix specifying the tangent vector at
#'   base point.
#' @param base_point An SPD matrix specifying the base point.
#'
#' @author Yann Thanwerdas
#'
#' @family symmetric positive definite matrix classes
#' @keywords internal
SPDMatrices <- R6::R6Class(
  classname = "SPDMatrices",
  inherit = OpenSet,
  public = list(
    #' @field n An integer value specifying the number of rows and columns of the
    #'   matrices.
    n = NULL,

    #' @description The [`SPDMatrices`] class constructor.
    #'
    #' @param n An integer value specifying the number of rows and columns of the
    #'   matrices.
    #' @param ... Extra arguments to be passed to parent class constructors. See
    #'   [`OpenSet`] and [`Manifold`] classes.
    #'
    #' @return An object of class [`SPDMatrices`].
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   spd3
    #' }
    initialize = function(n, ...) {
      dots <- capture_extra_params(...)
      dots$n <- as.integer(n)
      if ("shape" %in% names(dots)) {
        dots$shape <- dots$shape |>
          purrr::map(as.integer) |>
          reticulate::tuple()
      }
      if ("metric" %in% names(dots))
        dots$metric <- dots$metric$get_python_class()
      super$set_python_class(
        do.call(gs$geometry$spd_matrices$SPDMatrices, dots)
      )
      private$set_fields()
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
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$cholesky_factor(A)
    #' }
    cholesky_factor = function(mat) {
      if (!self$belongs(mat))
        cli::cli_abort("The input matrix {.arg mat} should be SPD.")
      M <- super$get_python_class()$cholesky_factor(mat = mat)
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
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$differential_cholesky_factor(diag(1, 3), A)
    #' }
    differential_cholesky_factor = function(tangent_vec, base_point) {
      if (!self$belongs(base_point))
        cli::cli_abort("The input matrix {.arg base_point} should be SPD.")
      super$get_python_class()$differential_cholesky_factor(
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
    #'   spd3 <- SPDMatrix(n = 3)
    #'   spd3$expm(diag(-1, 3))
    #' }
    expm = function(mat) {
      super$get_python_class()$expm(mat = mat)
    },

    #' @description Computes the differential of the matrix exponential.
    #'
    #' @return A matrix storing the differential of the matrix exponential on
    #'   SPD matrices at `base_point` applied to `tangent_vec`.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$differential_exp(diag(1, 3), A)
    #' }
    differential_exp = function(tangent_vec, base_point) {
      super$get_python_class()$differential_exp(
        tangent_vec = tangent_vec,
        base_point = base_point
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
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$inverse_differential_exp(diag(1, 3), A)
    #' }
    inverse_differential_exp = function(tangent_vec, base_point) {
      super$get_python_class()$inverse_differential_exp(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
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
    #'   spd3 <- SPDMatrix(n = 3)
    #'   spd3$logm(diag(1, 3))
    #' }
    logm = function(mat) {
      if (!self$belongs(mat))
        cli::cli_abort("The input matrix {.arg mat} should be SPD.")
      super$get_python_class()$logm(mat = mat)
    },

    #' @description Computes the differential of the matrix logarithm.
    #'
    #' @return A matrix storing the differential of the matrix logarithm on
    #'   SPD matrices at `base_point` applied to `tangent_vec`.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$differential_log(diag(1, 3), A)
    #' }
    differential_log = function(tangent_vec, base_point) {
      super$get_python_class()$differential_log(
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
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$inverse_differential_log(diag(1, 3), A)
    #' }
    inverse_differential_log = function(tangent_vec, base_point) {
      super$get_python_class()$inverse_differential_log(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
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
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$powerm(diag(1, 3), 2)
    #' }
    powerm = function(mat, power) {
      if (!self$belongs(mat))
        cli::cli_abort("The input matrix {.arg mat} should be SPD.")
      super$get_python_class()$powerm(
        mat = mat,
        power = power
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
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$differential_power(2, diag(1, 3), A)
    #' }
    differential_power = function(power, tangent_vec, base_point) {
      super$get_python_class()$differential_power(
        power = as.integer(power),
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
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$inverse_differential_power(2, diag(1, 3), A)
    #' }
    inverse_differential_power = function(power, tangent_vec, base_point) {
      super$get_python_class()$inverse_differential_power(
        power = as.integer(power),
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
      self$n <- super$get_python_class()$n
    }
  )
)

#' Class for the Manifold of Symmetric Positive Definite Matrices
#'
#' This function generates an instance of the class for the manifold of
#' symmetric positive definite matrices \eqn{\mathrm{SPD}(n)}.
#'
#' @author Yann Thanwerdas
#'
#' @param n An integer value specifying the number of rows and columns of the
#'   matrices.
#' @param ... Extra arguments to be passed to parent class constructors. See
#'   [`OpenSet`] and [`Manifold`] classes.
#'
#' @return An object of class [`SPDMatrices`].
#'
#' @family symmetric positive definite matrix classes
#' @export
#' @examples
#' if (reticulate::py_module_available("geomstats")) {
#'   spd3 <- SPDMatrix(n = 3)
#'   spd3
#' }
SPDMatrix <- function(n, ...) {
  SPDMatrices$new(n = n, ...)
}
