#' Abstract Class for Riemannian Metrics
#'
#' @description An [R6::R6Class] object implementing the base
#'   [`RiemannianMetric`] class. This is an abstract class for Riemannian and
#'   pseudo-Riemannian metrics which are the associated Levi-Civita connection
#'   on the tangent bundle.
#'
#' @param base_point A numeric array of shape `dim` specifying a point on
#'   the manifold. Defaults to `NULL`.
#' @param vector A numeric array of shape `dim` specifying a vector.
#'
#' @author Nina Miolane
RiemannianMetric <- R6::R6Class(
  classname = "RiemannianMetric",
  inherit = Connection,
  public = list(
    #' @field signature An integer vector specifying the signature of the
    #'   metric.
    signature = NULL,

    #' @description The [`RiemannianMetric`] class constructor.
    #'
    #' @param dim An integer value specifying the dimension of the manifold.
    #' @param shape An integer vector specifying the shape of one element of the
    #'   manifold. Defaults to `NULL`.
    #' @param signature An integer vector specifying the signature of the
    #'   metric. Defaults to `c(dim, 0L)`.
    #' @param default_coords_type A string specifying the coordinate type.
    #'   Choices are `extrensic` or `intrinsic`. Defaults to `intrinsic`.
    #'
    #' @return An object of class [`RiemannianMetric`].
    initialize = function(dim, shape = NULL, signature = NULL,
                          default_coords_type = "intrinsic") {
      dim <- as.integer(dim)
      if (!is.null(shape)) shape <- dim
      if (!is.null(signature)) signature <- as.integer(signature)
      super$set_python_class(
        gs$geometry$riemannian_metric$RiemannianMetric(
          dim = dim,
          shape = shape,
          signature = signature,
          default_coords_type = default_coords_type
        )
      )
      private$set_fields()
    },

    #' @description Metric matrix at the tangent space at a base point.
    #'
    #' @return A numeric array of shape `dim x dim` storing the inner-product
    #'   matrix.
    metric_matrix = function(base_point = NULL) {
      super$get_python_class$metric_matrix(base_point)
    },

    #' @description Inner co-product matrix at the cotangent space at a base
    #'   point. This represents the cometric matrix, i.e. the inverse of the
    #'   metric matrix.
    #'
    #' @return A numeric array of shape `dim x dim` storing the inverse of the
    #'   inner-product matrix.
    cometric_matrix = function(base_point = NULL) {
      super$get_python_class$cometric_matrix(base_point)
    },

    #' @description Compute derivative of the inner prod matrix at base point.
    #'
    #' @return A numeric array of shape `dim x dim` storing the derivative of
    #'   the inverse of the inner-product matrix.
    inner_product_derivative_matrix = function(base_point = NULL) {
      super$get_python_class$inner_product_derivative_matrix(base_point)
    },

    #' @description Inner product between two tangent vectors at a base point.
    #'
    #' @param tangent_vec_a A numeric array of shape `dim` specifying a tangent
    #'   vector at base point.
    #' @param tangent_vec_b A numeric array of shape `dim` specifying a tangent
    #'   vector at base point.
    #'
    #' @return A scalar value representing the inner product between the two
    #'   input tangent vectors at the input base point.
    inner_product = function(tangent_vec_a, tangent_vec_b, base_point = NULL) {
      super$get_python_class$inner_product(
        tangent_vec_a = tangent_vec_a,
        tangent_vec_b = tangent_vec_b,
        base_point = base_point
      )
    },

    #' @description Computes inner coproduct between two cotangent vectors at
    #'   base point. This is the inner product associated to the cometric
    #'   matrix.
    #'
    #' @param cotangent_vec_a A numeric array of shape `dim` specifying a
    #'   cotangent vector at base point.
    #' @param cotangent_vec_b A numeric array of shape `dim` specifying a
    #'   cotangent vector at base point.
    #'
    #' @return A scalar value representing the inner coproduct between the two
    #'   input cotangent vectors at the input base point.
    inner_coproduct = function(cotangent_vec_a, cotangent_vec_b, base_point = NULL) {
      super$get_python_class$inner_coproduct(
        cotangent_vec_a = cotangent_vec_a,
        cotangent_vec_b = cotangent_vec_b,
        base_point = base_point
      )
    },

    #' @description Computes the Hamiltonian energy associated to the cometric.
    #'   The Hamiltonian at state \eqn{(q, p)} is defined by \deqn{H(q, p) =
    #'   \frac{1}{2} \langle p, p \rangle_q,} where \eqn{\langle \cdot, \cdot
    #'   \rangle_q} is the cometric at \eqn{q}.
    #'
    #' @param state A list with two components: (i) a numeric array of shape
    #'   `dim` specifying the *position* which is a point on the manifold and
    #'   (ii) a numeric array of shape `dim` specifying the *momentum* which is
    #'   a cotangent vector.
    #'
    #' @return A numeric value representing the Hamiltonian energy at `state`.
    hamiltonian = function(state) {
      super$get_python_class$hamiltonian(state)
    },

    #' @description Computes the square of the norm of a vector. Squared norm of
    #'   a vector associated to the inner product at the tangent space at a base
    #'   point.
    #'
    #' @return A numeric value representing the squared norm of the input
    #'   vector.
    squared_norm = function(vector, base_point = NULL) {
      super$get_python_class$squared_norm(
        vector = vector,
        base_point = base_point
      )
    },

    #' @description Computes the norm of a vector associated to the inner
    #'   product at the tangent space at a base point.
    #'
    #' @details This only works for positive-definite Riemannian metrics and
    #'   inner products.
    #'
    #' @return A numeric value representing the norm of the input vector.
    norm = function(vector, base_point = NULL) {
      super$get_python_class$norm(
        vector = vector,
        base_point = base_point
      )
    },

    #' @description Normalizes a tangent vector at a given point.
    #'
    #' @return A numeric array of shape `dim` storing the normalized versio of
    #'   the input tangent vector.
    normalize = function(vector, base_point = NULL) {
      super$get_python_class$normalize(
        vector = vector,
        base_point = base_point
      )
    },

    #' @description Generates a random unit tangent vector at a given point.
    #'
    #' @param n_vectors An integer value specifying the number of vectors to be
    #'   generated at `base_point`. For vectorization purposes, `n_vectors` can
    #'   be greater than 1 *iff* `base_point` corresponds to a single point.
    #'   Defaults to `1L`.
    #'
    #' @return A numeric array of shape `c(n_vectors, dim)` storing random unit
    #'   tangent vectors at `base_point`.
    random_unit_tangent_vec = function(base_point = NULL, n_vectors = 1) {
      n_vectors <- as.integer(n_vectors)
      super$get_python_class$random_unit_tangent_vec(
        base_point = base_point,
        n_vectors = n_vectors
      )
    },

    #' @description Squared geodesic distance between two points.
    #'
    #' @param point_a A numeric array of shape `dim` on the manifold.
    #' @param point_b A numeric array of shape `dim` on the manifold.
    #' @param ... Extra parameters to be passed to the `$log()` method of the
    #'   parent [`Connection`] class.
    #'
    #' @return A numeric value storing the squared geodesic distance between the
    #'   two input points.
    squared_dist = function(point_a, point_b, ...) {
      check_extra_params(...)
      super$get_python_class$squared_dist(
        point_a = point_a,
        point_b = point_b,
        ...
      )
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
      self$signature <- super$get_python_class()$signature
    }
  )
)
