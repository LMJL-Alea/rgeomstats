#' Abstract Class for Manifolds
#'
#' @description An [R6::R6Class] object implementing the base [`Manifold`]
#'   class. In other words, a topological space that locally resembles Euclidean
#'   space near each point.
#'
#' @author Nina Miolane
Manifold <- R6::R6Class(
  classname = "Manifold",
  inherit = PythonClass,
  public = list(
    #' @field dim An integer value specifying the dimension of the manifold.
    dim = NULL,

    #' @field shape An integer vector specifying the shape of one element of the
    #'   manifold. Defaults to `NULL`.
    shape = NULL,

    #' @field metric A [RiemannianMetric] object specifying the metric to use on
    #'   the manifold. Defaults to `NULL`.
    metric = NULL,

    #' @field default_coords_type A string specifying the coordinate type.
    #'   Choices are `extrensic` or `intrinsic`. Dedaults to `intrinsic`.
    default_coords_type = NULL,

    #' @field default_point_type A string specifying the point type. Choices are
    #'   `vector` or `matrix`. It is automatically determined depending on the
    #'   manifold.
    default_point_type = NULL,

    #' @description The [`Manifold`] class constructor.
    #'
    #' @param dim An integer value specifying the dimension of the manifold.
    #' @param shape An integer vector specifying the shape of one element of the
    #'   manifold. Defaults to `NULL`.
    #' @param metric A [`RiemannianMetric`] object specifying the metric to use
    #'   on the manifold. Defaults to `NULL`.
    #' @param default_coords_type A string specifying the coordinate type.
    #'   Choices are `extrensic` or `intrinsic`. Defaults to `intrinsic`.
    #'
    #' @return An object of class [`Manifold`].
    initialize = function(dim, shape = NULL, metric = NULL, default_coords_type = "intrinsic") {
      dim <- as.integer(dim)
      if (!is.null(shape)) shape <- as.integer(shape)
      super$set_python_class(
        gs$geometry$manifold$Manifold(
          dim = dim,
          shape = shape,
          metric = metric,
          default_coords_type = default_coords_type
        )
      )
      private$set_fields()
    },

    #' @description Evaluates if a point belongs to the manifold.
    #'
    #' @param point An numeric array of shape `dim` specifying a point to be
    #'   checked.
    #' @param atol A numeric value specifying the absolute tolerance for
    #'   checking. Defaults to `gs$backend$atol`.
    #'
    #' @return A boolean that tells whether the input point belongs to the
    #'   manifold.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   mf <- SPDMatrices$new(n = 3)
    #'   A <- diag(1, 3)
    #'   mf$belongs(A)
    #'   B <- diag(-1, 3)
    #'   mf$belongs(B)
    #' }
    belongs = function(point, atol = gs$backend$atol) {
      super$get_python_class()$belongs(point, atol = atol)
    },

    #' @description Checks whether a vector is tangent at a base point.
    #'
    #' @param vector An numeric array of shape `dim` specifying a vector to be
    #'   checked.
    #' @param base_point An numeric array of shape `dim` specifying a base point
    #'   which belongs to the manifold.
    #' @param atol A numeric value specifying the absolute tolerance for
    #'   checking. Defaults to `gs$backend$atol`.
    #'
    #' @return A boolean that tells whether the input `vector` is ≥tangent to
    #'   the manifold at `base_point`.
    is_tangent = function(vector, base_point = NULL, atol = gs$backend$atol) {
      super$get_python_class()$is_tangent(
        vector = vector,
        base_point = base_point,
        atol = atol
      )
    },

    #' @description Projects a vector to a tangent space of the manifold.
    #'
    #' @param vector A (list of) numeric array(s) of shape `dim` specifying a
    #'   (list of) vector(s) to project on the manifold.
    #' @param base_point A (list of) numeric array(s) of shape `dim` specifying
    #'   a (list of) point(s) on the manifold. Defaults to `NULL`.
    #'
    #' @return A (list of) numeric array(s) of shape `dim` storing a (list of)
    #'   projected vector(s) that belong to the manifold.
    to_tangent = function(vector, base_point = NULL) {
      super$get_python_class()$to_tangent(
        vector = vector,
        base_point = base_point
      )
    },

    #' @description Samples random points on the manifold.
    #'
    #' @details If the manifold is compact, a uniform distribution is used.
    #'
    #' @param base_point An numeric array of shape `dim` specifying a base point
    #'   which belongs to the manifold.
    #' @param n_samples An integer value specifying the number of samples to be
    #'   drawn. Defaults to `1L`.
    #' @param bound A numeric value specifying the bound of the interval in
    #'   which to sample for non compact manifolds. Defaults to `1L`.
    #'
    #' @return A list of numeric arrays representing a sample of points on the
    #'   manifold.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   mf <- SPDMatrices$new(n = 3)
    #'   # mf$random_point(10) # TO DO: uncomment when bug fixed in gs
    #' }
    random_point = function(n_samples = 1, bound = 1.0) {
      super$get_python_class()$random_point(
        n_samples = as.integer(n_samples),
        bound = bound
      )
    },

    #' @description Regularizes a point to the canonical representation for the
    #'   manifold.
    #'
    #' @param point A numeric array of shape `dim` specifying a point which
    #'   belongs to the manifold.
    #'
    #' @return A numeric array of shape `dim` storing a regularized version of
    #'   the input `point`.
    regularize = function(point) {
      super$get_python_class()$regularize(
        point = point
      )
    },

    #' @description Sets the Riemannian Metric associated to the manifold.
    #'
    #' @param metric An object of class [`RiemannianMetric`].
    #'
    #' @return The [Manifold] class itself invisibly.
    set_metric = function(metric) {
      super$get_python_class()$metric(
        metric = metric
      )
      invisible(self)
    },

    #' @description Generates a random tangent vector.
    #'
    #' @param base_point An numeric array of shape `dim` specifying a base point
    #'   which belongs to the manifold.
    #' @param n_samples An integer value specifying the number of samples to be
    #'   drawn. Defaults to `1L`.
    #'
    #' @return A list of numeric arrays representing a sample of vectors that
    #'   are tangent to the manifold at `base_point`.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   mf <- SPDMatrices$new(n = 3)
    #'   mf$random_tangent_vec(diag(1, 3), 10)
    #' }
    random_tangent_vec = function(base_point, n_samples = 1) {
      if (!self$belongs(base_point))
        cli::cli_abort("The input matrix {.arg base_point} should be SPD.")
      n_samples <- as.integer(n_samples)
      array_res <- super$get_python_class()$random_tangent_vec(
        base_point = base_point,
        n_samples = n_samples
      )
      purrr::array_tree(array_res, margin = 1)
    }
  ),
  private = list(
    set_fields = function() {
      self$dim <- super$get_python_class()$dim
      self$shape <- super$get_python_class()$shape
      self$metric <- super$get_python_class()$metric
      self$default_coords_type <- super$get_python_class()$default_coords_type
      self$default_point_type <- super$get_python_class()$default_point_type
    }
  )
)
