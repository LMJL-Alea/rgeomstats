% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spd-metric-affine.R
\name{SPDMetricAffine}
\alias{SPDMetricAffine}
\title{Class for the Affine Metric on the Manifold of Symmetric Positive Definite
Matrices}
\description{
An \link[R6:R6Class]{R6::R6Class} object implementing the \code{\link{SPDMetricAffine}}
class. This is the class for the affine-invariant metric on the SPD
manifold \insertCite{thanwerdas2019affine}{rgeomstats}.
}
\references{
\insertAllCited{}
}
\author{
Yann Thanwerdas
}
\section{Super classes}{
\code{rgeomstats::PythonClass} -> \code{\link[rgeomstats:Connection]{rgeomstats::Connection}} -> \code{\link[rgeomstats:RiemannianMetric]{rgeomstats::RiemannianMetric}} -> \code{SPDMetricAffine}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{n}}{An integer value specifying the shape of the matrices: \eqn{n
\times n}.}

\item{\code{power_affine}}{An integer value specifying the power transformation
of the classical SPD metric.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SPDMetricAffine-new}{\code{SPDMetricAffine$new()}}
\item \href{#method-SPDMetricAffine-clone}{\code{SPDMetricAffine$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="PythonClass" data-id="get_python_class"><a href='../../rgeomstats/html/PythonClass.html#method-PythonClass-get_python_class'><code>rgeomstats::PythonClass$get_python_class()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="PythonClass" data-id="set_python_class"><a href='../../rgeomstats/html/PythonClass.html#method-PythonClass-set_python_class'><code>rgeomstats::PythonClass$set_python_class()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="Connection" data-id="christoffels"><a href='../../rgeomstats/html/Connection.html#method-Connection-christoffels'><code>rgeomstats::Connection$christoffels()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="Connection" data-id="curvature"><a href='../../rgeomstats/html/Connection.html#method-Connection-curvature'><code>rgeomstats::Connection$curvature()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="Connection" data-id="curvature_derivative"><a href='../../rgeomstats/html/Connection.html#method-Connection-curvature_derivative'><code>rgeomstats::Connection$curvature_derivative()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="Connection" data-id="directional_curvature"><a href='../../rgeomstats/html/Connection.html#method-Connection-directional_curvature'><code>rgeomstats::Connection$directional_curvature()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="Connection" data-id="directional_curvature_derivative"><a href='../../rgeomstats/html/Connection.html#method-Connection-directional_curvature_derivative'><code>rgeomstats::Connection$directional_curvature_derivative()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="Connection" data-id="exp"><a href='../../rgeomstats/html/Connection.html#method-Connection-exp'><code>rgeomstats::Connection$exp()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="Connection" data-id="geodesic"><a href='../../rgeomstats/html/Connection.html#method-Connection-geodesic'><code>rgeomstats::Connection$geodesic()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="Connection" data-id="geodesic_equation"><a href='../../rgeomstats/html/Connection.html#method-Connection-geodesic_equation'><code>rgeomstats::Connection$geodesic_equation()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="Connection" data-id="injectivity_radius"><a href='../../rgeomstats/html/Connection.html#method-Connection-injectivity_radius'><code>rgeomstats::Connection$injectivity_radius()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="Connection" data-id="ladder_parallel_transport"><a href='../../rgeomstats/html/Connection.html#method-Connection-ladder_parallel_transport'><code>rgeomstats::Connection$ladder_parallel_transport()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="Connection" data-id="log"><a href='../../rgeomstats/html/Connection.html#method-Connection-log'><code>rgeomstats::Connection$log()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="Connection" data-id="parallel_transport"><a href='../../rgeomstats/html/Connection.html#method-Connection-parallel_transport'><code>rgeomstats::Connection$parallel_transport()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="closest_neighbor_index"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-closest_neighbor_index'><code>rgeomstats::RiemannianMetric$closest_neighbor_index()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="cometric_matrix"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-cometric_matrix'><code>rgeomstats::RiemannianMetric$cometric_matrix()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="diameter"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-diameter'><code>rgeomstats::RiemannianMetric$diameter()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="dist"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-dist'><code>rgeomstats::RiemannianMetric$dist()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="dist_broadcast"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-dist_broadcast'><code>rgeomstats::RiemannianMetric$dist_broadcast()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="dist_pairwise"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-dist_pairwise'><code>rgeomstats::RiemannianMetric$dist_pairwise()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="hamiltonian"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-hamiltonian'><code>rgeomstats::RiemannianMetric$hamiltonian()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="inner_coproduct"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-inner_coproduct'><code>rgeomstats::RiemannianMetric$inner_coproduct()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="inner_product"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-inner_product'><code>rgeomstats::RiemannianMetric$inner_product()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="inner_product_derivative_matrix"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-inner_product_derivative_matrix'><code>rgeomstats::RiemannianMetric$inner_product_derivative_matrix()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="metric_matrix"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-metric_matrix'><code>rgeomstats::RiemannianMetric$metric_matrix()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="norm"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-norm'><code>rgeomstats::RiemannianMetric$norm()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="normal_basis"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-normal_basis'><code>rgeomstats::RiemannianMetric$normal_basis()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="normalize"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-normalize'><code>rgeomstats::RiemannianMetric$normalize()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="random_unit_tangent_vec"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-random_unit_tangent_vec'><code>rgeomstats::RiemannianMetric$random_unit_tangent_vec()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="sectional_curvature"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-sectional_curvature'><code>rgeomstats::RiemannianMetric$sectional_curvature()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="squared_dist"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-squared_dist'><code>rgeomstats::RiemannianMetric$squared_dist()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rgeomstats" data-topic="RiemannianMetric" data-id="squared_norm"><a href='../../rgeomstats/html/RiemannianMetric.html#method-RiemannianMetric-squared_norm'><code>rgeomstats::RiemannianMetric$squared_norm()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SPDMetricAffine-new"></a>}}
\if{latex}{\out{\hypertarget{method-SPDMetricAffine-new}{}}}
\subsection{Method \code{new()}}{
The \code{\link{SPDMetricAffine}} class constructor.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SPDMetricAffine$new(n, power_affine = 1, py_cls = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n}}{An integer value specifying the shape of the matrices: \eqn{n
\times n}.}

\item{\code{power_affine}}{An integer value specifying the power transformation
of the classical SPD metric. Defaults to \code{1L}.}

\item{\code{py_cls}}{A Python object of class \code{SPDMetricAffine}. Defaults to
\code{NULL} in which case it is instantiated on the fly using the other
input arguments.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An object of class \code{\link{SPDMetricAffine}}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SPDMetricAffine-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SPDMetricAffine-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SPDMetricAffine$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
