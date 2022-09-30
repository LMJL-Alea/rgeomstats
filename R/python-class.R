PythonClass <- R6::R6Class(
  classname = "PythonClass",
  public = list(
    set_python_class = function(py_class) {
      private$m_PythonClass = py_class
    },
    get_python_class = function() {
      private$m_PythonClass
    }
  ),
  private = list(
    m_PythonClass = NULL
  )
)
