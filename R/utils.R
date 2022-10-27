capture_extra_params <- function(...) {
  dots <- rlang::list2(...)
  if (length(dots) > 0 && !rlang::is_named(dots))
    cli::cli_abort("All extra-arguments should be named arguments.")
  dots
}

extract_class_name <- function(py_class) {
  nm <- py_class |>
    format() |>
    strsplit("\\.")
  nm <- nm[[1]]
  nm <- nm[length(nm)]
  substr(nm, 1, nchar(nm) - 1)
}

python_to_r_class <- function(py_class) {
  cls <- rlang::quo(extract_class_name(py_class))
  cls <- rlang::eval_tidy(cls$new())
  cls$set_python_class(py_class)
  cls
}

