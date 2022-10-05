check_extra_params <- function(...) {
  dots <- rlang::list2(...)
  if (length(dots) > 0 && !rlang::is_named(dots))
    cli::cli_abort("All extra-arguments should be named arguments.")
}
