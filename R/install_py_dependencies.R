#' Install python dependencies including `tensorflow`, `tensorflow_probability`,
#' `qenspy`, and the dependencies of those packages. This function simply calls
#' `reticulate::py_install`; see the documentation of that function for more
#' information about how to set up Python environments.
#' 
#' @inheritParams reticulate::py_install
#'
#' @export
install_py_dependencies <- function(method = "auto", conda = "auto") {
  reticulate::py_install(
    c("tensorflow", "tensorflow_probability"),
    method = method,
    conda = conda,
    pip = TRUE)
  reticulate::py_install(
    "git+https://github.com/reichlab/qenspy@refactor_api",
    method = method,
    conda = conda,
    pip = TRUE,
    pip_options = "--force-reinstall")
}


# library(reticulate)
# method <- conda <- "auto"
# reticulate::py_install(
#   "/home/eray/research/epi/tools/qenspy",
#   method = method,
#   conda = conda,
#   pip = TRUE,
#   pip_options = "--force-reinstall")
