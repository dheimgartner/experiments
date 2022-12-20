#' Load python modules from `./python` repo
#'
#' @param module module name without `.py` ending
#'
#' @return
#' @export
#'
#' @examples
#' trump <- python_import_module("trump")
#' trump$make("America")
#' trump$great("again")
python_import_module <- function(module) {
  path <- system.file("python", package = "experiments")
  reticulate::import_from_path(module, path = path, delay_load = TRUE)
}
