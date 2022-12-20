setup_venv <- function(envname = "experiments",
                       packages = c("pandas", "selenium",
                                    "git+https://github.com/dheimgartner/tcsscraper@v0.0.3"),
                       python = "python3.8",
                       ...) {
  reticulate::virtualenv_create(envname, python = python, ...)
  purrr::map(packages, function(x) reticulate::virtualenv_install(envname, x))
}
