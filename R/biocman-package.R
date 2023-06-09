#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

biocman_env <- new.env()

.onLoad <- function(lib, pkg) {

  file_name <- system.file("extdata/trained_vae_1.to", package = "biocman")
  biocman_env$biocman_vae <- torch::torch_load(file_name)

}
