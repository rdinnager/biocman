#' Return the 5 bioclim manifold variables in a raster format
#'
#' @param return_type Return format - One of `"rast"` (`terra` package: [terra::rast()]),
#' `"raster"` (`raster` package: [raster::stack()]), or `"stars"` (`stars` package: [stars::st_as_stars()])
#'
#' @return An object of `return_type` with the global raster data for biocman
#' @export
#'
#' @examples
#' biocman <- get_data()
#' ## plot 1st manifold variable
#' plot(biocman[[1]])
get_data <- function(return_type = c("rast", "raster", "stars")) {

  return_type <- match.arg(return_type)

  if(return_type == "raster") {
    if(!requireNamespace("raster", quietly = TRUE)) {
      stop('return_type = "raster" requires the raster package to be installed.')
    }
  }

  tif_file <- list.files(system.file("extdata", package = "biocman"),
                         pattern = ".tif",
                         full.names = TRUE)

  if(return_type == "stars") {
    if(!requireNamespace("stars", quietly = TRUE)) {
      stop('return_type = "stars" requires the stars package to be installed.')
    }
    return(stars::read_stars(tif_file))
  }

  rast_ob <- do.call(c, lapply(tif_file, terra::rast))

  if(return_type == "raster") {
    rast_ob <- raster::stack(rast_ob)
  }

  rast_ob

}

#' #' Extract the 5 bioclim manifold variables for a set of point coordinates
#' #'
#' #' @param x Point coordinates either as an `sf` object, a `SpatVector` object,
#' #' or a `matrix` or `data.frame` with latitude and longitude coordinates, in
#' #' which case you must also specify the `coords` argument.
#' #' @param coords A length 2 character vector giving the column names for longitude and
#' #' latitude, in that order.
#' #' @param return_type The desired return type. Can be `"tibble"`m `"sf"` or `"SpatVector"`
#' #'
#' #' @return An object of type `return_type`, containing the values for the 5
#' #' biocman variables at the coordinates in `x`
#' #' @export
#' #'
#' #' @examples
#' extract_data <- function(x, return_type = c("tibble", "SpatVector", "sf")) {
#'
#' }
