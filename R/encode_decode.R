#' Encode bioclim data to biocman data (5 bioclim manifold variables)
#'
#' @param x A matrix, data.frame, or SpatRaster. Columns are assumed to be in order from
#' BIO01 to BIO19. Result will be incorrect if they are not.
#' @param standardised Are the bioclim variables standardised? If so, this argument
#' should be `TRUE` and they must have been standardised using the mean and SD of the
#' WorldClim 2.1 2.5 minute data. These values can be found in `bioclim_scaling`
#' @param device A [torch::torch_device()], on which to run the encoder model, default: `"cpu"`
#'
#' @return A matrix of biocman variables (5 bioclimatic manifold variables). Rows
#' correspond to rows in `x`. If `x` is a `SpatRaster`, the result will also be a
#' `SpatRaster`.
#' @export
biocman_encode <- function(x, standardised = FALSE, device = "cpu") {

  if(inherits(x, "SpatRaster")) {
    df <- terra::as.data.frame(x, xy = TRUE, na.rm = FALSE)
    dat_processed <- as.matrix(df[ , -which(colnames(df) %in% c("x", "y"))])
    spatraster <- TRUE
  } else {
    dat_processed <- x
  }
  dat_processed <- dat_processed[ , c(1, 10, 11, 12, 13, 14,
                                      15, 16, 17, 18, 19, 2,
                                      3, 4, 5, 6, 7, 8, 9)]

  if(!standardised) {
    dat_processed <- t((t(dat_processed) - bioclim_scaling$means) / bioclim_scaling$sds)
  }

  mod <- biocman_env$biocman_vae$encoder$to(device = device)

  c_null <- torch::torch_zeros(nrow(dat_processed), 1, device = device) ## null conditioning data
  bioman <- as.matrix(mod(c_null, torch::torch_tensor(dat_processed, device = "device"))$means$cpu())
  bioman <- bioman[ , -non_manifold_dim_nums]

  colnames(bioman) <- c("BIOMAN1", "BIOMAN2", "BIOMAN3", "BIOMAN4", "BIOMAN5")

  if(spatraster) {
    bioman <- terra::rast(cbind(as.matrix(df[ , c("x", "y")]), bioman),
                          type = "xyz",
                          crs = terra::crs(x),
                          extent = terra::ext(x))
  }

  bioman

}

#' Decode biocman data to bioclim data (19 bioclimatic variables)
#'
#' @param x A matrix, data.frame, or SpatRaster. Columns are assumed to be in order from
#' BIOMAN1 to BIOMAN5.  Result will be incorrect if they are not.
#' @param standardised Should the returned bioclim variables be in standardised
#' form (the default)? If `FALSE` the variables will be back-transformed to the
#' original scale of the bioclim variables
#' @param device A [torch::torch_device()], on which to run the decoder model, default: `"cpu"`
#'
#' @return A matrix of bioclim variables (19 bioclimatic variables). Rows
#' correspond to rows in `x`. If `x` is a `SpatRaster`, the result will also be a
#' `SpatRaster`.
#' @export
biocman_decode <- function(x, standardised = TRUE, device = "cpu") {

  if(inherits(x, "SpatRaster")) {
    df <- terra::as.data.frame(x, xy = TRUE, na.rm = FALSE)
    mat <- as.matrix(df[ , -which(colnames(df) %in% c("x", "y"))])
    spatraster <- TRUE
  } else {
    mat <- as.matrix(x)
  }

  dat_processed <- matrix(0, ncol = 16, nrow = nrow(mat))
  dat_processed[ , -non_manifold_dim_nums] <- mat

  mod <- biocman_env$biocman_vae$decoder$to(device = device)

  c_null <- torch::torch_zeros(nrow(dat_processed), 1, device = device) ## null conditioning data

  bioman <- as.matrix(mod(c_null, torch::torch_tensor(dat_processed, device = "device"))$cpu())

  if(!standardised) {
    bioman <- t((t(bioman) * bioclim_scaling$sds) + bioclim_scaling$means)
  }

  colnames(bioman) <- c("BIO01", "BIO10", "BIO11", "BIO12", "BIO13", "BIO14",
                        "BIO15", "BIO16", "BIO17", "BIO18", "BIO19", "BIO02",
                        "BIO03", "BIO04", "BIO05", "BIO06", "BIO07", "BIO08",
                        "BIO09")
  bioman <- bioman[ , order(colnames(bioman))]

  if(spatraster) {
    bioman <- terra::rast(cbind(as.matrix(df[ , c("x", "y")]), bioman),
                          type = "xyz",
                          crs = terra::crs(x),
                          extent = terra::ext(x))
  }

  bioman

}
