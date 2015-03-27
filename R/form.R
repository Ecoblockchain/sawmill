##' Calculate stem volume based on height, diameter and species
##' 
##' This function uses the approximation function of Franz et al. (1973) to
##' calculate the stem volume based on the species specific form number. It is
##' estimated in two steps: a first level of functions is derived by species and
##' diameter, then this set of functions enters the final approximation function
##' which depends on height alone.
##' 
##' At the moment, only beech and spruce are implemented.
##' @title Stem volume
##' @param d numeric vector with stem diameters in 1.3 m height in cm
##' @param h numeric vectors with height in m
##' @param species character
##' @return numeric vector with volume of the trees in cubic metres
##' @examples
##' volume(30:35, 20:25, "fagus_sylvatica")
##' @export
volume <- function(d, h, species = "fagus_sylvatica") {

  if (!is.numeric(d) | !is.numeric(h)) {
    stop("d and h have to be numeric")
  }

  if (length(d) != length(h)) {
    stop("d and h have to have the same length")
  }

  species_list <- c("fagus_sylvatica",
                    "picea_abies")
  
  if (!any(species_list == species)) {
    stop("unknown species")
  }
  
  species_coefs <- list(
    picea_abies = c(
      k11  = -3.5962
      ,k12 = 1.8021
      ,k13 = -0.2882
      ,k21 = 1.0625
      ,k22 = -0.1290
      ,k23 = 0.0353
      ,k31 = 0.1423
      ,k32 = -0.0583
      ,k33 = 0.0046)
    ,fagus_sylvatica = c(
      k11  = -2.7284
      ,k12 = 0.8376
      ,k13 = -0.1058
      ,k21 = 1.6228
      ,k22 = -0.2148
      ,k23 = 0.0289
      ,k31 = -0.0880
      ,k32 = 0.0326
      ,k33 = -0.0045
      )
    )
  cl2 <- species_coefs[[species]]

  cl1f <- function(d) {
    unname(c(
    cl2["k11"] + cl2["k12"] * log(d) + cl2["k13"] * log(d)^2
    ,cl2["k21"] + cl2["k22"] * log(d) + cl2["k23"] * log(d)^2
    ,cl2["k31"] + cl2["k32"] * log(d) + cl2["k33"] * log(d)^2
    ))
  }

  cl1 <- lapply(d, cl1f)

  formhf <- function(h, coef) {
    exp(coef[1] + coef[2] * log(h) + coef[3] * log(h)^2)
  }

  formh <- mapply(formhf, h, cl1)

  d^2 * pi/40000 * formh
}
