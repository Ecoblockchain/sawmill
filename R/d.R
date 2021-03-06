## Functions for diameter calcution

##' Group diameters in diameter classes of given size
##'
##' This function takes a numeric vector of diameters and a group size
##' and classifies each diameter into a group.
##' @title Group Diameters
##' @param x numeric vector of diameters
##' @param s an integer for specifying group size
##' @return a \code{data.table} with the diameters, the group they
##' belong to and the respective upper group boundary
##' @import data.table
##' @examples
##' d <- rnorm(40, mean = 50, sd = 3)
##' diam_group(d)
##' @export
diam_group <- function(x, s = 1) {
  minx <- floor(min(x))
  maxx <- ceiling(max(x))
  groups <- seq(minx, maxx, by = s)
  groups1 <- groups[-1]
  groups2 <- groups[-length(groups)]
  m <- apply(sapply(groups1, function(g) x >= g), 1, function(r)
             which(!r)[1])
  data.table(diameter = x, group = m, ugb = groups2[m])[order(group)]
}

##' Calculate mean diameters in many different ways
##'
##' Provides some commonly used mean diameters for forest growth and
##' yield science.
##' @title Calculate Mean Diameters
##' @param x a vector of diameters
##' @param method "ar" for arithmetic, "qmd" for quadratic mean
##' diameter, "gfz" for Grundflaechenzentralstamm, "do" for dominant
##' diameter based on basal area of 100 largest trees per ha, "d200"
##' for dominant diameter based on basal area of 200 largest trees per
##' ha, and "dw" for Weise'sche Oberhoehe.
##' @param s class size (integer) 
##' @param a size of area in ha
##' @return a numeric vector with diameters
##' @import data.table
##' @examples
##' d <- rnorm(40, mean = 50, sd = 3)
##' diam(d)
##' @export
diam <- function(x, method = c("ar", "qmd", "gfz", "do", "d200", "dw"), s = 1, a = 1) {

  methods <- method[method %in% c("ar", "qmd", "gfz", "do", "d200", "dw")]

  if (length(methods) == 0) {
    stop("`method` can be 'ar', 'qmd', 'gfz', 'do', 'd200', or 'dw'.")
  }

  methods <- unique(methods)
  out <- numeric(0)

  dn <- function(x, n, a, m) {
      n <- floor(n * a)
      if(n >= length(x)) {
        n <- floor(0.75 * length(x))
        message(paste0("n adjusted to ", n, " for method'", m, "'."))
      }
      .x <- sort(x, decreasing = TRUE)[1:n]
      res <- sqrt(sum(.x^2)/n)
      out <<- c(out, res)
    }
    
  for (.method in methods) {

    if (.method == "ar") {
      res <- mean(x)
      out <- c(out, res)
    }

    if (.method == "qmd") {
      res <- sqrt(sum(x^2)/length(x))
      out <- c(out, res)
    }

    if (.method == "gfz") {
      ## basal area
      G <- sum(x^2 * pi/4)
      ## half of basal area
      G2 <- G/2
      ## next class boundary
      xk <- diam_group(x, s = s)
      Gk <- xk[,
               .(G = sum(diameter^2 * pi/4)),
               by = group]
      Gks <- Gk[, .(group, carea = cumsum(G))]
      ## group of relevant diameter
      m <- Gks[carea >= G2, group][1] - 1
      ## cumulative basal area until group
      Gmkc <- Gks[group == m, carea]
      ## width of group
      Gz <- Gk[group == m, G]
      ## lower boundary of group
      Gmd <- unique(xk[group == m, ugb])
      ## difference of halved G and cumulative basal area until group
      delta <- G2 - Gmkc
      ## add normalized delta to lower group boundary
      res <- Gmd + s * delta/Gz
      out <- c(out, res)
    }

    if (.method == "do") {
      dn(x, 100, a, "do")
    }

    if (.method == "d200") {
      dn(x, 200, a, "d200")
    }

    if (.method == "dw") {
      n <- floor(length(x) * a * 0.2)
      dn(x, n, a, "dw")
    }
  }

  names(out) <- methods
  out
}
