spectralInterpolation <- function(x, sensor)
{
  if (x@spectra@fromRaster)
    return(.blockwise(speclib_obj =  "x", pos = 1))
    
  if (any(toupper(names(sensor))=="FWHM"))
  {
    fwhm <- sensor[, which(toupper(names(sensor))=="FWHM")]
  } else {
    fwhm <- if (pmatch("FWHM", toupper(names(sensor)), nomatch = 0)==0) NULL else sensor[, pmatch("FWHM", toupper(names(sensor)))]
  }
  if (any(toupper(names(sensor))=="CENTER"))
  {
    centerwl <- sensor[, which(toupper(names(sensor))=="CENTER")]
  } else {
    centerwl <- if (pmatch("CENTER", toupper(names(sensor)), nomatch = 0)==0) NULL else sensor[, pmatch("CENTER", toupper(names(sensor)))]
  }
  if (any(c(is.null(fwhm), is.null(centerwl))))
  {
    if (any(toupper(names(sensor))=="LB"))
    {
      lb <- sensor[, which(toupper(names(sensor))=="LB")] 
    } else {
      lb <- if (pmatch("LB", toupper(names(sensor)), nomatch = 0)==0) NULL else sensor[, pmatch("LB", toupper(names(sensor)))]
    }
    if (any(toupper(names(sensor))=="UB"))
    {
      ub <- sensor[, which(toupper(names(sensor))=="UB")] 
    } else {
      ub <- if (pmatch("UB", toupper(names(sensor)), nomatch = 0)==0) NULL else sensor[, pmatch("UB", toupper(names(sensor)))]
    }
    if (any(c(is.null(lb), is.null(ub))))
    {    
      lb<-sensor[,1]
      ub<-sensor[,2]
    }
    centerwl <- lb + (ub - lb)/2
    fwhm <- (centerwl - lb) * 2
  } else {
    lb <- centerwl - fwhm/2
    ub <- centerwl + fwhm/2
  }
  sensor <- data.frame(lb = lb, ub = ub)
  band_means <- rowMeans(as.matrix(sensor))

  res <- .Fortran("inter",
                  b1 = as.double(wavelength(x)),
                  n_b1 = as.integer(nbands(x)),
                  b2 = as.double(band_means),
                  n_b2 = as.integer(length(band_means)),
                  refl_in = as.double(spectra(x)),
                  refl_out = as.double(matrix(0, ncol = length(band_means), nrow = nspectra(x))),
                  nspectra = as.integer(nspectra(x)),
                  PACKAGE="hsdar"
  )

  res <- res$refl_out
  
  x_new <- x
  wavelength(x_new) <- band_means
  fwhm(x_new) <- 2* (band_means - sensor$lb)
  spectra(x_new) <- matrix(res, ncol = length(band_means), nrow = nspectra(x))
  usagehistory(x_new) <- paste("Interpolated spectra to user defined channels")
  return(x_new)
}
