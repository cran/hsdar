noiseFiltering <- function(
                            x,
                            method="mean",
                            ...
                           )
{
  if (x@spectra@fromRaster)
    return(.blockwise(speclib_obj =  "x", pos = 1))
  
  predefinedmethod <- FALSE
  if (!is.speclib(x))
    stop("x must be of class 'Speclib'")
    
  if (!x@continuousdata)
    stop("Filtering is only useful for continuous spectra")
  
  setmask <- FALSE  
  
  if (!is.null(attr(x, "setmask")))
  {
    if (attr(x, "setmask"))
    {
      setmask <- TRUE
      x <- interpolate.mask(x)
    }
  }
  
  res <- x
  
  pp <- .process_parallel()
  i <- 0
  
  if (method=="sgolay")
  {
    if (!pp[[1]])
    {
      spectra(res) <- t(apply(spectra(x), 1, FUN = sgolayfilt, ...))
    } else {
      `%op%` <- pp[[2]]
      y <- spectra(x)
      y_smoothed <- foreach::foreach(i=1:nrow(y), .combine = 'rbind') %op%
      {
        sgolayfilt(y[i,], ...)
      }
      spectra(res) <- y_smoothed
      .restoreParallel()
    }
    usagehistory(res) <- paste("Filtered with Savitzky-Golay filter")
    predefinedmethod <- TRUE
  }
  
  if (method=="lowess")
  {
    lowessFUN <- function(y, x, ...) lowess(x=x, y=y, ...)$y
    
    wavelength(res) <- lowess(x=wavelength(x), y=spectra(x)[1,], ...)$x
    if (!pp[[1]])
    {
      spectra(res) <- t(apply(spectra(x), 1, FUN = lowessFUN, 
                              x=wavelength(x), ...))
    } else {
      `%op%` <- pp[[2]]
      y <- spectra(x)
      wav <- wavelength(x)
      y_smoothed <- foreach::foreach(i=1:nrow(y), .combine = 'rbind') %op%
      {
        lowessFUN(y[i,], wav, ...)
      }
      spectra(res) <- y_smoothed
      .restoreParallel()
    }
    usagehistory(res) <- paste("Filtered with lowess function")
    predefinedmethod <- TRUE
  } 
  if (method=="spline")
  {
    splineFUN <- function(y, x, ...) spline(x=x, y=y, ...)$y
    
    wavelength(res) <- spline(x=wavelength(x), y=spectra(x)[1,], ...)$x
    if (!pp[[1]])
    {
      spectra(res) <- t(apply(spectra(x), 1, FUN = splineFUN, 
                              x=wavelength(x), ...))
    } else {
      `%op%` <- pp[[2]]
      y <- spectra(x)
      wav <- wavelength(x)
      y_smoothed <- foreach::foreach(i=1:nrow(y), .combine = 'rbind') %op%
      {
        splineFUN(y[i,], wav, ...)
      }
      spectra(res) <- y_smoothed
      .restoreParallel()
    }
    usagehistory(res) <- paste("Filtered with spline function")
    predefinedmethod <- TRUE
  } 
  if (any(method==c("mean","mean_gliding")))
  {
    spectra  <- spectra(res)
    
    spectra(res) <- meanfilter(spectra, ...)
    usagehistory(res) <- paste("Filtered with meanfilter")
    predefinedmethod <- TRUE
  }
  
  if (!predefinedmethod)
  {
    spectra(res) <- t(apply(spectra(x), 1, FUN = method, ...))
    usagehistory(res) <- paste("Filtered with '", method, "'")
  }
  
  if (setmask) mask(res) <- attr(res, "dropped")*.ConvWlBwd(res@wlunit)
  return(res)            
}

meanfilter <- function(spectra, p=5)
{
  if (is.speclib(spectra))
  {
    backup <- spectra
    spectra <- spectra(spectra)
  } else {
    backup <- NULL
  } 
   
  gliding  <- FALSE
  spectra  <- as.matrix(spectra)
  nwl      <- ncol(spectra)
  n        <- nrow(spectra)
  smoothed <- spectra*0
  
  storage.mode(nwl)      <- "integer"
  storage.mode(n)        <- "integer"
  storage.mode(p)        <- "integer"
  storage.mode(spectra)  <- "double"
  storage.mode(smoothed) <- "double"  

  if (!gliding)
  {
    external <- .Fortran("meanfilter",
                        nwl=nwl,
                        n=n,
                        p=p,
                        y=spectra,
                        smoothed=smoothed,
                        PACKAGE="hsdar"
                        )
  } else {
    external <- .Fortran("gliding_meanfilter",
                        nwl=nwl,
                        n=n,
                        p=p,
                        y=spectra,
                        smoothed=smoothed,
                        PACKAGE="hsdar"
                        )
  }
  
  external$smoothed <- as.matrix(as.data.frame(external$smoothed))
  if (!is.null(backup))
  {
    spectra(backup) <- external$smoothed
    usagehistory(backup) <- paste("Filtered with meanfilter")
  } else {
    backup <- external$smoothed
  }
  return(backup)
}
smoothSpeclib <- noiseFiltering