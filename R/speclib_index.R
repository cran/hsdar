setMethod("[", "Speclib",
          function(x, i, j, ...)
{
  dots <- list(...)
  upduh <- !any(names(dots) == "usagehistory")
  if (missing(i)) 
  {
    tmp <- spectra(x, j = j)
    if (nspectra(x) == 1)
    { 
      x@spectra@fromRaster <- FALSE      
      spectra(x) <- matrix(tmp, ncol = length(tmp))
    } else {
      x@spectra@fromRaster <- FALSE
      spectra(x) <- tmp
    }
    wavelength(x) <- wavelength(x)[j]
    if (!is.null(attr(x, "bandnames")))
      bandnames(x) <- bandnames(x)[j]
    if (length(fwhm(x)) > 1)
      fwhm(x) <- fwhm(x)[j]
    
    if (upduh)
      usagehistory(x) <- "Subsetting speclib (spectral dimension)"
    return(x)
  } 
  if (missing(j))
  {
    tmp <- spectra(x, i = i) 
    if (class(tmp) == "numeric")
      tmp <- matrix(tmp, ncol = if (nbands(x) > 1) length(tmp) else 1)
    x@spectra@fromRaster <- FALSE
    spectra(x) <- tmp
    idSpeclib(x) <- as.character(idSpeclib(x)[i])
    at_x <- SI(x, i = i)
    if (! class(at_x) %in% c("matrix", "data.frame"))
    {
      at_x <- data.frame(x = at_x)
      names(at_x) <- names(SI(x))
    }
    SI(x) <- at_x   

    if (upduh)
      usagehistory(x) <- "Subsetting speclib (sample dimension)"
    return(x)
  }
  tmp <- spectra(x, i = i, j = j)
  if (class(tmp) == "numeric")
  {
    ncols <- sum(rep.int(1, nbands(x))[j])
    nrows <- sum(rep.int(1, nspectra(x))[i])
    tmp <- matrix(tmp, ncol = ncols, nrow = nrows)
  }
  x@spectra@fromRaster <- FALSE
  spectra(x) <- tmp
  wavelength(x) <- wavelength(x)[j]
  if (!is.null(attr(x, "bandnames")))
    bandnames(x) <- bandnames(x)[j]
  if (length(fwhm(x)) > 1)
    fwhm(x) <- fwhm(x)[j]
  idSpeclib(x) <- as.character(idSpeclib(x)[i])

  at_x <- SI(x, i = i)
  if (! class(at_x) %in% c("matrix", "data.frame"))
  {
    at_x <- data.frame(x = at_x)
    names(at_x) <- names(SI(x))
  }
  SI(x) <- at_x   

  if (upduh)
    usagehistory(x) <- "Subsetting speclib (spectral and sample dimensions)"
  return(x)
})


setMethod("[", "Specfeat",
          function(x, i, j, ...)
{
  x_new <- callNextMethod(x, i, j, ...)
  FWL <- as.numeric(substr(names(x@features), 2, nchar(names(x@features))))
  tmp <- x_new
  class(tmp) <- "Speclib"
  x_new_specfeat <- specfeat(tmp, FWL)
  x_new@features      <- x_new_specfeat@features
  x_new@featureLimits <- x_new_specfeat@featureLimits
#   wl <- .get.feature.wavelength(x)
#   rep <- .get.rep.feature.parts(wl, x)
#   
#   if (missing(i)) 
#   {    
#     for (k in 1:length(rep$matches))
#         rep$matches[[k]] <- rep$matches[[k]][,j]     
#   } else {
#     if (missing(j))
#     {
#       for (k in 1:length(rep$matches))
#         rep$matches[[k]] <- rep$matches[[k]][i,]          
#     } else {
#       for (k in 1:length(rep$matches))
#         rep$matches[[k]] <- rep$matches[[k]][i,j]     
#     }
#   }
#   for (m in 1:length(rep$matches))
#   {
#     for (k in 1:length(x@features[[i]]))
#     {
#       xval <- which(x@wavelength==x@features[[m]][[k]]$x1)
#       xval <- x@wavelength[xval:(length(x@features[[m]][[k]]$y)+xval-1)]
#       y    <- xval>=limits[2*(m-1)+1] & xval<=limits[2*(m-1)+2]
#       
#       if (any(y))
#       {
#         x_new@features[[m]][[k]]$y  <- x@features[[m]][[k]]$y[y]
#         x_new@features[[m]][[k]]$x1 <- xval[y][1]
#       } else {
#         x_new@features[[m]][[k]]$y  <- NaN
#         x_new@features[[m]][[k]]$x1 <- limits[2*(m-1)+1]
#       }      
#     }
#   }
  return(x_new)
})