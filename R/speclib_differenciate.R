derivative.speclib <- function(
                                  x,
                                  m=1,
                                  method="sgolay",
                                  ...
                                 )
{
  if (!is.speclib(x))
    stop("x must be of class 'Speclib'")
    
  if (x@spectra@fromRaster)
    return(.blockwise(speclib_obj =  "x", pos = 1))
    
  res <- x
  mf <- FALSE  
  if (method=="finApprox")
  {
    mf <- TRUE
    spectra(res) <- t(apply(spectra(x), 1, FUN = function(single_spectrum, wl, nwl)
      {
        return(c((single_spectrum[-1] - single_spectrum[-nwl]) / (wl[-1] - wl[-nwl]), 0))
      }, wavelength(res), nbands(res)))
    
#     spectra <- as.matrix(spectra(res))
#     nwl     <- ncol(spectra)
#     n       <- nrow(spectra)
#     bandc   <- res$wavelength
#     deriv   <- spectra*0
#     
#     storage.mode(nwl)     <- "integer"
#     storage.mode(n)       <- "integer"
#     storage.mode(m)       <- "integer"
#     storage.mode(bandc)   <- "double"
#     storage.mode(deriv)   <- "double"
#     storage.mode(spectra) <- "double"
# 
#     external <- .Fortran("differenciate",
#                         nwl=nwl,
#                         n=n,
#                         m=m,
#                         y=spectra,
#                         bandcenter=bandc,
#                         derivation=deriv,
#                         PACKAGE="hsdar"
#                         )
#     external$derivation <- as.data.frame(external$derivation)
#     
#     spectra(res) <- external$derivation
    usagehistory(res) <- paste(m,". derivation using finite approximation",sep="")
  }
  if (method=="sgolay")
  {
    mf <- TRUE
    spectra(res) <- t(apply(spectra(x), 1, FUN = sgolayfilt, m = m,...))
    usagehistory(res) <- paste(m,". derivation using Savitzky-Golay filter",sep="")
  }
  if (!mf) stop("Specified method not found")
  
  
  return(res) 
}
