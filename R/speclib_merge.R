setMethod("merge", signature(x = "Speclib", y = "Speclib"),
          function(x, y, ...)
{
  if (dim(x)[2] != dim(y)[2])
    stop("Dimensions of Speclibs do not fit")

  wl <- wavelength(x)
  if (any(wl!=wavelength(y)))
    stop("Wavelengths differ")

  if (nrow(y@SI) == dim(y)[1])
  {
    if (nrow(x@SI) == dim(x)[1])
    {
      SI(x) <- rbind(SI(x),SI(y))
    } else {
      warning("x does not have proper SI definition. SI information will be lost")
    }
  } else {
    warning("y does not have proper SI definition. SI information will be lost")
  }
  ids <- c(idSpeclib(x), idSpeclib(y))
  spectra(x) <- as.matrix(rbind(spectra(x),spectra(y))) 
  idSpeclib(x) <- as.character(ids)
  
  return(x)
}
)