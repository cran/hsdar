as.hyperSpec <- function(object)
{
  if (!requireNamespace("hyperSpec", quietly = TRUE))
    stop("Library 'hyperSpec' is required to convert object to hyperSpec-class")

  spc <- new("hyperSpec", spc = spectra(object), wavelength = wavelength(object), data = SI(object))
  
  return(spc)
}
