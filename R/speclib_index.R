setMethod("[", "Speclib",
          function(x, i, j, ...)
{
  if (missing(i)) 
  {
    spectra(x) <- spectra(x)[,j]
    wavelength(x) <- wavelength(x)[j]
    return(x)
  } 
  if (missing(j))
  {
    spectra(x) <- spectra(x)[i,]
    idSpeclib(x) <- as.character(idSpeclib(x)[i])
    attribute(x) <- attribute(x)[i,]
    return(x)
  }
  spectra(x) <- spectra(x)[i,j]
  wavelength(x) <- wavelength(x)[j]
  idSpeclib(x) <- as.character(idSpeclib(x)[i])
  attribute(x) <- attribute(x)[i,]
  return(x)
})