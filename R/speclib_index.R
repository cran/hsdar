setMethod("[", "Speclib",
          function(x, i, j, ...)
{
  if (missing(i)) 
  {
    if (nspectra(x) == 1)
    { 
      tmp <- spectra(x)[,j]
      spectra(x) <- matrix(tmp, ncol = length(tmp))
    } else {
      spectra(x) <- spectra(x)[,j]
    }
    wavelength(x) <- wavelength(x)[j]
    return(x)
  } 
  if (missing(j))
  {
    tmp <- spectra(x)[i,] 
    if (class(tmp) == "numeric")
      tmp <- matrix(tmp, ncol = if (nbands(x) > 1) length(tmp) else 1)
    spectra(x) <- tmp
    idSpeclib(x) <- as.character(idSpeclib(x)[i])
    attribute(x) <- attribute(x)[i,]
    return(x)
  }
  tmp <- spectra(x)[i,j] 
  if (class(tmp) == "numeric")
  {
    ncols <- sum(rep.int(1, nbands(x))[j])
    nrows <- sum(rep.int(1, nspectra(x))[i])
    tmp <- matrix(tmp, ncol = ncols, nrow = nrows)
  }
  spectra(x) <- tmp
  wavelength(x) <- wavelength(x)[j]
  idSpeclib(x) <- as.character(idSpeclib(x)[i])
  attribute(x) <- attribute(x)[i,]
  return(x)
})