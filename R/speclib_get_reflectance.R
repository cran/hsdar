get_reflectance <- function(spectra, wavelength, position, weighted = FALSE, ...)
{
  if (class(spectra)[1] == "Speclib")
  {
    if (missing(position))
    {
      return(get_reflectance(spectra = spectra(spectra), 
                             wavelength = wavelength(spectra),
                             position = wavelength, 
                             weighted = weighted, ...))
    } else {      
      if (missing(wavelength))
      {
        return(get_reflectance(spectra = spectra(spectra), 
                               wavelength = wavelength(spectra),
                               position = position, 
                               weighted = weighted, ...))
      } else {
        stop("Either wavelength or position is not correctly set")
      }
    }
  }
  if (wavelength[1]<=position & wavelength[length(wavelength)]>=position)
  {
    if (weighted)
    {
      if (any(wavelength==position))
      {
        return(get_reflectance(spectra, wavelength, position, weighted = FALSE))
      } else {
        temp <- abs(wavelength-position)
        ord <- order(temp)
        return((spectra[,ord[1]]*1/temp[ord[1]]+spectra[,ord[2]]*1/temp[ord[2]])/
               (1/temp[ord[1]]+1/temp[ord[2]]))
      }
    } else {
      temp <- abs(wavelength-position)
      return(spectra[,which(temp==min(temp))[1]])
    }
  } else {
    return(rep.int(NA,nrow(spectra)))
  }
}

