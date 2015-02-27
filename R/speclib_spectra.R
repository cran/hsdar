setMethod("spectra", signature(object = "Speclib"), 
          function(object, ...)
  return(.spectra(object, ...))
)

setReplaceMethod("spectra", signature(object = "Speclib", value = "matrix"), 
                 function(object, value)
{
  object@spectra <- value
  return(object)
}
)

setReplaceMethod("spectra", signature(object = "Speclib", value = "data.frame"),
                 function(object, value)
{
  object@spectra <- as.matrix(value)
  return(object)
}
)

setReplaceMethod("spectra", signature(object = "Speclib", value = "numeric"),
                 function(object, value)
{
  object@spectra <- as.matrix(value)
  return(object)
}
)


.spectra <- function(object, return_names = FALSE)
{
  if (return_names) 
  {
    spec <- object@spectra
    if (!is.null(bandnames(object)))
    {
      if (length(bandnames(object)) == ncol(spec))
      {
        colnames(spec) <- bandnames(object)
      } else {
        warning("Length of bandnames and number of bands in spectra differ. Drop bandnames")
        colnames(spec) <- paste("B_", wavelength(object), sep = "")
      }
    } else {
      colnames(spec) <- paste("B_", wavelength(object), sep = "")
    }
    
    if (length(idSpeclib(object)) > 0)
    {
      if (length(idSpeclib(object)) == nrow(spec))
      {
        rownames(spec) <- idSpeclib(object)
      } else {
        warning("Length of idSpeclib and number of spectra differ. Drop ID")
        rownames(spec) <- paste("ID_", c(1:nspectra(object)), sep = "")
      }
    } else {
      rownames(spec) <- paste("ID_", c(1:nspectra(object)), sep = "")
    }    
    return(spec)
  } else {
    return(object@spectra)
  }
}


