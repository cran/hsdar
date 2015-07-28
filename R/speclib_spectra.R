setMethod("spectra", signature(object = "Speclib"), 
          function(object, ...)
  return(.spectra(object, ...))
)

setReplaceMethod("spectra", signature(object = "Speclib", value = "matrix"), 
                 function(object, value)
{
  if (object@spectra@fromRaster)
  {
    object@spectra@spectra_ra <- setValues(object@spectra@spectra_ra, value)
  } else {
    object@spectra@spectra_ma <- value
  }
  return(object)
}
)

setReplaceMethod("spectra", signature(object = "Speclib", value = "data.frame"),
                 function(object, value)
{
  if (object@spectra@fromRaster)
  {
    object@spectra@spectra_ra <- setValues(object@spectra@spectra_ra, as.matrix(value))
  } else {
    object@spectra@spectra_ma <- as.matrix(value)
  }
  return(object)
}
)

setReplaceMethod("spectra", signature(object = "Speclib", value = "numeric"),
                 function(object, value)
{
  if (object@spectra@fromRaster)
  {
    object@spectra@spectra_ra <- setValues(object@spectra@spectra_ra,
                                           matrix(value, ncol = nbands(object)))
  } else {
    object@spectra@spectra_ma <- as.matrix(value)
  }
  return(object)
}
)


setReplaceMethod("spectra", signature(object = "Speclib", value = "RasterBrick"),
                 function(object, value)
{
  object@spectra@fromRaster <- TRUE
  object@spectra@spectra_ra <- value  
  return(object)
}
)


.spectra <- function(object, return_names = FALSE)
{
  if (object@spectra@fromRaster)
  {
    spec <- getValues(object@spectra@spectra_ra)
  } else {
    spec <- object@spectra@spectra_ma
  }
  if (return_names) 
  {    
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
  }
  return(spec)
}


