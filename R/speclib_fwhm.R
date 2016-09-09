setReplaceMethod("fwhm", signature(object = "Speclib", value = "numeric"), 
                 function(object, value)
{
  object@fwhm <- value
  return(object)
}
)

setMethod("fwhm", signature(object = "Speclib"), 
          function(object)
  return(object@fwhm)
)