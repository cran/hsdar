# if (!isGeneric("apply")) {
#   setGeneric("apply", function(X, MARGIN, FUN, ...)
#   standardGeneric("apply"))
# }

setMethod("apply", signature(X = "Speclib"), 
          function(X, 
                   FUN,
                   byattributes = NULL,
                   ...)
{ 
  x <- X 

  if (is.character(FUN))
  {
    FUN_str <- FUN
  } else {
    call_fu <- match.call(call = sys.call(-1))
    FUN_str <- as.character(call_fu[which(names(call_fu) == "FUN")])
  }
  
  if (!is.function(try(match.fun(FUN), silent = TRUE)))
    stop("Unknown function")
  
  FUN <- match.fun(FUN)
  
  if (is.null(byattributes))
  {
    result <- speclib(spectra = matrix(data = 0,
                                        ncol = length(wavelength(x)),
                                        nrow = 1),  
                      wavelength = wavelength(x),                      
                      usagehistory = x@usagehistory
                    )
    idSpeclib(result) <- FUN_str  
    
    spectra <- t(spectra(x))
    x <- wavelength(x)
    n <- ncol(spectra)
    
    spec <- apply(spectra, 1, FUN = FUN, ...)
    spec <- t(as.matrix(data.frame(data = spec)))
    spectra(result) <- spec
    idSpeclib(result) <- FUN_str
    usagehistory(result) <- paste(X@ylabel, "=", FUN_str, "applied to matrix of",n , "spectra")
  } 
  return(result)
}
)
   




