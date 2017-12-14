# if (!isGeneric("apply")) {
#   setGeneric("apply", function(X, MARGIN, FUN, ...)
#   standardGeneric("apply"))
# }

setMethod("apply", signature(X = "Speclib"), 
          function(X, 
                   FUN,
                   bySI = NULL,
                   ...)
{ 
  usage <- usagehistory(X)
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
  
  FUN <- match.fun(FUN_str)
  
  if (is.null(bySI))
  {
    result <- speclib(spectra = matrix(data = 0,
                                        ncol = length(wavelength(x)),
                                        nrow = 1),  
                      wavelength = wavelength(x),                      
                      usagehistory = x@usagehistory
                    )
    idSpeclib(result) <- FUN_str  
    
    pp <- .process_parallel()
    
    spectra <- spectra(x)
    x <- wavelength(x)
    n <- ncol(spectra)
    if (!pp[[1]])
    {
      spec <- apply(spectra, 2, FUN = FUN, ...)
    } else {
      `%op%` <- pp[[2]]
      spec <- foreach::foreach(i=1:ncol(spectra), .combine = 'rbind') %op%
      {
        do.call(FUN, list(spectra[,i], ...))
      }
      .restoreParallel()
    }
    spec <- matrix(spec, nrow = 1)
    spectra(result) <- spec
    idSpeclib(result) <- FUN_str
    usagehistory(result) <- paste(X@ylabel, "=", FUN_str, "applied to matrix of",n , "spectra")
  } else {
    SI_X <- SI(X)
    SI_col <- which(names(SI_X) == bySI)
    if (length(SI_col) == 0)
      stop(paste("Could not find column '", bySI, "' in SI of X", sep = ""))
    SI_X <- SI_X[, SI_col]
    if (!is.factor(SI_X))
      SI_X <- as.factor(SI_X)
    lev <- levels(SI_X)
    tmp <- parse(text = bySI)
    tmp_lev <- lev[1]
    result <- apply(.subset.speclib(X, expression(eval(parse(text = tmp)) == tmp_lev)), FUN = FUN, ...)
    SI(result) <- data.frame(X = lev[1], stringsAsFactors = FALSE)
    for (i in 2:length(lev))
    {
      tmp_lev <- lev[i]
      res_lev <- apply(.subset.speclib(X, expression(eval(parse(text = tmp)) == tmp_lev)),
                       FUN = FUN, ...)
      SI(res_lev) <- data.frame(X = lev[i], stringsAsFactors = FALSE)
      result <- merge(result, res_lev)
    }
    
    usagehistory(result) <- NULL
    usagehistory(result) <- usage
    usagehistory(result) <- paste(X@ylabel, " = ", FUN_str, " applied to matrix spectra by SI '", 
                                  bySI, "'", sep = "")
    names(SI(result)) <- bySI
  }
  return(result)
}
)
   




