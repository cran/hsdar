if (!isGeneric("predictHyperspec")) {
  setGeneric("predictHyperspec", function(object, newdata, preProcess, ...)
  standardGeneric("predictHyperspec"))
}

setMethod("predictHyperspec",
          signature(object = "train", 
                    newdata = ".CaretHyperspectral",
                    preProcess = "missing"),
          definition = function(object,
                                newdata,
                                preProcess = NULL,
                                ...)
{
  if (class(newdata) == "Speclib")
  {
    if (newdata@spectra@fromRaster)
      return(.blockwise(speclib_obj = "newdata", pos = 2))
  }
  
  useSIAsPredicants <- !is.na(.getPredicantVar(newdata, stopifmissing = FALSE))[1]
  
  if (class(newdata) == "Nri")
  {
    all_vals <- as.data.frame(newdata, na.rm = TRUE)
  } else {
    all_vals <- as.data.frame(newdata)
  }
  
  if (useSIAsPredicants)
  {
    addVar <- .getPredicantVar(newdata)
    all_vals <- cbind(all_vals, addVar)
  }

  return(predict(object = object, newdata = all_vals, ...))
})

setMethod("predictHyperspec",
          signature(object = "train", 
                    newdata = ".CaretHyperspectral",
                    preProcess = "function"),
          definition = function(object,
                                newdata,
                                preProcess,
                                ...)
{
  if (class(newdata) == "Speclib")
  {
    if (newdata@spectra@fromRaster)
      return(.blockwise(speclib_obj = "newdata", pos = 2))
  }
  
  newdata <- preProcess(newdata, ...)
  
  useSIAsPredicants <- !is.na(.getPredicantVar(newdata, stopifmissing = FALSE))[1]
  
  if (class(newdata) == "Nri")
  {
    all_vals <- as.data.frame(newdata, na.rm = TRUE)
  } else {
    all_vals <- as.data.frame(newdata)
  }
  
  if (useSIAsPredicants)
  {
    addVar <- .getPredicantVar(newdata)
    all_vals <- cbind(all_vals, addVar)
  }

  return(predict(object = object, newdata = all_vals, ...))
})

