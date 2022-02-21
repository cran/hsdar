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
  if (class(newdata)[1] == "Speclib")
  {
    if (newdata@spectra@fromRaster)
      return(.blockwise(speclib_obj = "newdata", pos = 2))
  }
  
  useSIAsPredicants <- !is.na(.getPredicantVar(newdata, stopifmissing = FALSE))[1]
  
  if (class(newdata)[1] == "Nri")
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
  
  
  preds_obj <- predictors(object)
#   preds_obj <- preds_obj[-length(preds_obj)]
  missing <- sapply(preds_obj, function(x,y) return(!(x %in% y)), names(all_vals))
  
  
  
  if (any(missing))
  {
    k <- 1:length(missing)
    k <- k[missing]
    missing <- missing[missing]
    cat("There are missing predictors:\n")
    for (i in 1:length(missing))
      cat(paste0("  - ", preds_obj[k[i]], "\n"))
    cat("\n * The following predictors were used to train the model:\n")
    print(preds_obj)
    cat("\n * Newdata includes the following variables:\n")
    print(names(all_vals))
    cat("\n\n Did you set additional predictor variables from SI?\n")
  }
  res <- try(predict(object = object, newdata = all_vals, ...))
  if (inherits(res, "try-error"))
    return(all_vals)
  return(res)
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
  if (class(newdata)[1] == "Speclib")
  {
    if (newdata@spectra@fromRaster)
      return(.blockwise(speclib_obj = "newdata", pos = 2))
  }
  
  newdata <- preProcess(newdata, ...)
  
  useSIAsPredicants <- !is.na(.getPredicantVar(newdata, stopifmissing = FALSE))[1]
  
  if (class(newdata)[1] == "Nri")
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

  preds_obj <- predictors(object)
  missing <- sapply(preds_obj, function(x,y) if (! x%in%y) return(x) else return(NULL), names(all_vals))
  if (length(missing) > 0)
  {
    cat("There are missing predictors:\n")
    for (i in 1:length(missing))
      cat(paste0("  - ", missing[i], "\n"))
  }
  
  return(predict(object = object, newdata = all_vals, ...))
})

