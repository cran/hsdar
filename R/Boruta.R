# if (!isGeneric("Boruta")) {
#   setGeneric("Boruta")
# }
# if (!isGeneric("train.formula")) {
#   setGeneric("train.formula")
# }
setMethod("Boruta", signature(x = "Speclib"),
          definition = function(x,
                                y,
                                ..., 
                                returnData = TRUE, 
                                includeTentative = FALSE,
                                na.rm = FALSE
                               )
{
  y_missing <- missing(y)
  
  if (y_missing)
  {
    y <- .getResponseVar(x, 
                         advice = c("Boruta", "setResponse", 
                                    "This is only required if you do not specify 'y'.")) 
  }
  
  useSIAsPredicants <- !is.na(.getPredicantVar(x, stopifmissing = FALSE))[1]
  
  x_dat <- as.data.frame(spectra(x))
  
  spec_nam <- names(x_dat)
  
  if (useSIAsPredicants)
  {
    addVar <- .getPredicantVar(x) 
    
    
    if (na.rm)
    {
      valid_data <- apply(addVar, 2, function(x) all(is.finite(x)))
      if (any(!valid_data))      
      {
        cat(paste("Remove following variables because at least one sample is not finite:\n"))
        print(names(addVar)[!valid_data])
        addVar <- addVar[,valid_data]
      }
    }
    x_dat <- cbind(x_dat, addVar)
    if (nlevels(as.factor(names(x_dat))) != ncol(x_dat))
    {
      print(names(x_dat))
      stop("Names in predictor data.frame not unique")
    }
  }
  
  res <- Boruta(x = x_dat, y = y, ...)
  if (!returnData)
    return(res)
    
  pred <- if (includeTentative)
  {
    names(res$finalDecision)[res$finalDecision %in% c("Tentative", "Confirmed")]
  } else {
    names(res$finalDecision)[res$finalDecision == "Confirmed"]
  }
  
  x <- x[,sapply(spec_nam, FUN = function(x, pred) any(pred == x), pred), usagehistory = FALSE]
  
  
  if (useSIAsPredicants)
  {
    warning(paste("SI data.frame will only contain relevant variables", 
                  if (y_missing) " and the response variable", ".", sep = ""))
    if (y_missing)
      pred <- c(pred, names(SI(x))[.getCaretParameter(x, "response")])
    cols_keep <- sapply(names(SI(x)), FUN = function(x, pred) any(pred == x), pred)
    if (sum(cols_keep) > 0)
    {
      if (sum(cols_keep) == 1)
      {
        tmp <- as.data.frame(matrix(SI(x)[,cols_keep], ncol = 1))
        names(tmp) <- names(SI(x))[cols_keep]
      } else {
        tmp <- SI(x)[,sapply(names(SI(x)), FUN = function(x, pred) any(pred == x), pred)]
      }
      SI(x) <- tmp
    }
    x <- .updateCaretParameters(x, c("response", "predictor"))
  }
  
  x <- .setCaretParameter(x, "Boruta_result", res)
  usagehistory(x) <- "Important variables selected using Boruta"
  return(x)
})


setMethod("Boruta", signature(x = "Nri"),
          definition = function(x,
                                y,
                                ..., 
                                returnData = TRUE, 
                                includeTentative = FALSE,
                                na.rm = FALSE)
{
  y_missing <- missing(y)
  
  if (y_missing)
  {
    y <- .getResponseVar(x, 
                         advice = c("Boruta", "setResponse", 
                                    "This is only required if you do not specify 'y'.")) 
  }  
    
  useSIAsPredicants <- !is.na(.getPredicantVar(x, stopifmissing = FALSE))[1]
  
  nri_vals <- as.data.frame(x)
  nri_vals_all <- nri_vals
  
  if (useSIAsPredicants)
  {
    addVar <- .getPredicantVar(x)
    
    if (na.rm)
    {
      valid_data <- apply(addVar, 2, function(x) all(is.finite(x)))
      if (any(!valid_data))      
      {
        cat(paste("Remove following variables because at least one sample is not finite:\n"))
        print(names(addVar)[!valid_data])
        addVar <- addVar[,valid_data]
      }
    }
    
    nri_vals <- cbind(nri_vals, addVar)
    if (nlevels(as.factor(names(nri_vals))) != ncol(nri_vals))
    {
      print(names(nri_vals))
      stop("Names in predictor data.frame not unique")
    }
  }

 
  res <- Boruta(x = nri_vals, y = y, ...)
  if (!returnData)
    return(res)
    
  pred <- if (includeTentative)
  {
    names(res$finalDecision)[res$finalDecision %in% c("Tentative", "Confirmed")]
  } else {
    names(res$finalDecision)[res$finalDecision == "Confirmed"]
  }
  
  is.pred.col <- sapply(names(nri_vals_all), FUN = function(x, pred) any(pred == x), pred)
  
  values <- numeric(length = length(x@nri@values))
  values[] <- NA
  incr <- length(x@nri@values)/nrow(nri_vals)
  for (i in 1:ncol(nri_vals_all))
  { 
    if (is.pred.col[i])
    {
      index <- seq(i, length(values), incr)
      values[index] <- nri_vals_all[,i]
    }
  }
  
  x@nri <- distMat3D(values, ncol = ncol(x@nri), nlyr = nrow(nri_vals))  

  if (useSIAsPredicants)
  {
    warning(paste("Attibute data.frame will only contain relevant variables", 
                  if (y_missing) " and the response variable", ".", sep = ""))
    if (y_missing)
      pred <- c(pred, names(SI(x))[.getCaretParameter(x, "response")])
    cols_keep <- sapply(names(SI(x)), FUN = function(x, pred) any(pred == x), pred)
    if (sum(cols_keep) > 0)
    {
      if (sum(cols_keep) == 1)
      {
        tmp <- as.data.frame(matrix(SI(x)[,cols_keep], ncol = 1))
        names(tmp) <- names(SI(x))[cols_keep]
      } else {
        tmp <- SI(x)[,sapply(names(SI(x)), FUN = function(x, pred) any(pred == x), pred)]
      }
      SI(x) <- tmp
    }
    x <- .updateCaretParameters(x, c("response", "predictor"))
  }
  
  x <- .setCaretParameter(x, "Boruta_result", res)
  usagehistory(x) <- "Important variables selected using Boruta"
  return(x)
})

setMethod("Boruta", signature(x = "Specfeat"),
          definition = function(x,
                                y,
                                ..., 
                                returnData = TRUE, 
                                includeTentative = FALSE,
                                na.rm = FALSE
                               )
{
  x <- .as.speclib.specfeat(x, na.rm = na.rm)
  if (missing(y))
  {
    return(Boruta(x, includeTentative = includeTentative, returnData = returnData, na.rm = na.rm, ...))
  } else {
    return(Boruta(x, y, includeTentative = includeTentative, returnData = returnData, na.rm = na.rm, ...))
  }
})

get_Boruta  <- function(x)
  .getCaretParameter(x, "Boruta_result")
