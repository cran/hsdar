if (!isGeneric("safs")) {
  setGeneric("safs")
}


setMethod("safs", signature(x = "Speclib"),
          definition = function(x,
                                y,
                                cutoff = .95,
                                returnData = TRUE,
                                na.rm = FALSE,
                                ...)
{
  y_missing <- missing(y)
  
  if (y_missing)
  {
    y <- .getResponseVar(x, 
                         advice = c("safs", "setResponse", 
                                    "This is only required if you do not specify 'y'."))
  }
  
  useSIAsPredicants <- !is.na(.getPredicantVar(x, stopifmissing = FALSE))[1]
  
  x_dat <- as.data.frame(spectra(x))
  if (is.finite(cutoff))
  {
    x_dat_cor <- cor(x_dat)
    x_dat_cor[!is.finite(x_dat_cor)] <- 0
    x_dat <- x_dat[, -findCorrelation(x_dat_cor, cutoff)]
    x_dat <- as.data.frame(x_dat)
  }
  
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

  dots <- list(...)
  res <- if (!any(names(dots) == "safsControl"))
           safs(x_dat, y, safsControl = safsControl(functions = rfSA), ...)
         else
           safs(x_dat, y, ...)
  if (!returnData)
    return(res)
  
  pred <- res$optVariables# predictors(res) ## BUG? in caret

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
  
  x <- .setCaretParameter(x, "safs_result", res)
  usagehistory(x) <- "Supervised feature selection using simulated annealing"
  return(x)
})

setMethod("safs", signature(x = "Nri"),
          definition = function(x,
                                y,
                                cutoff = .95,
                                returnData = TRUE,
                                na.rm = FALSE,
                                ...)
{  
  y_missing <- missing(y)
  
  if (y_missing)
  {
    y <- .getResponseVar(x,
                         advice = c("safs", "setResponse", 
                                    "This is only required if you do not specify 'y'."))
  }  
    
  useSIAsPredicants <- !is.na(.getPredicantVar(x, stopifmissing = FALSE))[1]
  
  nri_vals_all <- as.data.frame(x)
  if (is.finite(cutoff))
  {
    nri_vals_all_cor <- cor(nri_vals_all)
    nri_vals_all_cor[!is.finite(nri_vals_all_cor)] <- 0
    nri_vals <- nri_vals_all[, -findCorrelation(nri_vals_all_cor, cutoff)]
  } else {
    nri_vals <- nri_vals_all
  }  
  nri_vals <- as.data.frame(nri_vals)
  
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

  dots <- list(...)
  res <- if (!any(names(dots) == "safsControl"))
           safs(nri_vals, y, safsControl = safsControl(functions = rfSA), ...)
         else
           safs(nri_vals, y, ...)

  if (!returnData)
    return(res)
  
  pred <- res$optVariables# predictors(res) ## BUG? in caret
  
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
  
  return(.setCaretParameter(x, "safs_result", res))
})


setMethod("safs", signature(x = "Specfeat"),
          definition = function(x,
                                y,
                                cutoff = .95,
                                returnData = TRUE,
                                na.rm = FALSE,
                                ...)
{
  x <- .as.speclib.specfeat(x, na.rm = na.rm)

  if (missing(y))
  {
    return(safs(x, cutoff = cutoff, returnData = returnData, na.rm = na.rm, ...))
  } else {
    return(safs(x, y, cutoff = cutoff, returnData = returnData, na.rm = na.rm, ...))
  }
})

get_safs  <- function(x)
  .getCaretParameter(x, "safs_result")
