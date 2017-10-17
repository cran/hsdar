setMethod("SI", signature(object = "Speclib", i = "missing", j = "missing"), 
          function(object, i, j)
  return(.SI(object@SI))
)

setMethod("SI", signature(object = "Speclib", i = "ANY", j = "missing"), 
          function(object, i, j)
  return(object@SI[i,])
)

setMethod("SI", signature(object = "Speclib", i = "missing", j = "ANY"), 
          function(object, i, j)
  return(object@SI[,j])
)

setMethod("SI", signature(object = "Speclib", i = "ANY", j = "ANY"), 
          function(object, i, j)
  return(object@SI[i,j])
)

setReplaceMethod("SI", signature(object = "Speclib", value = "matrix"), 
                 function(object, value)
{
  object@SI <- new(".SI", value)
  return(object)
}
)

setReplaceMethod("SI", signature(object = "Speclib", value = "data.frame"),
                 function(object, value)
{
  object@SI <- new(".SI", value)
  return(object)
}
)

setReplaceMethod("SI", signature(object = "Speclib", value = "ANY"),
                 function(object, value)
{
  object@SI <- new(".SI", value)
  return(object)
}
)

.SI <- function(object, col2keep = NULL, row2keep = NULL)
{
  if (ncol(object) == 0)
    return(data.frame())
  if (nrow(object) == 0)
    return(data.frame())  
   
  if (is.null(col2keep))
    col2keep <- c(1:ncol(object))
  if (is.null(row2keep))
  {
    sample2keep <- c(1:nrow(object))
  } else {
    sample2keep <- c(1:nrow(object))[row2keep]
  }
  
  var2keep <- c(1:ncol(object))*0
  var2keep[col2keep] <- 1
  var2keep[(var2keep == 1) & object@rasterObject] <- 2
  if (any(var2keep == 2))
  {
    first_raster <- c(1:ncol(object))[var2keep == 2]
    first_raster <- first_raster[1]
    idx <- rowColFromCell(object@SI_data[[first_raster]], sample2keep)
    minmax <- apply(idx, 2, range)
  }
  
  res <- as.data.frame(matrix(NA, ncol = sum(var2keep > 0), nrow = length(sample2keep)))
  ivar <- 0
  if (ncol(res) > 0)
  {
    for (i in 1:ncol(object))
    {
      if (var2keep[i] == 1)
      {
        ivar <- ivar + 1
        res[,ivar] <- object@SI_data[[i]][sample2keep]
      } 
      if (var2keep[i] == 2)
      {
        ivar <- ivar + 1
        res[,ivar] <- unlist(getValuesBlock(object@SI_data[[i]],
                                            row = minmax[1,1], 
                                            nrows = minmax[2,1] - minmax[1,1] + 1,
                                            col = minmax[1,2],
                                            ncols = minmax[2,2] - minmax[1,2] + 1))
      }
    }
  }
  names(res) <- names(object@SI_data)[var2keep > 0]
  return(res)
}
  
        
setMethod("initialize", signature(.Object = ".SI"),
          function(.Object, ...)
{ 
  dots <- list(...)
  dims <- c(0, 0)
  res  <- list()
  nam  <- character()
  ra   <- logical()
  num  <- logical()
  j    <- 0
  if (length(dots) > 0)
  {
    if (class(dots[[1]]) %in% c("matrix", "data.frame"))
    {
      dims <- c(nrow(dots[[1]]), 0)
    } else {
      dims <- c(length(dots[[1]]), 0)
    }    
    for (i in 1:length(dots))
    {
      if (class(dots[[i]]) %in% c("matrix", "data.frame"))
      {
        if (nrow(dots[[i]]) != dims[1])
          stop("Length of SI variables not identical")
        if (ncol(dots[[i]]) > 0)
        {
          nam <- c(nam, names(dots[[i]]))
          for (k in 1:ncol(dots[[i]]))
          {     
            j <- j + 1
            res[[j]] <- dots[[i]][,k]            
            ra  <- c(ra, FALSE)
            num <- c(num, is.numeric(dots[[i]][,k]))
          }
        }
      } else {
        if (class(dots[[i]]) %in% c("RasterLayer", "RasterBrick", "RasterStack"))
        {
          if (class(dots[[i]]) != "RasterLayer")
            stop("Only objects of class 'RasterLayer' can be currently added to SI.\nUse multiple RasterLayers to include RasterBrick/RasterStack.")
          j <- j + 1
          res[[j]] <- dots[[i]]
          ra <- c(ra, TRUE)
          num <- c(num, TRUE)
        } else {
          if (length(dots[[i]]) != dims[1])
            stop("Length of SI variables not identical")
          j <- j + 1
          res[[j]] <- dots[[i]]
          ra <- c(ra, FALSE)
          num <- c(num, is.numeric(dots[[i]]))
        }
        if (is.null(names(dots[[i]])))
        {
          nam <- c(nam, paste0("V", j))
        } else {
          nam <- c(nam, names(dots)[i])
        }
      }
    }
    dims <- c(dims[1], length(res))
  }
#   print(nam)
#   print(res)
  names(res) <- nam
  object <- .Object
  object@SI_data      <- res
  object@dim          <- dims
  object@rasterObject <- ra
  object@numericVar   <- num
  return(object)
}
)

setMethod("nrow", signature(x = ".SI"), 
          function(x)
  return(x@dim[1])
)

setMethod("ncol", signature(x = ".SI"), 
          function(x)
  return(x@dim[2])
)

setMethod("names", signature(x = ".SI"), 
          function(x)
  return(names(x@SI_data))
)

setMethod("[", ".SI",
          function(x, i, j, ...)
{
  dots <- list(...)
  
  if (missing(i)) 
    i = NULL
  if (missing(j)) 
    j = NULL 
    
  return(.SI(x, col2keep = j, row2keep = i))
}
)

.getSI_rasterObject <- function(x)
  x@rasterObject
  