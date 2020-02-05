# setMethod('extract', signature(x='Speclib', y='ANY'), 
#           function(x, y, ...)
# { 
#   if (!x@spectra@fromRaster)
#     stop("x does not contain RasterBrick")
#   
#   returnSpeclib <- FALSE
#   gotID <- FALSE
#   
#   res <- extract(x@spectra@spectra_ra, y, ...)
#   if (is.list(res))
#   {
#     if (length(res) > 1)
#     {
#       n <- 1:length(res)
#       n[1] <- nrow(res[[1]])
#       for (i in 2:length(res))
#         n[i] <- nrow(res[[i]]) + n[i-1]
#     } else {
#       n <- nrow(res[[1]])
#     }
#     spec <- matrix(0, ncol = nbands(x), nrow = n[length(n)])
# 
#     id <- c(1:nrow(spec))
#     
#     n <- matrix(c(1:length(n), 1, n[-length(n)] + 1, n ), ncol = 3) 
# 
#     res <- apply(n, 1, function(i, x) 
#     {
#       spec[c(i[2]:i[3]),] <<- as.matrix(x[[i[1]]])
#       id[c(i[2]:i[3])] <<- rep.int(i[1], length(i[2]:i[3])) 
#     }, res)
#     res <- spec
#     returnSpeclib <- TRUE
#     gotID <- TRUE
#   }
#   
#   if (class(res) == "RasterBrick")
#     returnSpeclib <- TRUE
#   
#   if (class(res) == "data.frame")
#   {
#     res <- as.matrix(res)
#     returnSpeclib <- TRUE
#   }
#   
#   if (returnSpeclib)
#   {
#     res <- speclib(res, wavelength(x))
#     if (gotID)
#       SI(res) <- data.frame(ID = id)
#     usagehistory(res) <- usagehistory(x)
#     usagehistory(res) <- paste("Values extracted using object of class '",
#                                class(y), "' as overlay", sep = "")
#   }
#   
#   return(res)
# })

setMethod("extract", signature(x = "Speclib"), 
          definition = function(x, y, ...)
{
  if (x@spectra@fromRaster)
  {
    vals <- extract(x@spectra@spectra_ra, y, ...)
    if (class(vals)[1] == "matrix")
    {
      vals <- speclib(vals, wavelength(x))
      SI(vals) <- as.data.frame(y)
      usagehistory(vals) <- usagehistory(x)
      usagehistory(vals) <- paste("Values extracted using object of class '",
                                  class(y)[1], "' as overlay", sep = "")
      return(vals)
    }
    if (class(vals)[1] == "list")
    {
      SI_data <- as.data.frame(y)
      n_px <- unlist(lapply(vals, function(i) nrow(i)))
      vals <- speclib(do.call(rbind, vals), wavelength(x))
      n_px_i <- unlist(apply(matrix(c(1:length(n_px), n_px), ncol = 2, byrow = FALSE), 1, 
                             function(n) rep.int(n[1], n[2])))
      SI_data_rep <- SI_data[n_px_i,]
      SI(vals) <- SI_data_rep
      usagehistory(vals) <- usagehistory(x)
      usagehistory(vals) <- paste("Values extracted using object of class '",
                                  class(y)[1], "' as overlay", sep = "")
      return(vals)
    }
    if (class(vals)[1] == "numeric" || class(vals)[1] == "integer")
    {
      vals <- speclib(as.numeric(vals), wavelength(x))
      SI(vals) <- as.data.frame(y)
      usagehistory(vals) <- usagehistory(x)
      usagehistory(vals) <- paste("Values extracted using object of class '",
                                  class(y)[1], "' as overlay", sep = "")
      return(vals)
    }
    warning("Cannot construct Speclib from extract value")
    return(vals)
  } else {
    stop("Speclib does not contain spectra from *raster-object")
  }
}
)
  
setMethod('writeRaster', signature(x='Speclib', filename='character'), 
          function(x, filename, ...) 
{
  if (!x@spectra@fromRaster)
    stop("x does not contain RasterBrick")
  
  spectra(x) <- writeRaster(x@spectra@spectra_ra, filename = filename, ...)
  return(x)
})


setMethod('plotRGB', signature(x='Speclib'), 
          function(x, ...) 
{
  if (!x@spectra@fromRaster)
    stop("x does not contain RasterBrick")
  
  dots <- list(...)
  
  if (!("r" %in% names(dots)))
    dots$r <- which.min(abs(wavelength(x) - 680))
    
  if (!("g" %in% names(dots)))
    dots$g <- which.min(abs(wavelength(x) - 540))
    
  if (!("b" %in% names(dots)))
    dots$b <- which.min(abs(wavelength(x) - 470))
    
  dots$x <- x@spectra@spectra_ra
  invisible(do.call("plotRGB", dots))
})
