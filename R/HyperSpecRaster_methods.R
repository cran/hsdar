setMethod('writeStart', signature(x = 'Speclib', filename = "character"),
           function(x, filename, ...) 
 {
   if (!x@spectra@fromRaster)
     stop("Speclib does not contain spectra from *Raster-object")
   
   x_brick <- x@spectra@spectra_ra  
     
   mc <- match.call(definition = sys.function(-1), 
                    call = sys.call(-1), expand.dots = TRUE)
   args <- as.list(mc)[-1]
   got_wv <- FALSE
   if (any(names(args) == "wavelength"))
   {
     wv <- list(...)$wavelength
     got_wv <- TRUE
   } else {
     wv <- wavelength(x)
   }
   
   if (any(names(args) == "nl"))
   {
     if (!got_wv)
       wv <- 1:eval(parse(text = args$nl))
     x_brick <- brick(x_brick, nl = eval(parse(text = args$nl)))
   } 
   x_brick <- writeStart(x_brick, filename, ...)
   
   return(speclib(x_brick, wv, 
                  SI = NULL,
                  usagehistory = usagehistory(x),
                  transformation = x@transformation,
                  continuousdata = x@continuousdata,
                  wlunit = x@wlunit,
                  xlabel = x@xlabel,
                  ylabel = x@ylabel,
                  rastermeta = x@rastermeta))
 } 
)

setMethod('writeStart', signature(x = 'HyperSpecRaster', filename = "character"),
           function(x, filename, ...) 
 {
   mc <- match.call(definition = sys.function(-1), 
                    call = sys.call(-1), expand.dots = TRUE)
   args <- as.list(mc)[-1]
   if (any(names(args) == "nl"))
   {
     x <- as(x, "RasterBrick")
     x <- brick(x, nl = eval(parse(text = args$nl)))
   }
   return(callNextMethod(x, filename, ...))
 } 
)

setMethod('writeStop', signature(x = 'Speclib'),
           function(x) 
 {
   if (!x@spectra@fromRaster)
     stop("Speclib does not contain spectra from *Raster-object")
   res <- writeStop(x@spectra@spectra_ra)
   if (class(res) == "RasterLayer")
     res <- brick(res)
   x@spectra@spectra_ra <- res
   return(x)
 }
)

setMethod('writeValues', signature(x='HyperSpecRaster', v='Speclib'),
           function(x, v, start) 
 {
   if (nlayers(x) != nbands(v))
     stop(paste("Number of bands in x (", 
                nlayers(x), ") and v (",
                nbands(v),") differ", sep =""))
   spec <- spectra(v)   
   return(writeValues(x, spec, start))
 } 
)

setMethod('writeValues', signature(x='Speclib', v='Speclib'),
           function(x, v, start) 
 {
   if (!x@spectra@fromRaster)
     stop("Speclib does not contain spectra from *Raster-object")
     
   if (nbands(x) != nbands(v))
     stop(paste("Number of bands in x (", 
                nbands(x), ") and v (",
                nbands(v),") differ", sep =""))
   spec <- spectra(v) 
   spec <- writeValues(x, spec, start)
   usagehistory(spec) <- usagehistory(v)
   spec@transformation = v@transformation
   spec@continuousdata = v@continuousdata
   spec@wlunit = v@wlunit
   spec@xlabel = v@xlabel
   spec@ylabel = v@ylabel
   return(spec)
 } 
)

setMethod('writeValues', signature(x='Speclib', v='matrix'),
           function(x, v, start) 
 {
   if (!x@spectra@fromRaster)
     stop("Speclib does not contain spectra from *Raster-object")
     
   if (nbands(x) != ncol(v))
     stop(paste("Number of bands in x (", 
                nbands(x), ") and v (",
                ncol(v),") differ", sep =""))
   x_brick <- x@spectra@spectra_ra 
   return(speclib(writeValues(x_brick, v, start),
                  wavelength(x)))
 } 
)

setMethod('writeValues', signature(x='Speclib', v='numeric'),
           function(x, v, start) 
 {
   if (!x@spectra@fromRaster)
     stop("Speclib does not contain spectra from *Raster-object")
     
   if (nbands(x) != 1)
     stop(paste("Number of bands in x (", 
                nbands(x), ") and v differ", sep =""))
   x_brick <- x@spectra@spectra_ra 
   return(speclib(writeValues(x_brick, v, start), 
                  wavelength(x)))
 } 
)


setMethod('writeValues', signature(x='RasterBrick', v='Speclib'),
           function(x, v, start) 
 {
   if (nlayers(x) != nbands(v))
     stop(paste("Number of bands in x (", 
                nlayers(x), ") and v (",
                nbands(v),") differ", sep =""))
   spec <- spectra(v)
   return(writeValues(x, spec, start))
 } 
)

setMethod('writeValues', signature(x='RasterLayer', v='Speclib'),
           function(x, v, start) 
 {
   if (nbands(v) != 1)
     stop(paste("Number of bands in v must equal 1 if x is of class RasterLayer", sep =""))
   spec <- spectra(v)   
   return(writeValues(x, spec, start))
 } 
)

setMethod('show', 'HyperSpecRaster',
           function(object) 
 {
   callNextMethod(object)
   cat('wavelength:\n')
   print(object@wavelength)
 } 
)

setMethod('getValuesBlock', signature(x = 'HyperSpecRaster'),
           function(x, ...) 
 {
   v <- callNextMethod(x, ...)
   return(speclib(v, x@wavelength, fwhm = if (length(x@fwhm) > 0) x@fwhm else NULL, 
                  SI = if (nrow(x@SI) > 0) x@SI else NULL)) 
 } 
)

setMethod('getValuesBlock', signature(x = 'Speclib'),
           function(x, ...) 
 {
   if (!x@spectra@fromRaster)
     stop("Speclib does not contain spectra from *Raster-object")
   x_brick <- x@spectra@spectra_ra
   v <- getValuesBlock(x_brick, ...)
   SI_data <- .getValuesBlockSI(x, ...)
   return(speclib(v, wavelength(x), SI = SI_data))
 } 
)

setMethod('HyperSpecRaster', signature(x = 'character', wavelength = "numeric"),
           function(x, wavelength, fwhm = NULL, SI = NULL, ...) 
 {
   res <- brick(x, ...)
   if (nlayers(res) != length(wavelength))
     stop("Length of wavelength do not equal number of bands in file")
   res <- as(res, 'HyperSpecRaster')
   res@wavelength <- wavelength
   if (!is.null(fwhm))
     res@fwhm <- fwhm
   if (!is.null(SI))
     res@SI <- SI
   return(res)
 } 
)

setMethod('HyperSpecRaster', signature(x = 'RasterBrick', wavelength = "numeric"),
           function(x, wavelength, fwhm = NULL, SI = NULL) 
 {
   if (nlayers(x) != length(wavelength))
     stop("Length of wavelength do not equal number of bands in file")
   res <- as(x, 'HyperSpecRaster')
   res@wavelength <- wavelength
   if (!is.null(fwhm))
     res@fwhm <- fwhm
   if (!is.null(SI))
     res@SI <- SI
   return(res)
 } 
)

setMethod('HyperSpecRaster', signature(x = 'RasterLayer', wavelength = "numeric"),
           function(x, wavelength, fwhm = NULL, SI = NULL) 
 {
   res <- brick(x, nl = length(wavelength))
   res <- as(res, 'HyperSpecRaster')
   res@wavelength <- wavelength
   if (!is.null(fwhm))
     res@fwhm <- fwhm
   if (!is.null(SI))
     res@SI <- SI
   return(res)
 } 
)


setMethod('brick', signature(x = 'Speclib'),
           function(x, nrow, ncol, xmn, xmx, ymn, ymx, crs) 
 {
   if (missing(nrow))
   {
     if (!.is.rastermeta(x))
     {
       stop("nrow missing")
     } else {
       nrow <- x@rastermeta[[1]][1]
     }
   }
   if (missing(ncol))
   {
     if (!.is.rastermeta(x))
     {
       stop("ncol missing")
     } else {
       ncol <- x@rastermeta[[1]][2]
     }
   }
   if (missing(xmn))
   {
     if (!.is.rastermeta(x))
     {
       xmn <- 0 
     } else {
       xmn <- x@rastermeta[[2]]@xmin
     }
   }
   if (missing(xmx))
   {
     if (!.is.rastermeta(x))
     {
       xmx <- 1 
     } else {
       xmx <- x@rastermeta[[2]]@xmax
     }
   }
   if (missing(ymn))
   {
     if (!.is.rastermeta(x))
     {
       ymn <- 0 
     } else {
       ymn <- x@rastermeta[[2]]@ymin
     }
   }
   if (missing(ymx))
   {
     if (!.is.rastermeta(x))
     {
       ymx <- 1
     } else {
       ymx <- x@rastermeta[[2]]@ymax
     }
   }
   if (missing(crs))
   {
     if (!.is.rastermeta(x))
     {
       crs <- NA
     } else {
       crs <- x@rastermeta[[3]]
     }
   }
   
   
   arr <- array(data = spectra(x), dim = c(ncol, nrow, nbands(x)))
   res <- brick(arr, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx, crs = crs, transpose = TRUE)

   return(res)
 } 
)

setMethod('HyperSpecRaster', signature(x = 'Speclib'),
            function(x, nrow, ncol, xmn, xmx, ymn, ymx, crs) 
 {
   if (missing(nrow))
   {
     if (!.is.rastermeta(x))
     {
       stop("nrow missing")
     } else {
       nrow <- x@rastermeta[[1]][1]
     }
   }
   if (missing(ncol))
   {
     if (!.is.rastermeta(x))
     {
       stop("ncol missing")
     } else {
       ncol <- x@rastermeta[[1]][2]
     }
   }
   if (missing(xmn))
   {
     if (!.is.rastermeta(x))
     {
       xmn <- 0 
     } else {
       xmn <- x@rastermeta[[2]]@xmin
     }
   }
   if (missing(xmx))
   {
     if (!.is.rastermeta(x))
     {
       xmx <- 1 
     } else {
       xmx <- x@rastermeta[[2]]@xmax
     }
   }
   if (missing(ymn))
   {
     if (!.is.rastermeta(x))
     {
       ymn <- 0 
     } else {
       ymn <- x@rastermeta[[2]]@ymin
     }
   }
   if (missing(ymx))
   {
     if (!.is.rastermeta(x))
     {
       ymx <- 1
     } else {
       ymx <- x@rastermeta[[2]]@ymax
     }
   }
   if (missing(crs))
   {
     if (!.is.rastermeta(x))
     {
       crs <- NA
     } else {
       crs <- x@rastermeta[[3]]
     }
   }
   
   
   arr <- array(data = spectra(x), dim = c(ncol, nrow, nbands(x)))
   res <- brick(arr, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx, crs = crs, transpose = TRUE)
   wl <- wavelength(x)
   if (is.data.frame(wl))
     wl <- rowMeans(wl[,c(1,2)])
      
   return(HyperSpecRaster(res, wavelength = wl))
 }
)


setReplaceMethod("wavelength", signature(object = "HyperSpecRaster", value = "numeric"), 
                 function(object, value)
{
  object@wavelength <- value
  return(object)
}
)

setMethod("wavelength", signature(object = "HyperSpecRaster"), 
          function(object)
  return(object@wavelength)
)

setMethod("blockSize", signature(x = "Speclib"), 
          function(x)
{
  if (!x@spectra@fromRaster)
    stop("Speclib does not contain spectra from *Raster-object")
  return(blockSize(x@spectra@spectra_ra))
}
)

.getValuesBlockSI <- function(x, row=1, nrows=1, col=1, ncols=(ncol(x@spectra@spectra_ra)-col+1))
{
  ncol_raster <- ncol(x@spectra@spectra_ra)
  start_index <- cellFromRowCol(x@spectra@spectra_ra, row, col)-1
  start_matrix <- matrix(start_index+c(1:(nrows*ncol_raster)), nrow = nrows, ncol = ncol_raster, byrow = TRUE)
  index_table <- as.numeric(unlist(start_matrix[,1:ncols]))
  
  SI_data_frame <- SI(x, i = index_table)
  return(SI_data_frame)
}
  
