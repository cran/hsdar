setReplaceMethod("wavelength", signature(object = "Speclib", value = "numeric"), 
                 function(object, value)
{
  object@wavelength <- value
  return(object)
}
)

setMethod("wavelength", signature(object = "Speclib"), 
          function(object)
  return(object@wavelength)
)

.ConvWlFwd <- function(wlunit)
{
  micro <- "\u00B5m"
  if (wlunit == micro)
    wlunit <- "mu"
  convwl <- switch(wlunit,
                   "mu" = 1/1e-03,
                   "nm" = 1,
                   "m"  = 1/1e-09,
                   "dm" = 1/1e-08,
                   "cm" = 1/1e-07,
                   "mm" = 1/1e-06,
                   NA
                  )
  if (!is.finite(convwl))
    stop("Could not recognize wavelength unit")
  return(convwl)
}

.ConvWlBwd <- function(wlunit)
{
  micro <- "\u00B5m"
  if (wlunit == micro)
    wlunit <- "mu"
  convwl <- switch(wlunit,
                   "mu" = 1e-03,
                   "nm" = 1,
                   "m"  = 1e-09,
                   "dm" = 1e-08,
                   "cm" = 1e-07,
                   "mm" = 1e-06,
                   NA
                  )
  if (!is.finite(convwl))
    stop("Could not recognize wavelength unit")
  return(convwl)
}

.ConvWlRnd <- function(wlunit)
{  
  micro <- "\u00B5m"
  if (wlunit == micro)
    wlunit <- "mu"
  convwl <- switch(wlunit,
                   "mu" = 4,
                   "nm" = 2,
                   "m"  = 9,
                   "dm" = 8,
                   "cm" = 7,
                   "mm" = 6,
                   NA
                  )
  if (!is.finite(convwl))       
    stop("Could not recognize wavelength unit")
  return(convwl)
}
