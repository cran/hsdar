setMethod("print", signature(x = "Speclib"), 
          function(x)
{
  cat(paste("Summary of ", class(x), "\n", sep = ""))
  .printUsagehistory(x)
  cat("\n\nSummary of spectra\n")
  cat("---------------------\n")
  cat(paste("Total number of spectra :",dim(x)[1]))
  cat(paste("\nNumber of bands :",dim(x)[2]))
  if (length(x@fwhm)==1)
  {
    cat(paste("\nWidth of bands :",x@fwhm * .ConvWlBwd(x@wlunit)))
  } else {
    cat(paste("\nMean width of bands :",round(mean(x@fwhm) * .ConvWlBwd(x@wlunit),
                                              .ConvWlRnd(x@wlunit)), x@wlunit))
  }
  ra <- range(wavelength(x), na.rm = T) * .ConvWlBwd(x@wlunit)  
  cat(paste("\nSpectral range of data :", prettyNum(ra[1]), "-", prettyNum(ra[2]), x@wlunit))
  if (x@wlunit != "nm")
    cat("\n    Note that wavelength are internally in stored in nm\n")
  cat("\n")
  if (x@spectra@fromRaster)
  {
    cat(paste("Use RasterBrick for spectra"))
    if (x@spectra@spectra_ra@data@inmemory)
    {
      cat(paste(" (in RAM)\n"))
    } else {
      cat(paste(" stored at\n'", x@spectra@spectra_ra@file@name, "'\n", sep =""))
    }
  }
  
  
   if (x@SI@dim[2] > 0)
  {
    cat("\n\nSpeclib contains SI\n---------------------\n")
#     cat("Columns:\n")
#     nam <- " "
#     nc <- 0
#     for (i in names(x@SI@SI_data))
#     {
#       if (nc > 90)
#       {
#         nam <- paste0(nam, "\n")
#         nc <- 0
#       }
#       nc <- nc + nchar(i) + 2
#       if (nchar(nam) > 1)
#       {
#         nam <- paste0(nam, ", ", i)
#       } else {
#         nam <- i
#       }
#     }
#     cat(nam)
#     cat("Classes of data:\n")
    
    si_info <- data.frame(Variables = names(x@SI@SI_data),
                          Classes = unlist(lapply(x@SI@SI_data, function (x) class(x))), 
                          row.names = 1:x@SI@dim[2])
    print(si_info)
    
  } 
}
)

setMethod ('show' , signature(object = "Speclib"), 
           function(object)
{
  print(object)  
}
)

.printUsagehistory <- function(x)
{
  if (length(x@usagehistory)>0)
  {
    cat("\n\nHistory of usage\n")
    cat("---------------------\n")
    for (i in 1:length(x@usagehistory)) cat(paste("(",i,")   ",x@usagehistory[i],"\n",sep=""))
  }
}