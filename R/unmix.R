
unmix <- function(spectra, endmember, returnHCR = "auto", ...
)
{
  if (!all(c(is.speclib(spectra), is.speclib(endmember))))
    stop("Spectra and endmember must be of class 'speclib'")
  if (returnHCR == "auto")
    returnHCR <- !is.null(attr(spectra, "rastermeta"))
  if (dim(endmember)[2]!=dim(spectra)[2])
    stop("Number of bands in spectra must be equal to number of bands in endmember")
  if (dim(endmember)[1] > dim(endmember)[2])
    stop("Number of endmember exceed number of bands")
    
  
  em_matrix <- t(as.matrix(spectra(endmember)))
  spec_matrix <- t(as.matrix(spectra(spectra)))
  
  if (max(c(max(spec_matrix),max(em_matrix)))>1)
      stop("Function needs reflectance values <= 1.0")
  
  if (nrow(em_matrix)!=nrow(spec_matrix))
    stop("Number of bands in spectra is not equal to number of bands in endmember matrix")
  
  n_em      <- ncol(em_matrix)
  n_spec    <- ncol(spec_matrix)
  n_band    <- nrow(em_matrix)
  fractions <- matrix(data=0, ncol=n_spec, nrow=n_em)
  error     <- rep.int(1,n_spec)
  
  storage.mode(n_em)        <- "integer"
  storage.mode(n_band)      <- "integer"
  storage.mode(n_spec)      <- "integer"
  storage.mode(em_matrix)   <- "double"
  storage.mode(spec_matrix) <- "double"
  storage.mode(fractions)   <- "double"
  storage.mode(error)       <- "double"
  
  un_mix <- .C("unmix",
               n_em        = n_em, 
               n_band      = n_band, 
               n_spec      = n_spec,
               em_matrix   = em_matrix, 
               spec_vector = spec_matrix, 
               fractions   = fractions,
               error       = error,
               package     = "hsdar"
              )
  fractions <- matrix(un_mix$fractions, ncol = n_spec)
  error <- un_mix$error
  
  colnames(fractions) <- idSpeclib(spectra)
  row.names(fractions) <- idSpeclib(endmember)
  
  if (returnHCR)
  {
    spec <- as.data.frame(t(as.matrix(rbind(fractions, error))))
    names(spec)[ncol(spec)] <- "error"
    spec <- speclib(spec, c(1:ncol(spec)))
    if (!is.null(attr(spectra, "rastermeta")))
      attr(spec, "rastermeta") <- attr(spectra, "rastermeta")
    spec <- HyperSpecRaster(spec, ...)
    return(spec)
  } else {
    return(list(fractions = fractions, error = error))
  }
}
