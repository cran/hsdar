setMethod("dim", signature(x = "Speclib"), 
                 definition = function(x)
{
  dimX <- c(nrow(x@spectra), ncol(x@spectra))
  return(dimX)
}
)

nspectra <- function(x)
{
  if (class(x) == "Nri")
    return(dim(x)[3])
  return(dim(x)[1])
}


nbands <- function(x)
{
  stopifnot(is.speclib(x))
  return(dim(x)[2])
}


is.speclib <- function(x)
  any(c(class(x) == "Speclib",
        class(x) == "Specfeat"))