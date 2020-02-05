.getcorfac <- function(crossing, x)
  return(spectra(x)[,crossing + 1] - (2* spectra(x)[,crossing] - spectra(x)[,crossing - 1]))
  
.applcorfac <- function(cor_fac, crossing, x)
{
  cor_fac <- c(list(rep.int(0, nspectra(x))), cor_fac)
  cor_fac_cu <- cor_fac[[1]]
  crossing <- c(0, crossing, nbands(x))
  
  spec <- spectra(x)
  for (i in 1:length(cor_fac))
  {
    cor_fac_cu <- cor_fac_cu + cor_fac[[i]]
    spec[,c((crossing[i]+1):crossing[i+1])] <- spec[,c((crossing[i]+1):crossing[i+1])] - cor_fac_cu
  }
  spec[spec < 0] <- 0
  return(spec)
}

read.ASD <- function(f, type = "reflectance", ...)
{
  if (length(f) > 1)
  {
    for (i in 1:length(f))
    {      
      if (i > 1)
      {
        dat <- merge(dat, read.ASD(f[i], type = type, ...))
      } else {
        dat <- read.ASD(f[i], type = type, ...)
      }
    }
    usagehistory(dat) <- NULL
    usagehistory(dat) <- paste("Spectra from", length(f), "ASD-files read")
    return(dat)
  }
  if (!requireNamespace("asdreader", quietly = TRUE))
    stop("Library 'asdreader' is required to read ASD file(s)")
    
  dat_raw <- asdreader::get_spectra(f, type = type)
  dat <- speclib(dat_raw, as.numeric(attr(dat_raw, "dimnames")[[2]]))
  SI(dat) <- data.frame(file = attr(dat_raw, "dimnames")[[1]], stringsAsFactors = FALSE)
  return(dat)
}

postprocessASD <- function(x, reference, removeCrossings = TRUE, correctReflectance = TRUE)
{
  if (removeCrossings)
  {
    crossings <- c(1000, 1800)
    crossings <- sapply(crossings, function(x, wv) which.min(abs(wv - x)), wavelength(x))
    cor_fac <- lapply(as.list(crossings), .getcorfac, x)
    spectra(x) <- .applcorfac(cor_fac, crossings, x)
  }
  
  if (correctReflectance)
  {
    if (nspectra(reference) != 1)
    {
      stop("Reference speclib must contain exactly one spectrum")
    }
    spectra(x) <- t(apply(spectra(x), 1, function(v, r) return(v*r), spectra(reference)))
  }
  
  if (all(c(removeCrossings, correctReflectance)))
  {
    usagehistory(x) <- "Channel crossing removed and reflectance corrected"
  } else {
    if (removeCrossings)
    {
      usagehistory(x) <- "Channel crossing removed"
    } else {
      usagehistory(x) <- "Reflectance corrected"
    }
  }
  return(x)
}
