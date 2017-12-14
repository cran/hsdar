rededge <- function(x)
{
  if (!is.speclib(x))
    stop("x is not of class 'Speclib'")
  
  if (x@spectra@fromRaster)
    return(.blockwise(speclib_obj =  "x", pos = 1))
  
  #if (!x@continuousdata)
  #  stop("x must contain continuous spectral data")
  
  if (wavelength(x)[1] > 600 || wavelength(x)[length(wavelength(x))] < 900)
    stop("x does not contain relevant spectral range. Please ensure that x covers 600 to 900 nm") 
  
  n_sgolay <- floor((25/mean(x@fwhm))/2)*2+1
  if (n_sgolay < 5)
    n_sgolay <- 5
  
  D1 <- derivative.speclib(x,method="sgolay",m=1, n=n_sgolay)
  D2 <- derivative.speclib(D1,method="sgolay",m=1, n=n_sgolay)
  
  RedEdge_data <- as.data.frame(t(as.matrix(sapply(c(1:nspectra(x)), 
                                                   FUN = .rededge_apply, spectra(x), D1, D2), ncol = 6)))
  row.names(RedEdge_data) <- idSpeclib(x)
  names(RedEdge_data) <- c("R0","l0","Rp","lp","Rs","ls")
  
#  if (round)
#  {
#    RedEdge_data[,1] <- round(RedEdge_data[,1], 0)
#    RedEdge_data[,2] <- round(RedEdge_data[,2], 0)
#    RedEdge_data[,3] <- round(RedEdge_data[,3], 0)
#  }
  
  return(RedEdge_data)
}

.rededge_apply <- function(i, x, D1, D2)
{
  i <- i[1]
  tmp <- wavelength(D2) >= 660 & wavelength(D2) <= 700
  R0 <- min(x[i,tmp],na.rm=TRUE)
  l0 <- wavelength(D2)[tmp]
  l0 <- l0[which.min(abs(R0 - x[i,tmp]))]
  tmp <- wavelength(D2) >= 700 & wavelength(D2) <= 750
  tmp2 <- spectra(D1)[i,]
  tmp2[!tmp] <- -99999.9
  lp <- which.max(tmp2)
  Rp <- x[i,lp]
  lp <- wavelength(D2)[lp]
  
  tmp <- wavelength(D2) > lp & wavelength(D2) < 900
  tmp2 <- sign(spectra(D2)[i,tmp])
  tmp3 <- tmp2[-c(1,2)]*tmp2[-c(length(tmp2)-1,length(tmp2))]
  tmp3 <- c(FALSE,tmp3==-1,FALSE)
  tmp4 <- wavelength(D2)[tmp]
  tmp3 <- tmp4[tmp3]
  ls <- tmp3[1]
  if (is.finite(ls))
  {
    Rs <- x[i,wavelength(D2)==ls]
  } else {
    Rs <- NA
  }
  return(c(R0,l0,Rp,lp,Rs,ls))
  
}  