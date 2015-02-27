rededge <- function(x, 
                    smooth = TRUE,
                    round = FALSE,
                    ...
                   )
{

  rededge_apply <- function(i)
  {
    i <- i[1]
    D2 <- eval.parent(D2)
    x <- eval.parent(spectra(x))
    model <- splinefun(D2$wavelength, D2$spectra[i,])
    dev2Max <- 600+which(model(600:800)==max(model(600:800)))
    dev2Min <- 600+which(model(600:800)==min(model(600:800)))
    RedEdge_data <- c(uniroot.all(model, c(600,dev2Max))[length(uniroot.all(model, c(600,dev2Max)))],
                      uniroot.all(model, c(dev2Min,900))[1],
                      uniroot.all(model, c(dev2Max,dev2Min))[1])
    R0 <- get_reflectance(x, D2$wavelength, RedEdge_data[1])[i]
    return(c(RedEdge_data, R0))
  }  
  
  if (!is.speclib(x))
    stop("x is not of class 'Speclib'")
    
  if (!x@continuousdata)
    stop("x must contain continuous spectral data")
  
  if (x$wavelength[1] > 600 || x$wavelength[length(x$wavelength)] < 900)
    stop("x does not contain relevant spectral range. Please ensure that x covers 600 to 900 nm") 
    
  D2 <- derivative.speclib(if (smooth) smoothSpeclib(x, method="spline", n=round(nbands(x)/10,0)) else x,
                           m = 2,
                           ...
                   )
  
  RedEdge_data <- as.data.frame(t(apply(data.frame(i=c(1:nspectra(x))), 1, FUN = rededge_apply)))
  
  row.names(RedEdge_data) <- idSpeclib(x)
  names(RedEdge_data) <- c("l0", "Rs", "lp", "R0")

  if (round)
  {
    RedEdge_data[,1] <- round(RedEdge_data[,1], 0)
    RedEdge_data[,2] <- round(RedEdge_data[,2], 0)
    RedEdge_data[,3] <- round(RedEdge_data[,3], 0)
  }
  
  return(RedEdge_data)
}
