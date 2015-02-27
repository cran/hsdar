## @TODO
##1.und letzter stuetzpunkt !=NA

transform_speclib <- function(
                               data, ...,
                               method = "ch",
                               out = "bd"
                              )
{
x <- data
methodfound = FALSE
usespeclib  = FALSE

if (!is.speclib(x))
  stop("data must be of class 'Speclib'")

setmask <- if (is.null(attr(x, "setmask"))) FALSE else attr(x, "setmask")
if (setmask)
{
  dropped <- attr(x, "dropped")
  x <- interpolate.mask(x)
  for (i in 1:nrow(dropped))
    spectra(x)[,x$wavelength >= dropped[i,1] & x$wavelength <= dropped[i,2]] <- 0
}
result <- x
y <- as.data.frame(spectra(x))
x <- x$wavelength

usagehistory(result) <- paste("Reflectance = transform (",method,"), ",out,sep="")


if (method == "ch")
{
  methodfound=TRUE
  hull <- matrix(data=0,nrow=nrow(y),ncol=ncol(y))
  cp   <- y*0
  for (i in 1:nrow(y))
  {
    c.hull <- chull(t(y[i,]))
    c.hull <- c.hull[(which(c.hull == 1):length(c.hull))]
    c.hull <- sort(c.hull)
    cp[i,c.hull]   <- x[c.hull]
    hull[i,] <- approx(x=x[c.hull],y=y[i,c.hull], xout = x,method = "linear", ties = "mean")$y
  }
}
if (method == "sh")
{
  methodfound=TRUE
  hull <- y
  cp   <- y
  for (i in 1:nrow(y))
  {
    y_i <- as.vector(as.matrix(y[i,]))
    external <- .Fortran("localmaxima",
                         n      = as.integer(length(y_i)),
                         y      = as.single(y_i),
                         locmax = as.integer(c(1:length(y_i))*0),
                         PACKAGE="hsdar"
                        )
    lm <- external$locmax
    lm <- lm[lm>0]
                         
    external <- .Fortran("suh",
                         nlm  = as.integer(length(lm)), 
                         n    = as.integer(length(y)),
                         LMin = as.integer(lm),
                         y    = as.single(y_i),
                         hull = as.single(c(1:length(y))*0),
                         cp   = as.integer(c(1:length(y))*0),
                         PACKAGE="hsdar"
                        )
    hull[i,] <- external$hull
    cp[i,]   <- external$cp
  }
}

if (!methodfound) stop(paste("Unknown method '",method,"'!",sep=""))

if (out=="bd") 
{
  spectra(result) <- 1 - y/hull
  result@transformation <- "bd"
  result@ylabel <- "Band depth"
  if (setmask)
    mask(result) <- dropped
  return (result)
} else {
  if (out=="difference") 
  {
    spectra(result) <- hull - y
    result@transformation <- "difference"
    result@ylabel <- "Transformed difference"
    if (setmask)
      mask(result) <- dropped
    return (result)
  } else {
    if (out=="raw") 
    {
      return(new("Clman", result, cp = cp, hull = hull))
    } else {
      spectra(result) <- y/hull
      result@transformation <- "ratio"
      result@ylabel <- "Band depth ratio"
      if (setmask)
        mask(result) <- dropped
      return (result)
    }
  }
}
}

checkhull <- function(
                      x,
                      ispec
                     )
{
  ptscon <- getcp(x,ispec)
  
  ispec  <- ptscon$ispec
  ptscon <- ptscon$ptscon$Wavelength
  
  Reflectance  <- spectra(x)[ispec,]

  result <- c(0,0)
  hull <- Reflectance*0
  
  storage.mode(ptscon)      <- "integer"
  storage.mode(Reflectance) <- "double"
  storage.mode(result)      <- "integer"
  storage.mode(hull)        <- "double"

  external <- .Fortran("checkhull",
                       ncp     = as.integer(length(ptscon)), 
                       n       = as.integer(length(Reflectance)),
                       ptscon  = ptscon, 
                       y       = Reflectance,
                       offset  = as.integer(x$wavelength[1]-1),
                       res     = result,
                       hull    = hull,
                       PACKAGE = "hsdar"
                      )
  if (external$res[1]!=0)
    warning(paste("Mismatch of continuum line at wavelength =",external$res[1],
                  "\n  Maximum distance between continuum line & spectrum at\n",
                  " Wavelength =",external$res[2]))
  return(list(hull=external$hull,error=external$res))
}

makehull <- function(
                      x,
                      ispec
                     )
{
  ptscon <- getcp(x,ispec)
  
  ispec  <- ptscon$ispec
  ptscon <- ptscon$ptscon$Wavelength
  
  Reflectance  <- spectra(x)[ispec,]

  result <- c(0,0)
  hull <- Reflectance*0
  
  storage.mode(ptscon)      <- "integer"
  storage.mode(Reflectance) <- "double"
  storage.mode(result)      <- "integer"
  storage.mode(hull)        <- "double"

  external <- .Fortran("checkhull",
                       ncp     = as.integer(length(ptscon)), 
                       n       = as.integer(length(Reflectance)),
                       ptscon  = ptscon, 
                       y       = Reflectance,
                       offset  = as.integer(x$wavelength[1]-1),
                       res     = result,
                       hull    = hull,
                       PACKAGE = "hsdar"
                      )
  if (external$res[1]!=0)
    warning(paste("Mismatch of continuum line at wavelength =",external$res[1],
                  "\n  Maximum distance between continuum line & spectrum at\n",
                  " Wavelength =",external$res[2]))
  
  result <- external$hull
  attr(result,"ispec") <- ispec
  attr(result,"reflectance") <- spectra(x)[ispec,]
  return(result)
}

updatecl <- function (
                      x,
                      hull
                     )
{
  
  if (!is.speclib(x))
    stop("x must be of class 'Speclib'")
  
  setmask <- if (is.null(attr(x, "setmask"))) FALSE else attr(x, "setmask")
  
  if (is.null(attr(hull,"ispec")))
  {
    stop("hull must be output of function 'makehull'")
  } else {
    ispec <- attr(hull,"ispec")
    reflectance <- attr(hull,"reflectance")
  }
  
  if (setmask)
  {
    dropped <- mask(x)
    x <- interpolate.mask(x)
  }
  
  if (mode(x@transformation)=="NULL")
    stop("x must be a transformed speclib")
  if (x@transformation == "difference")
    spectra(x)[ispec,] <- hull - reflectance
  if (x@transformation == "bd")
    spectra(x)[ispec,] <- 1 - reflectance/hull
  if (x@transformation == "ratio")
    spectra(x)[ispec,] <- reflectance/hull
  
  if (setmask)
    mask(x) <- dropped
    
  return(x)
}
