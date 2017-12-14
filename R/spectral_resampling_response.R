.get.response <- function(sensor, range=NULL, response_function=TRUE, continuousdata = "auto")
{
if (!is.null(range))
  wavelength <- if (is.speclib(range)) range$wavelength else range
  
if (is.data.frame(sensor))
{
  if (continuousdata=="auto") 
    continuousdata <- FALSE

  if (any(toupper(names(sensor))=="FWHM"))
  {
    fwhm <- sensor[, which(toupper(names(sensor))=="FWHM")]
  } else {
    fwhm <- if (pmatch("FWHM", toupper(names(sensor)), nomatch = 0)==0) NULL else sensor[, pmatch("FWHM", toupper(names(sensor)))]
  }
  if (any(toupper(names(sensor))=="CENTER"))
  {
    centerwl <- sensor[, which(toupper(names(sensor))=="CENTER")]
  } else {
    centerwl <- if (pmatch("CENTER", toupper(names(sensor)), nomatch = 0)==0) NULL else sensor[, pmatch("CENTER", toupper(names(sensor)))]
  }
  if (any(c(is.null(fwhm), is.null(centerwl))))
  {
    if (any(toupper(names(sensor))=="LB"))
    {
      lb <- sensor[, which(toupper(names(sensor))=="LB")] 
    } else {
      lb <- if (pmatch("LB", toupper(names(sensor)), nomatch = 0)==0) NULL else sensor[, pmatch("LB", toupper(names(sensor)))]
    }
    if (any(toupper(names(sensor))=="UB"))
    {
      ub <- sensor[, which(toupper(names(sensor))=="UB")] 
    } else {
      ub <- if (pmatch("UB", toupper(names(sensor)), nomatch = 0)==0) NULL else sensor[, pmatch("UB", toupper(names(sensor)))]
    }
    if (any(c(is.null(lb), is.null(ub))))
    {    
      lb<-sensor[,1]
      ub<-sensor[,2]
    }
    centerwl <- lb + (ub - lb)/2
    fwhm <- (centerwl - lb) * 2
  } else {
    lb <- centerwl - fwhm/2
    ub <- centerwl + fwhm/2
  }
  
  nch <- length(centerwl)
  if (is.speclib(response_function))
  {
    response <- as.data.frame(t(spectra(response_function)))
    names(response) <- paste("Band",c(1:nspectra(response_function)),sep="_")
    attr(response,"minwl") <- wavelength(response_function)[1]
    attr(response,"maxwl") <- wavelength(response_function)[nbands(response_function)]
    attr(response,"stepsize") <- response_function@fwhm
    attr(response,"wlunit") <- response_function@wlunit
  } else {    
    sc <- data.frame(No=c(1:nch), center=centerwl, fwhm=fwhm)
    attr(sc, "fwhm") <- TRUE
    response <- get.gaussian.response(sc)
  }
  if (is.null(range))
    wavelength <- c(lb[1]:ub[nch])
} else {
  sc <- get.sensor.characteristics(sensor,response_function=response_function)
  if (is.null(sc))
  {
    return(NULL)
  } else {
    if (response_function)
    {
      lb <- sc$characteristics$lb
      ub <- sc$characteristics$ub
      nch <- length(lb)
      response <- sc$response
    } else {
      if (!attr(sc, "fwhm"))
      {
        lb <- sc[,attr(sc, "50pass")[1]]
        ub <- sc[,attr(sc, "50pass")[2]]
        centerwl <- lb + (ub - lb)/2
        fwhm <- (centerwl - lb) * 2
        sc <- data.frame(No=c(1:length(centerwl)), center=centerwl, fwhm=fwhm)
      } else {
        lb <- sc$center - sc$fwhm/2
        ub <- sc$center + sc$fwhm/2
      }
      attr(sc, "fwhm") <- TRUE
      nch <- nrow(sc)
      response <- get.gaussian.response(sc)
      
    }
    if (is.null(range))
      wavelength <- c(lb[1]:ub[nch])
  }
}
unit <- attr(response,"wlunit")

responsedim <- c(as.double(attr(response, "minwl")),
                 as.double(attr(response, "maxwl")),
                 as.double(attr(response, "stepsize")))
cha_names <- names(response)
nwlresponse <- nrow(response)
response <- as.double(as.matrix(response))
response_transformed <- matrix(data=0, nrow=length(wavelength), ncol=nch)
response_transformed <- .Fortran("transform_response",
                                 nwl=as.integer(length(wavelength)), 
                                 nband=as.integer(nch), 
                                 nwlresponse=as.integer(nwlresponse), 
                                 responsedim=responsedim, 
                                 response=response,
                                 response_transformed=as.double(response_transformed),
                                 wl=as.double(wavelength),
                                 package="hsdar"
                                )
response_transformed <- matrix(response_transformed$response_transformed,ncol=nch)

response_transformed[response_transformed<0] <- 0
response_transformed[response_transformed>1] <- 1
result <- speclib(spectra=response_transformed, wavelength=wavelength)
idSpeclib(result) <- cha_names
attr(result, "wlunit") <- unit
attr(result, "minwl") <- responsedim[1]
attr(result, "maxwl") <- responsedim[2]
attr(result, "stepsize") <- responsedim[3]
attr(result, "lb") <- lb
attr(result, "ub") <- ub
attr(result, "is.response") <- TRUE
return(result)
  
}

.transform_irr_response <- function(spectral_response, wavelength = NULL)
{
  if (is.null(wavelength))
    wavelength <- c(floor(wavelength(spectral_response)[1]):ceiling(wavelength(spectral_response)[nbands(spectral_response)]))
    
  response <- spectra(spectral_response)

  nch <- nspectra(spectral_response)
  nwlresponse <- ncol(response)
  response <- as.double(t(as.matrix(response)))
  response_transformed <- matrix(data=0, nrow=length(wavelength), ncol=nch)
  response_transformed <- .Fortran("transform_irregular_response",
                                   nwl=as.integer(length(wavelength)), 
                                   nband=as.integer(nch), 
                                   nwlresponse=as.integer(nwlresponse), 
                                   response=response,
                                   response_transformed=as.double(response_transformed),
                                   wl=as.double(wavelength),
                                   wl_response=as.double(wavelength(spectral_response)),
                                   package="hsdar"
                                  )

  response_transformed_2 <- speclib(matrix(response_transformed$response_transformed, ncol = nch, byrow = FALSE), wavelength)
  return(response_transformed_2)
}