.inversionFUN <- function(P, reflectance_spectra, transmittance_spectra, sam, resample, resample_sensor, upperval, lowerval, verbose)
{
  P <- P*(upperval-lowerval)+lowerval
  pros_spectra <- PROSPECT(N = P[1], Cab = P[2], Car = P[3], Cbrown = P[4],
                           Cw = P[5], Cm = P[6], transmittance = FALSE,
                           parameterList = NULL, version = "5B")
  if (verbose)
    cat(paste0("N = ", P[1], "; Cab = ", P[2], "; Car = ", P[3], "; Cbrown = ", P[4], "; Cw = ", P[5], "; Cm = ", P[6], " => "))
    
  if (resample)
    pros_spectra <- spectralResampling(pros_spectra, sensor = resample_sensor)
  pros_spectra <- spectra(pros_spectra)[1,]
  pros_spectra[!is.finite(pros_spectra)] <- 0
  
    
  if (is.null(transmittance_spectra))
  {
    
    if (sam)
    {
      specang <- 0
      chi2 <- .Fortran("sam",
                       nspec=as.integer(1),
                       nref=as.integer(1),
                       nbands=as.integer(2101),
                       spec=as.double(pros_spectra)+0.0001,
                       specref=as.double(reflectance_spectra)+0.0001,
                       specang=as.double(specang),
                       PACKAGE="hsdar"
                      )$specang
      
      if (!is.finite(chi2))
      {
        ntry <- 0
        while (((!is.finite(chi2))*(ntry< 10)) == 1)
        {
          chi2 <- .Fortran("sam",
                           nspec=as.integer(1),
                           nref=as.integer(1),
                           nbands=as.integer(2101),
                           spec=as.double(pros_spectra)+0.0001,
                           specref=as.double(reflectance_spectra)+0.0001,
                           specang=as.double(specang),
                           PACKAGE="hsdar"
          )$specang
          ntry <- ntry + 1
        }
      }
    } else {
      chi2 <- sqrt(mean((reflectance_spectra-pros_spectra)^2))
    }
  } else {
    pros_spectra_t <- PROSPECT(N = P[1], Cab = P[2], Car = P[3], Cbrown = P[4],
                               Cw = P[5], Cm = P[6], transmittance = TRUE,
                               parameterList = NULL, version = "5B")
    if (resample)
      pros_spectra_t <- spectralResampling(pros_spectra_t, sensor = resample_sensor)
    pros_spectra_t <- spectra(pros_spectra_t)[1,]
    chi2 <- sqrt(sum((pros_spectra-reflectance_spectra)^2+(pros_spectra_t-transmittance_spectra)^2))
  }
  if (verbose)
    cat(paste(chi2, "\n"))
  return(chi2)
}
  
  PROSPECTinvert <- function(x, P0 = NULL, lower = NULL, upper = NULL,
                             transmittance_spectra = NULL, sam = FALSE, 
                             verbose = FALSE, ...)
{
  
  if (is.null(P0))
  {
    P0 <- c(1.5, 40,  8,  0.0, 0.01, 0.009)
  }
  
  if (is.null(lower))
  {
    lower <- c(1e+00,  0e+00 , 0e+00 ,-1e-10 , 5e-05 , 2e-03)
  } 
  
  if (is.null(upper))
  {
    upper <- c(3.000, 100.000  ,30.000  , 1.000  , 0.040  , 0.018)
  } 
  
   
  if (any((upper - lower) < 0))
    stop("Error: upper boundaries' values lower than lower boundaries")
  
  if (any((upper - P0) < 0))
    stop("Error: upper boundaries' values lower than initialization values (P0)")
  
  if (any((P0 - lower) < 0))
    stop("Error: lower boundaries' values higher than initialization values (P0)")    
  
  P0 <- (P0-lower)/(upper-lower)
  if (any(!is.finite(P0)))
    P0[!is.finite(P0)] <- 0
    
  x_spec <- spectra(x)[1,]
  if (is.null(transmittance_spectra))
  {
    t_spec <- NULL
  } else {
    t_spec <- spectra(transmittance_spectra)[1,]
  }
  
  resample <- nbands(x) != 2101
  if (!resample)
    resample <- !all(wavelength(x) == 400:2500)
  resample_data <- data.frame(center = wavelength(x), fwhm = fwhm(x))
  
  res <- optim(par = P0, fn = .inversionFUN, method = c("L-BFGS-B"), 
               lower = rep.int(0, 6),
               upper = rep.int(1, 6), 
               reflectance_spectra = x_spec, 
               transmittance_spectra = t_spec, 
               sam = sam, resample = resample,
               resample_sensor = resample_data, 
               upperval = upper, lowerval = lower,
               verbose = verbose, ...)
  res$par <- res$par*(upper-lower)+lower
  res$par <- c(N = res$par[1], Cab = res$par[2], Car = res$par[3], 
               Cbrown = res$par[4], Cw = res$par[5], 
               Cm = res$par[6])
  return(res)
}
