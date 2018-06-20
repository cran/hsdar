.inversionFUN <- function(P, reflectance_spectra, transmittance_spectra, sam, resample, resample_sensor)
{
  pros_spectra <- PROSPECT(N = P[1]/10, Cab = P[2], Car = P[3], Cbrown = P[4]/100,
                           Cw = P[5]/100, Cm = P[6]/100, transmittance = FALSE,
                           parameterList = NULL, version = "5B")
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
                       spec=as.double(pros_spectra),
                       specref=as.double(reflectance_spectra),
                       specang=as.double(specang),
                       PACKAGE="hsdar"
                      )$specang
    } else {
      chi2 <- sqrt(mean((reflectance_spectra-pros_spectra)^2))
    }
  } else {
    pros_spectra_t <- PROSPECT(N = P[1]/10, Cab = P[2], Car = P[3], Cbrown = P[4]/100,
                               Cw = P[5]/100, Cm = P[6]/100, transmittance = TRUE,
                               parameterList = NULL, version = "5B")
    if (resample)
      pros_spectra_t <- spectralResampling(pros_spectra_t, sensor = resample_sensor)
    pros_spectra_t <- spectra(pros_spectra_t)[1,]
    chi2 <- sqrt(sum((pros_spectra-reflectance_spectra)^2+(pros_spectra_t-transmittance_spectra)^2))
  }
  return(chi2)
}
  
PROSPECTinvert <- function(x, P0 = NULL, transmittance_spectra = NULL, sam = FALSE, ...)
{
#   if (!requireNamespace("pracma", quietly = TRUE))
#     stop("Library 'pracma' is required to invert PROSPECT")
  
  if (is.null(P0))
  {
    P0 <- (c(10, 0, 0, 0, 0.005, 0.2) + c(30, 100, 30, 100, 4, 1.8))/2
  } else {
    P0 <- c(P0[1]*10, P0[2], P0[3], P0[4]*100, P0[5]*100, P0[6]*100)
  }
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
               lower = c(10, 0, 0, -1e-08, 0.005, 0.2),
               upper = c(30, 100, 30, 100, 4, 1.8), 
               reflectance_spectra = x_spec, 
               transmittance_spectra = t_spec, 
               sam = sam, resample = resample,
               resample_sensor = resample_data, ...)
  
  res$par <- c(N = res$par[1]/10, Cab = res$par[2], Car = res$par[3], 
               Cbrown = res$par[4]/100, Cw = res$par[5]/100, 
               Cm = res$par[6]/100)
  return(res)
}