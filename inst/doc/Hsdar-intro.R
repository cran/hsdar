### R code from vignette source 'Hsdar-intro.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Hsdar-intro.Rnw:53-54
###################################################
options(width=70)


###################################################
### code chunk number 2: Hsdar-intro.Rnw:82-85
###################################################
#install.packages("hsdar")
library(hsdar)
data(spectral_data) #Load the data used in the tutorial


###################################################
### code chunk number 3: Hsdar-intro.Rnw:90-91 (eval = FALSE)
###################################################
## help(hsdar)


###################################################
### code chunk number 4: Hsdar-intro.Rnw:104-105
###################################################
spectral_data #See how Speclibs are printed


###################################################
### code chunk number 5: Hsdar-intro.Rnw:112-113 (eval = FALSE)
###################################################
## str(spectral_data)


###################################################
### code chunk number 6: Hsdar-intro.Rnw:116-117
###################################################
str(spectral_data)


###################################################
### code chunk number 7: Hsdar-intro.Rnw:135-136
###################################################
spectra <- spectra(spectral_data)


###################################################
### code chunk number 8: Hsdar-intro.Rnw:142-143
###################################################
str(spectra)


###################################################
### code chunk number 9: Hsdar-intro.Rnw:149-150
###################################################
wavelength <- wavelength(spectral_data)


###################################################
### code chunk number 10: Hsdar-intro.Rnw:155-156
###################################################
newSpeclib <- speclib(spectra, wavelength)


###################################################
### code chunk number 11: Hsdar-intro.Rnw:161-162 (eval = FALSE)
###################################################
## str(newSpeclib)


###################################################
### code chunk number 12: Hsdar-intro.Rnw:165-166
###################################################
str(newSpeclib)


###################################################
### code chunk number 13: Hsdar-intro.Rnw:172-175
###################################################
ids <- idSpeclib(spectral_data) #extract ID from "spectral_data"
idSpeclib(newSpeclib) <- as.character(ids) #...and assign them to the 
                                           #new Speclib


###################################################
### code chunk number 14: Hsdar-intro.Rnw:181-183
###################################################
SI <- SI(spectral_data) 
head(SI)


###################################################
### code chunk number 15: Hsdar-intro.Rnw:188-189
###################################################
SI(newSpeclib) <- SI


###################################################
### code chunk number 16: Hsdar-intro.Rnw:194-195 (eval = FALSE)
###################################################
## str(newSpeclib)


###################################################
### code chunk number 17: Hsdar-intro.Rnw:198-199
###################################################
str(newSpeclib)


###################################################
### code chunk number 18: Hsdar-intro.Rnw:216-236
###################################################
## Create raster file using PROSAIL
## Run PROSAIL
parameter <- data.frame(N = c(rep.int(seq(0.5, 1.4, 0.1), 6)),
                        LAI = c(rep.int(0.5, 10), rep.int(1, 10), 
                                rep.int(1.5, 10), rep.int(2, 10), 
                                rep.int(2.5, 10), rep.int(3, 10)))
spectra <- PROSAIL(parameterList = parameter)

## Create SpatialPixelsDataFrame and fill data with spectra from 
## PROSAIL
rows <- round(nspectra(spectra)/10, 0)
cols <- ceiling(nspectra(spectra)/rows)
grd <- SpatialGrid(GridTopology(cellcentre.offset = c(1,1,1), 
                                cellsize = c(1,1,1), 
                                cells.dim = c(cols, rows, 1)))
x <- SpatialPixelsDataFrame(grd, 
                            data = as.data.frame(spectra(spectra)))

## Write data to example file (example_in.tif) in workingdirectory
writeGDAL(x, fname = "example_in.tif", drivername = "GTiff")


###################################################
### code chunk number 19: Hsdar-intro.Rnw:241-245
###################################################
infile <- "example_in.tif"
wavelength <- wavelength(spectra)
ra <- speclib(infile, wavelength)
tr <- blockSize(ra)


###################################################
### code chunk number 20: Hsdar-intro.Rnw:250-261
###################################################
outfile <- "example_result.tif" 
n_veg <- as.numeric(length(vegindex()))
res <- writeStart(ra, outfile, overwrite = TRUE, nl = n_veg)
for (i in 1:tr$n) 
{
  v <- getValuesBlock(ra, row=tr$row[i], nrows=tr$nrows[i])
  mask(v) <- c(1350, 1450)
  v <- as.matrix(vegindex(v, index=vegindex()))
  res <- writeValues(res, v, tr$row[i])
}
res <- writeStop(res)


###################################################
### code chunk number 21: Hsdar-intro.Rnw:271-275
###################################################
LAI <- SI(spectra)$LAI
SI_file <- "example_SI.tif" 
SI_raster <- setValues(raster(infile), LAI)
SI_raster <- writeRaster(SI_raster, SI_file)


###################################################
### code chunk number 22: Hsdar-intro.Rnw:280-294
###################################################
outfile <- "example_result_ndvi.tif" 
SI(ra) <- raster(SI_file)
names(SI(ra)) <- "LAI"
res <- writeStart(ra, outfile, overwrite = TRUE, nl = 1)
for (i in 1:tr$n) 
{
  v <- getValuesBlock(ra, row=tr$row[i], nrows=tr$nrows[i])
  mask(v) <- c(1350, 1450)
  LAI <- SI(v)$LAI
  v <- as.matrix(vegindex(v, index="NDVI"))
  v[LAI <= 1] <- NA
  res <- writeValues(res, v, tr$row[i])
}
res <- writeStop(res)


###################################################
### code chunk number 23: Hsdar-intro.Rnw:300-301
###################################################
plot(raster("example_result_ndvi.tif"))


###################################################
### code chunk number 24: Hsdar-intro.Rnw:356-361 (eval = FALSE)
###################################################
## par(mfrow = c(2,2))
## plot(spectral_data, main = "Default Plot")
## plot(spectral_data, FUN = 1, main = "First spectrum of Speclib")
## plot(spectral_data, FUN = "median", main = "Median spectrum")
## plot(spectral_data, FUN = "mean", main = "Mean spectrum")


###################################################
### code chunk number 25: Hsdar-intro.Rnw:364-369
###################################################
par(mfrow = c(2,2), mar = c(4,4,3,0.1), cex = 0.75)
plot(spectral_data, main = "Default Plot", ylim = c(0, 70))
plot(spectral_data, FUN = 1, main = "First spectrum of Speclib", ylim = c(0, 70))
plot(spectral_data, FUN = "median", main = "Median spectrum", ylim = c(0, 70))
plot(spectral_data, FUN = "mean", main = "Mean spectrum", ylim = c(0, 70))


###################################################
### code chunk number 26: Hsdar-intro.Rnw:378-381 (eval = FALSE)
###################################################
## plot(spectral_data, FUN = 1, col = "red") 
## plot(spectral_data, FUN = 2, col = "blue", new = FALSE)
## plot(spectral_data, FUN = 3, col = "orange", new = FALSE)


###################################################
### code chunk number 27: Hsdar-intro.Rnw:384-388
###################################################
par(mar = c(4,4,0.1,0.1), cex = 0.75)
plot(spectral_data, FUN = 1, col = "red") 
plot(spectral_data, FUN = 2, col = "blue", new = FALSE)
plot(spectral_data, FUN = 3, col = "orange", new = FALSE)


###################################################
### code chunk number 28: Hsdar-intro.Rnw:413-424
###################################################
## Simulate first spectrum with lower chlorophyll content
spectrum1 <- PROSPECT(N = 1.3, Cab = 30, Car = 10, Cbrown = 0, 
                      Cw = 0.01, Cm = 0.01)
## Simulate second spectrum with higher chlorophyll content
spectrum2 <- PROSPECT(N = 1.3, Cab = 60, Car = 10, Cbrown = 0, 
                      Cw = 0.01, Cm = 0.01)

## Plot results:
plot(spectrum1, col = "darkorange4", ylim = c(0,0.5), 
     subset = c(400, 800))
plot(spectrum2, col = "darkgreen", new = FALSE)


###################################################
### code chunk number 29: Hsdar-intro.Rnw:427-431
###################################################
par(mar = c(4,4,0.1,0.1), cex = 0.75)
plot(spectrum1, col = "darkorange4", ylim = c(0,0.5), 
     subset = c(400, 800))
plot(spectrum2, col = "darkgreen", new = FALSE)


###################################################
### code chunk number 30: Hsdar-intro.Rnw:438-449
###################################################
## Defining parameter
parameter <- data.frame(tts = seq(15, 85, 0.5))
head(parameter)

## Perform simulation (all other parameters are set to default
## values)
spectra <- PROSAIL(parameterList = parameter)
spectra

## Let's see the SI
summary(SI(spectra))


###################################################
### code chunk number 31: Hsdar-intro.Rnw:454-462
###################################################
colours <- colorRamp(c("darkorange4", "yellow"))
plot(spectra[1,], ylim = c(0, 0.3),
     col = rgb(colours(SI(spectra)$tts[1]/85),
               maxColorValue = 255))
for (i in 2:nspectra(spectra))
  plot(spectra[i,], new = FALSE,
       col = rgb(colours(SI(spectra)$tts[i]/85),
                 maxColorValue = 255))


###################################################
### code chunk number 32: Hsdar-intro.Rnw:465-474
###################################################
par(mar = c(4,4,0.1,0.1), cex = 0.75)
colours <- colorRamp(c("darkorange4", "yellow"))
plot(spectra[1,], ylim = c(0, 0.3),
     col = rgb(colours(SI(spectra)$tts[1]/85),
               maxColorValue = 255))
for (i in 2:nspectra(spectra))
  plot(spectra[i,], new = FALSE,
       col = rgb(colours(SI(spectra)$tts[i]/85),
                 maxColorValue = 255))


###################################################
### code chunk number 33: Hsdar-intro.Rnw:480-514
###################################################
## Defining parameter
parameter <- data.frame(LAI = rep.int(c(1,2,3),5),
                        TypeLidf = 1,
                        lidfa = c(rep.int(1,3), rep.int(-1,3), 
                                  rep.int(0,6), rep.int(-0.35,3)),
                        lidfb = c(rep.int(0,6), rep.int(-1,3), 
                                  rep.int(1,3), rep.int(-0.15,3)))
parameter

## Perform simulation
spectra <- PROSAIL(parameterList = parameter)
spectra

## Plot result:
## Colour indicates LAI
## Line style indicates LIDF type
colours <- c("darkblue", "red", "darkgreen")
LIDF_type <- as.factor(c(rep.int("Planophile", 3), 
                         rep.int("Erectophile", 3),
                         rep.int("Plagiophile", 3),
                         rep.int("Extremophile", 3), 
                         rep.int("Spherical", 3)))

plot(spectra[1,], ylim = c(0, 0.5),
     col = colours[SI(spectra)$LAI[1]], 
     lty = which(levels(LIDF_type) == LIDF_type[1]))
for (i in 2:nspectra(spectra))
  plot(spectra[i,], new= FALSE,
       col = colours[SI(spectra)$LAI[i]],
       lty = which(levels(LIDF_type) == LIDF_type[i]))
legend("topright", 
       legend = c(paste("LAI =", c(1:3)), "", levels(LIDF_type)), 
       col = c(colours, rep.int("black", 1 + length(levels(LIDF_type)))),
       lty = c(rep.int(1, 3), 0, 1:length(levels(LIDF_type))))


###################################################
### code chunk number 34: Hsdar-intro.Rnw:517-529
###################################################
par(mar = c(4,4,0.1,0.1), cex = 0.75)
plot(spectra[1,], ylim = c(0, 0.5),
     col = colours[SI(spectra)$LAI[1]], 
     lty = which(levels(LIDF_type) == LIDF_type[1]))
for (i in 2:nspectra(spectra))
  plot(spectra[i,], new= FALSE,
       col = colours[SI(spectra)$LAI[i]],
       lty = which(levels(LIDF_type) == LIDF_type[i]))
legend("topright", 
       legend = c(paste("LAI =", c(1:3)), "", levels(LIDF_type)), 
       col = c(colours, rep.int("black", 1 + length(levels(LIDF_type)))),
       lty = c(rep.int(1, 3), 0, 1:length(levels(LIDF_type))))


###################################################
### code chunk number 35: Hsdar-intro.Rnw:541-552
###################################################
## Return names of SI data
names(SI(spectral_data))

## Devide into both seasons using to the SI attribute "season"
sp_spring <- subset(spectral_data, season == "spring")
sp_summer <- subset(spectral_data, season == "summer")
#
#Plot results:
#
plot(sp_spring, FUN = "mean", col = "darkgreen", ylim = c(0,70))
plot(sp_summer, FUN = "mean", col = "darkred", new = FALSE)


###################################################
### code chunk number 36: Hsdar-intro.Rnw:555-558
###################################################
par(mar = c(4,4,0.1,0.1), cex = 0.75)
plot(sp_spring, FUN = "mean", col = "darkgreen", ylim = c(0,70))
plot(sp_summer, FUN = "mean", col = "darkred", new = FALSE)


###################################################
### code chunk number 37: Hsdar-intro.Rnw:570-578
###################################################
spectral_data_masked <- spectral_data
mask(spectral_data_masked) <- c(1040,1060,1300,1450)
#
#plot results:
#
par(mfrow = c(1,2))
plot(spectral_data, FUN = 1)
plot(spectral_data_masked, FUN = 1)


###################################################
### code chunk number 38: Hsdar-intro.Rnw:581-584
###################################################
par(mfrow=c(1,2), mar = c(4,4,0.1,0.1), cex = 0.75)
plot(spectral_data, FUN = 1)
plot(spectral_data_masked, FUN = 1)


###################################################
### code chunk number 39: Hsdar-intro.Rnw:591-593
###################################################
spectral_data_interpolated <- interpolate.mask(spectral_data_masked)
plot(spectral_data_interpolated, FUN = 1)


###################################################
### code chunk number 40: Hsdar-intro.Rnw:596-598
###################################################
par(mar = c(4,4,0,0.1), cex = 0.75)
plot(spectral_data_interpolated, FUN = 1)


###################################################
### code chunk number 41: Hsdar-intro.Rnw:602-603
###################################################
spectral_data <- spectral_data_masked


###################################################
### code chunk number 42: Hsdar-intro.Rnw:611-612 (eval = FALSE)
###################################################
## plot(spectral_data, FUN = 1, subset = c(1200,1300)) #raw spectrum


###################################################
### code chunk number 43: Hsdar-intro.Rnw:615-617
###################################################
par(mar = c(4,4,0.1,0.1), cex = 0.75)
plot(spectral_data, FUN = 1, subset = c(1200,1300)) #raw spectrum


###################################################
### code chunk number 44: Hsdar-intro.Rnw:625-633
###################################################
#
#Filter Speclib:
#
sgolay <- smoothSpeclib(spectral_data, method = "sgolay", n = 25)
lowess <- smoothSpeclib(spectral_data, method = "lowess", f = .01)
meanflt <- smoothSpeclib(spectral_data, method = "mean", p = 5)
spline <- smoothSpeclib(spectral_data, method = "spline", 
                         n = round(nbands(spectral_data)/10, 0))


###################################################
### code chunk number 45: Hsdar-intro.Rnw:638-654 (eval = FALSE)
###################################################
## par(mfrow = c(2,2))
## plot(sgolay, FUN = 1, subset = c(1200,1300), col = "red",
##      main = "Savitzky-Golay-Filter")
## plot(spectral_data, FUN = 1, new = FALSE) #raw spectrum
## 
## plot(lowess, FUN = 1, subset = c(1200,1300), col = "red",
##      main = "Lowess-Filter")
## plot(spectral_data, FUN = 1, new = FALSE) #raw spectrum
## 
## plot(meanflt, FUN = 1, subset = c(1200,1300), col = "red",
##      main = "Mean-filter")
## plot(spectral_data, FUN = 1, new = FALSE) #raw spectrum
## 
## plot(spline, FUN = 1, subset = c(1200,1300), col = "red",
##      main = "Spline-Filter")
## plot(spectral_data, FUN = 1, new = FALSE) #raw spectrum


###################################################
### code chunk number 46: Hsdar-intro.Rnw:657-673
###################################################
par(mfrow=c(2,2), mar = c(4,4,3,0.1), cex = 0.75)
plot(sgolay, FUN = 1, subset = c(1200,1300), col = "red",
     main = "Savitzky-Golay-Filter")
plot(spectral_data, FUN = 1, new = FALSE) #raw spectrum

plot(lowess, FUN = 1, subset = c(1200,1300), col = "red",
     main = "Lowess-Filter")
plot(spectral_data, FUN = 1, new = FALSE) #raw spectrum

plot(meanflt, FUN = 1, subset = c(1200,1300), col = "red",
     main = "Mean-filter")
plot(spectral_data, FUN = 1, new = FALSE) #raw spectrum

plot(spline, FUN = 1, subset = c(1200,1300), col = "red",
     main = "Spline-Filter")
plot(spectral_data, FUN = 1, new = FALSE) #raw spectrum


###################################################
### code chunk number 47: Hsdar-intro.Rnw:684-703
###################################################
spectral_data_1deriv <- derivative.speclib(spectral_data, m = 1)
spectral_data_2deriv <- derivative.speclib(spectral_data, m = 2)

## Get index of red edge wavelength
redEdgePart <- wavelength(spectral_data_2deriv) >= 600 & 
               wavelength(spectral_data_2deriv) <= 800
               
## Cut spectra to red edge
spectral_data_1deriv <- spectral_data_1deriv[,redEdgePart]
spectral_data_2deriv <- spectral_data_2deriv[,redEdgePart]
  
#
#plot derivations of the red edge area of 1. spectrum in the Speclib:
#
par(mfrow=c(1,2))
plot(spectral_data_1deriv, FUN = 1, xlim = c(600,800),
     main = "First derivation")
plot(spectral_data_2deriv, FUN = 1, xlim = c(600,800),
     main = "Second Derivation")


###################################################
### code chunk number 48: Hsdar-intro.Rnw:706-711
###################################################
par(mfrow = c(1,2), mar = c(4,4,3,0.1), cex = 0.75)
plot(spectral_data_1deriv, FUN = 1, xlim = c(600,800),
     main = "First derivation")
plot(spectral_data_2deriv, FUN = 1, xlim = c(600,800),
     main = "Second Derivation")


###################################################
### code chunk number 49: Hsdar-intro.Rnw:717-729
###################################################
spectral_data_1deriv <- derivative.speclib(smoothSpeclib(
  spectral_data, method = "sgolay", n = 35), m = 1)
spectral_data_2deriv <- derivative.speclib(smoothSpeclib(
  spectral_data, method = "sgolay", n = 35), m = 2)
#
#Plot results:
#
par(mfrow=c(1,2))
plot(spectral_data_1deriv, FUN = 1, xlim = c(600,800),
     main = "First derivation")
plot(spectral_data_2deriv, FUN = 1, xlim = c(600,800),
     main = "Second Derivation")


###################################################
### code chunk number 50: Hsdar-intro.Rnw:732-740
###################################################
## Cut spectra to red edge
spectral_data_1deriv <- spectral_data_1deriv[,redEdgePart]
spectral_data_2deriv <- spectral_data_2deriv[,redEdgePart]
par(mfrow = c(1,2), mar = c(4,4,3,0.1), cex = 0.75)
plot(spectral_data_1deriv, FUN = 1, xlim = c(600,800),
     main = "First derivation")
plot(spectral_data_2deriv, FUN = 1, xlim = c(600,800),
     main = "Second Derivation")


###################################################
### code chunk number 51: Hsdar-intro.Rnw:749-750
###################################################
get.sensor.characteristics(0)


###################################################
### code chunk number 52: Hsdar-intro.Rnw:756-759
###################################################
## use spectral response function
spectral_data_resampled <- spectralResampling(spectral_data, 
                                              "WorldView2-8")


###################################################
### code chunk number 53: Hsdar-intro.Rnw:764-769
###################################################
spectral_data_resampled
wavelength(spectral_data_resampled)
#
#plot results:
plot(spectral_data_resampled)


###################################################
### code chunk number 54: Hsdar-intro.Rnw:772-774
###################################################
par(mar = c(4,4,0.1,0.1), cex = 0.75)
plot(spectral_data_resampled)


###################################################
### code chunk number 55: Hsdar-intro.Rnw:789-793
###################################################
data(spectral_data)
spectral_data_preproc <- smoothSpeclib(spectral_data, 
                                       method = "sgolay", n = 5)
mask(spectral_data_preproc) <- c(1040,1060,1300,1450)


###################################################
### code chunk number 56: Hsdar-intro.Rnw:798-799
###################################################
str(transformSpeclib)


###################################################
### code chunk number 57: Hsdar-intro.Rnw:807-819
###################################################
#convex hull:
ch_cline <- transformSpeclib(spectral_data_preproc[16:30,],
                             method = "ch", out = "raw")
ch_bd <- transformSpeclib(spectral_data_preproc,
                          method = "ch", out = "bd")
#
#segmented hull:
#
sh_cline <- transformSpeclib(spectral_data_preproc,
                             method = "sh", out = "raw")
sh_bd <- transformSpeclib(spectral_data_preproc,
                          method = "sh", out = "bd")


###################################################
### code chunk number 58: Hsdar-intro.Rnw:824-833 (eval = FALSE)
###################################################
## #plot results for the first spectrum:
## #
## par(mfrow = c(2,2))
## plot(ch_cline, ispec = 1, numeratepoints = FALSE,
##      main = "Convex hull - Continuum line")
## plot(ch_bd, ispec = 1, main = "Convex hull - Band depth")
## plot(sh_cline, ispec = 1, numeratepoints = FALSE,
##      main = "Segmented hull - Continuum line")
## plot(sh_bd, ispec = 1, main = "Segmented hull - Band depth")


###################################################
### code chunk number 59: Hsdar-intro.Rnw:836-843
###################################################
par(mfrow=c(2,2), mar = c(4,4,3,0.1), cex = 0.75)
plot(ch_cline, ispec = 1, numeratepoints = FALSE,
     main = "Convex hull - Continuum line")
plot(ch_bd, ispec = 1, main = "Convex hull - Band depth")
plot(sh_cline, ispec = 1, numeratepoints = FALSE,
     main = "Segmented hull - Continuum line")
plot(sh_bd, ispec = 1, main = "Segmented hull - Band depth")


###################################################
### code chunk number 60: Hsdar-intro.Rnw:852-857 (eval = FALSE)
###################################################
## par(mfrow = c(1,2))
## plot(sh_cline, ispec = 1, main = "Continuum line, Spectrum 1",
##      subset = c(500,800)) #first spectrum
## plot(sh_cline, ispec = 5, main = "Continuum line, Spectrum 5",
##      subset = c(500,800)) #fifth spectrum


###################################################
### code chunk number 61: Hsdar-intro.Rnw:860-865
###################################################
par(mfrow=c(1,2), mar = c(4,4,3,0.1), cex = 0.75)
plot(sh_cline, ispec = 1, main = "Continuum line, Spectrum 1",
     subset = c(500,800)) #first spectrum
plot(sh_cline, ispec = 5, main = "Continuum line, Spectrum 5",
     subset = c(500,800)) #fifth spectrum


###################################################
### code chunk number 62: Hsdar-intro.Rnw:881-882
###################################################
str(deletecp)


###################################################
### code chunk number 63: Hsdar-intro.Rnw:887-891
###################################################
getcp(sh_cline, 1, subset = c(500, 700)) #see all points
sh_cline <- deletecp(sh_cline, 1, 
                     c(500:700)) #delete all between 500 and 700 nm
getcp(sh_cline, 1, subset = c(500, 700)) #see what happened


###################################################
### code chunk number 64: Hsdar-intro.Rnw:896-897
###################################################
#sh_cline <- addcp(sh_cline, 1, 460)


###################################################
### code chunk number 65: Hsdar-intro.Rnw:903-904
###################################################
checkhull(sh_cline, 1)$error


###################################################
### code chunk number 66: Hsdar-intro.Rnw:909-910
###################################################
sh_cline <- addcp(sh_cline, 1, 1060)


###################################################
### code chunk number 67: Hsdar-intro.Rnw:915-916
###################################################
checkhull(sh_cline, 1)$error


###################################################
### code chunk number 68: Hsdar-intro.Rnw:921-922
###################################################
sh_clineUpdate <- makehull(sh_cline, 1) #update the hull of spectrum 1


###################################################
### code chunk number 69: Hsdar-intro.Rnw:927-928
###################################################
sh_bd <- updatecl(sh_bd, sh_clineUpdate) #update the band depth


###################################################
### code chunk number 70: Hsdar-intro.Rnw:933-940 (eval = FALSE)
###################################################
## #plot new line:
## par (mfrow = c(1,2))
## plot(sh_cline, ispec = 1, main = "Updated Segmented hull", 
##      xlim = c(300,800))
## #plot new band depth
## plot(sh_bd[1,], main="Updated hull - Band depth",
##      xlim = c(300,800))


###################################################
### code chunk number 71: Hsdar-intro.Rnw:943-949
###################################################
par(mfrow=c(1,2), mar = c(4,4,3,0.1), cex = 0.75)
plot(sh_cline, ispec = 1, main = "Updated Segmented hull", 
     xlim = c(300,800))
#plot new band depth
plot(sh_bd[1,], main="Updated hull - Band depth",
     xlim = c(300,800))


###################################################
### code chunk number 72: Hsdar-intro.Rnw:955-991 (eval = FALSE)
###################################################
## data(spectral_data)
## 
## spectral_data_preproc <- smoothSpeclib(spectral_data,
##                                         method = "sgolay", n = 5)
## mask(spectral_data_preproc) <- c(1040,1060,1300,1450)
## 
## sh_cline <- transformSpeclib(spectral_data_preproc,
##                              method = "sh", out = "raw")
## par (mfrow = c(2,1))
## plot(sh_cline, 1, subset = c(550, 650))
## plot(sh_cline, 5, subset = c(550, 650))
## 
## getcp(sh_cline, 1, subset = c(500, 700)) 
## getcp(sh_cline, 5, subset = c(500, 700))
## 
## sh_cline <- deletecp(sh_cline, 1, c(550:700)) #delete 
## 
## checkhull(sh_cline, 1)$error
## 
## sh_cline <- addcp(sh_cline, 1, 560)
## 
## sh_clineUpdate <- makehull(sh_cline, 1)
##  
## sh_bd <- transformSpeclib(spectral_data_preproc,
##                           method = "sh", out = "bd")
## 
## sh_bd <- updatecl(sh_bd, sh_clineUpdate) 
##   
##   
## par (mfrow = c(1,2))
## plot(sh_cline, 1, main = "Updated Segmented hull",
##      subset = c(500,800))
## #plot new band depth
## plot(sh_bd, 1, main="Updated hull - Band depth",
##      subset = c(500,800))
##   


###################################################
### code chunk number 73: Hsdar-intro.Rnw:999-1010
###################################################
sh_bd <- transformSpeclib(spectral_data_preproc,
                          method = "sh", out = "bd")

## Define features automatically
features <- define.features(sh_bd)

##Example to isolate the features around 450,700,1200 and 1500nm.
featureSelection <- specfeat(features, c(450,700,1200,1500))

## Plot features
plot(featureSelection, 1:4)


###################################################
### code chunk number 74: Hsdar-intro.Rnw:1013-1015
###################################################
par(mar = c(4,4,0.1,0.1), cex = 0.75)
plot(featureSelection, 1:4)


###################################################
### code chunk number 75: Hsdar-intro.Rnw:1023-1028
###################################################
featuresCut <- cut_specfeat(featureSelection, fnumber = c(1,2), 
                            limits = c(c(310, 560), c(589, 800)))

## Plot result
plot(featuresCut, 1:2)


###################################################
### code chunk number 76: Hsdar-intro.Rnw:1031-1033
###################################################
par(mar = c(4,4,0.1,0.1), cex = 0.75)
plot(featuresCut, 1:2)


###################################################
### code chunk number 77: Hsdar-intro.Rnw:1134-1137
###################################################
data(spectral_data)
ndvi <- vegindex(spectral_data, "NDVI")
ndvi #see ndvi


###################################################
### code chunk number 78: Hsdar-intro.Rnw:1143-1145
###################################################
avl <- vegindex()
vi <- vegindex(spectral_data, index = avl)


###################################################
### code chunk number 79: Hsdar-intro.Rnw:1161-1163
###################################################
data(spectral_data)
rd <- rededge(spectral_data)


###################################################
### code chunk number 80: Hsdar-intro.Rnw:1168-1169 (eval = FALSE)
###################################################
## boxplot(rd$R0 ~ SI(spectral_data)$season, ylab = "R0")


###################################################
### code chunk number 81: Hsdar-intro.Rnw:1172-1174
###################################################
par(mar = c(4,4,0.1,0.1), cex = 0.75)
boxplot(rd$R0 ~ SI(spectral_data)$season, ylab = "R0")


###################################################
### code chunk number 82: Hsdar-intro.Rnw:1190-1192
###################################################
spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
                              response_function = FALSE)


###################################################
### code chunk number 83: Hsdar-intro.Rnw:1197-1198
###################################################
str(nri)


###################################################
### code chunk number 84: Hsdar-intro.Rnw:1200-1201 (eval = FALSE)
###################################################
## help(nri)


###################################################
### code chunk number 85: Hsdar-intro.Rnw:1206-1208
###################################################
nri_WV <- nri(spec_WV, recursive = TRUE)
nri_WV


###################################################
### code chunk number 86: Hsdar-intro.Rnw:1213-1214 (eval = FALSE)
###################################################
## str(nri_WV)


###################################################
### code chunk number 87: Hsdar-intro.Rnw:1219-1220
###################################################
nri_WV$nri[,,1] 


###################################################
### code chunk number 88: Hsdar-intro.Rnw:1236-1239
###################################################
spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
                              response_function = FALSE)
nri_WV <- nri(spec_WV, recursive = TRUE)


###################################################
### code chunk number 89: Hsdar-intro.Rnw:1247-1248
###################################################
chlorophyll <- SI(spec_WV)$chlorophyll


###################################################
### code chunk number 90: Hsdar-intro.Rnw:1253-1254
###################################################
cortestnri <- cor.test(nri_WV, chlorophyll)


###################################################
### code chunk number 91: Hsdar-intro.Rnw:1259-1260
###################################################
cortestnri


###################################################
### code chunk number 92: Hsdar-intro.Rnw:1266-1267 (eval = FALSE)
###################################################
## plot(cortestnri, coefficient = "p.value")


###################################################
### code chunk number 93: Hsdar-intro.Rnw:1270-1272
###################################################
par(mar = c(4,4,0.1,0.1), cex = 0.75)
plot(cortestnri, coefficient = "p.value")


###################################################
### code chunk number 94: Hsdar-intro.Rnw:1279-1280 (eval = FALSE)
###################################################
## plot(cortestnri, coefficient = "p.value", range = c(0,0.01))


###################################################
### code chunk number 95: Hsdar-intro.Rnw:1283-1285
###################################################
par(mar = c(4,4,0.1,0.1), cex = 0.75)
plot(cortestnri, coefficient = "p.value", range = c(0,0.01))


###################################################
### code chunk number 96: Hsdar-intro.Rnw:1297-1298
###################################################
str(lm.nri)


###################################################
### code chunk number 97: Hsdar-intro.Rnw:1304-1305
###################################################
lmnri <- lm.nri(nri_WV ~ chlorophyll, preddata = spec_WV)


###################################################
### code chunk number 98: Hsdar-intro.Rnw:1310-1311
###################################################
lmnri


###################################################
### code chunk number 99: Hsdar-intro.Rnw:1317-1318
###################################################
str(nri_best_performance)


###################################################
### code chunk number 100: Hsdar-intro.Rnw:1325-1327
###################################################
nribest <- nri_best_performance(lmnri, n = 1)
nribest


###################################################
### code chunk number 101: Hsdar-intro.Rnw:1333-1334
###################################################
getNRI(nri_WV, nribest)


###################################################
### code chunk number 102: Hsdar-intro.Rnw:1348-1349 (eval = FALSE)
###################################################
## plot(lmnri, coefficient = "r.squared", main = "R squared")


###################################################
### code chunk number 103: Hsdar-intro.Rnw:1352-1354
###################################################
par(mar = c(4,4,3,0.1), cex = 0.75)
plot(lmnri, coefficient = "r.squared", main = "R squared")


###################################################
### code chunk number 104: Hsdar-intro.Rnw:1360-1362 (eval = FALSE)
###################################################
## plot(lmnri, coefficient = "r.squared", main = "R squared",
##      constraint = "p.value<0.01")


###################################################
### code chunk number 105: Hsdar-intro.Rnw:1365-1368
###################################################
par(mar = c(4,4,3,0.1), cex = 0.75)
plot(lmnri, coefficient = "r.squared", main = "R squared",
     constraint = "p.value<0.01")


###################################################
### code chunk number 106: Hsdar-intro.Rnw:1380-1387
###################################################
## Use PROSAIL to generate some vegetation spectra with different LAI
parameter <- data.frame(LAI = seq(0, 1, 0.01))
spectral_data <- PROSAIL(parameterList = parameter)

## We resample the data to Quickbird channels to get the same 
## spectral ranges
spectral_data_qb <- spectralResampling(spectral_data, "Quickbird")


###################################################
### code chunk number 107: Hsdar-intro.Rnw:1392-1406 (eval = FALSE)
###################################################
## ## Get endmember spectra
## ## Retrieve all available spectra
## avl <- USGS_get_available_files()
## 
## ## Download all spectra matching "grass-fescue"
## grass_spectra <- USGS_retrieve_files(avl = avl, 
##                                      pattern = "grass-fescue")
## limestone <- USGS_retrieve_files(avl = avl, pattern = "limestone")
## 
## ## Perform resampling for the endmember spectra. Note that we only  
## ## use the first vegetation spectrum
## grass_spectra_qb <- spectralResampling(grass_spectra[1,], 
##                                        "Quickbird")
## limestone_qb <- spectralResampling(limestone, "Quickbird")


###################################################
### code chunk number 108: Hsdar-intro.Rnw:1408-1410
###################################################
grass_spectra_qb <- speclib(spectra = matrix(c(3.190627, 5.137504, 5.797486, 29.68515), nrow = 1), wavelength = c(485, 560, 660, 830))
limestone_qb <- speclib(spectra = matrix(c(16.93489, 18.97302, 21.407, 23.98981), nrow = 1), wavelength = c(485, 560, 660, 830))


###################################################
### code chunk number 109: Hsdar-intro.Rnw:1415-1422
###################################################
em <- speclib(spectra = rbind(spectra(grass_spectra_qb),
                              spectra(limestone_qb))/100,
              wavelength = wavelength(limestone_qb))

unmix_res <- unmix(spectral_data_qb, em)
## Let's have a look at the output:
str(unmix_res)


###################################################
### code chunk number 110: Hsdar-intro.Rnw:1434-1437
###################################################
plot(unmix_res$fractions[1,] ~ SI(spectral_data_qb)$LAI,
     type = "l", xlab = "LAI", 
     ylab = "Unmixed fraction of vegetation")


###################################################
### code chunk number 111: Hsdar-intro.Rnw:1440-1443
###################################################
par(mar = c(4,4,0,0.1), cex = 0.75)
plot(unmix_res$fractions[1,] ~ SI(spectral_data_qb)$LAI, type = "l",
     xlab = "LAI", ylab = "Unmixed fraction of vegetation")


