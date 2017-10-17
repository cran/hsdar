\name{predictHyperspec}
\alias{predictHyperspec}
\alias{predictHyperspec,train,.CaretHyperspectral,function-method}
\alias{predictHyperspec,train,.CaretHyperspectral,missing-method}

\title{
Prediction based on train-object and Speclib
}
\description{
Perform predictions based on a train-object from the \pkg{caret}-package and a hyperspectral dataset from \pkg{hsdar}. See help file to function \code{\link[caret]{predict.train}} of the \pkg{caret}-package for general information on prediction with \pkg{caret}.
}
\usage{
\S4method{predictHyperspec}{train,.CaretHyperspectral,missing}(object, newdata, preProcess, ...)  
  
\S4method{predictHyperspec}{train,.CaretHyperspectral,function}(object, newdata, preProcess, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{train} from \code{caret}-package
}
  \item{newdata}{
Object of class Speclib or Nri to predict on.
}
  \item{preProcess}{
Optional function to be applied on newdata prior to the prediction.
}
\item{...}{
Further arguments passed to original train function and/or to the preProcess-function.
}
}

\value{
Depending on the settings either a vector of predictions if \code{type = "raw"} or a data frame of class probabilities for \code{type = "prob"}. In the latter case, there are columns for each class. For \code{predict.list}, a list results. Each element is produced by \code{predict.train}. See details in \code{\link{predict.train}} in the \code{caret}-package.
}

\author{
Lukas Lehnert
}

\seealso{
\code{\link{predict.train}}, \code{\linkS4class{Speclib}}
}
\examples{
\dontrun{
## The following example is taken from the journal paper 
## "Hyperspectral Data Analysis in R: the hsdar Package"
## under review at the "Journal of Statistical Software"
  
data(spectral_data)

spectral_data <- smoothSpeclib(spectral_data, method = "sgolay", p = 15)

## Convert the chlorophyll measurements stored in the SI dataframe 
## from SPAD-values into mg.
SI(spectral_data)$chlorophyll <- 
  (117.1 * SI(spectral_data)$chlorophyll) /
  (148.84 - SI(spectral_data)$chlorophyll)
  
## Mask spectra  
spectral_data <- spectral_data[, wavelength(spectral_data) >= 310 & 
  wavelength(spectral_data) <= 1000]

## Transform reflectance values into band depth applying a segmented upper hull
## continuum removal.  
spec_bd <- transformSpeclib(spectral_data, method = "sh", out = "bd")

## Search for fix points in the hull and define the parts of the spectra between 
## two fix points as absorption "features"
features <- define.features(spec_bd)

## Select the chlorophyll absorption features at 460 nm and 670 nm for further
## processing
featureSpace <- specfeat(features, c(460, 670))

## Calculate all parameters from both selected features such as area, distance
## to Gauss curve etc.
featureSpace <- feature_properties(featureSpace)

## Set response and additional predictor variables for random forest model
featureSpace <- setResponse(featureSpace, "chlorophyll")
featureSpace <- setPredictor(featureSpace,
  names(SI(featureSpace))[4:ncol(SI(featureSpace))])

## Define training and cross validation for random forest model tuning
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

## Partition data set for training and validation
training_validation <- createDataPartition(featureSpace)

## Train random forest model based on training-subset
rfe_trained <- train(featureSpace[training_validation$Resample1,], 
                     trainControl = ctrl, method = "rf")  

## Predict on the validation data set
pred <- predictHyperspec(rfe_trained, featureSpace[-training_validation$Resample1,])

## Plot result for visual interpretation
lim <- range(c(SI(featureSpace,i = -training_validation$Resample1)$chlorophyll,
               pred))
plot(SI(featureSpace,i = -training_validation$Resample1)$chlorophyll, pred,
     ylab = "Predicted chlorophyll content",
     xlab = "Estimated chlorophyll content", 
     xlim = lim, ylim = lim)
lines(par()$usr[c(1,2)],par()$usr[c(3,4)], lty = "dotted")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

