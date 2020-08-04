\name{get.sensor.characteristics}
\alias{get.sensor.characteristics}
\alias{list.available.sensors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sensor characteristics
}
\description{
Get channel wavelength of implemented (multispectral) satellite sensors. 
}
\usage{
get.sensor.characteristics(sensor, response_function = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sensor}{
Character or integer. Name or numerical abbreviation of sensor. See 'sensor="help"' or 'sensor=0' for an overview of available sensors.
}
  \item{response_function}{
If TRUE, the spectral response function is returned. 
}
}
\details{
The following sensors are currently implemented: \Sexpr[echo=TRUE,results=text]{
paste(row.names(hsdar:::get.sensor.characteristics(0)), collapse = ", ")
}.

Spectral response functions are available for the following ones: \Sexpr[echo=TRUE,results=text]{
paste(row.names(hsdar:::get.sensor.characteristics(0))[hsdar:::get.sensor.characteristics(0)[,2]], collapse = ", ")
}.
}

\author{
Lukas Lehnert
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{spectralResampling}}
}
\examples{
## Return implemented sensors
get.sensor.characteristics(0)

## Sentinel 2A
data_s2a <- get.sensor.characteristics("Sentinel2a", TRUE)

## Plot response functions
plot(c(0,1)~c(attr(data_s2a$response, "minwl"),
              attr(data_s2a$response, "maxwl")),
     type = "n", xlab = "Wavelength [nm]", 
     ylab = "Spectral response")
xwl_response <- seq.int(attr(data_s2a$response, "minwl"),
                        attr(data_s2a$response, "maxwl"),
                        attr(data_s2a$response, "stepsize"))
for (i in 1:nrow(data_s2a$characteristics))
  lines(xwl_response, data_s2a$response[,i], col = i)
  
  
  
## Sentinel 2B
data_s2b <- get.sensor.characteristics("Sentinel2b", TRUE)

## Add response functions
for (i in 1:nrow(data_s2b$characteristics))
  lines(xwl_response, data_s2b$response[,i], col = i, lty = "dashed")  
legend("topright", legend = c("Sentinel2a", "Sentinel2b"), 
       lty = c("solid", "dashed"))
  
  
  

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
