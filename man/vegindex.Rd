\name{vegindex}
\alias{vegindex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
vegindex
}
\description{
Function calculates a variety of hyperspectral vegetation indices
}
\usage{
vegindex(x, index, returnHCR = "auto", L = 0.5, 
         weighted = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Speclib}
}
  \item{index}{
Character string. Name or definition of index or vector with names/definitions of indices to calculate. See Details section for further information.
}
  \item{returnHCR}{
If TRUE, the result will be of class HyperSpecRaster, otherwise it is a data frame. If "auto", the class is automatically determined by passed Speclib.
}
\item{L}{
Factor for SAVI index. Unused for other indices.
}
\item{weighted}{
Logical indicating if reflectance values should be interpolated to fit wavelength position. If \code{FALSE} the reflectance values of nearest neighbour to passed position are returned. See \code{\link{get_reflectance}} for further explanation.
}
  \item{...}{
Further arguments passed to derivative functions. Only used for indices requiring derivations.
}
}
\details{
Index must be a charater vector containing pre-defined indices (selected by their name) or self defined indices or any combination of pre- and self-defined indices.
\subsection{Pre-defined indices}{
The following indices are available:
\tabular{lll}{
\tab\tab\cr
\strong{Name}\tab \strong{Formula} \tab \strong{Reference*}\cr
\tab\tab\cr
Boochs \tab \eqn{D_{703}}                                                                       \tab Boochs et al. (1990)\cr
Boochs2 \tab \eqn{D_{720}}                                                                      \tab Boochs et al. (1990)\cr
CAI \tab \eqn{0.5\cdot (R_{2000}+R_{2200})-R_{2100}}{0.5*(R_{2000}+R_{2200})-R_{2100}}          \tab Nagler et al. (2003)\cr
CARI \tab \eqn{a = (R_{700}-R_{550}) / 150}                                                     \tab Kim et al. (1994)\cr
     \tab \eqn{b = R_{550}-(a\cdot 550)}{b = R_{550}-(a*550)}                                   \tab \cr
     \tab \eqn{R_{700}\cdot \textnormal{abs}(a\cdot 670+R_{670}+b)/R_{670}\cdot}{R_{700}*abs(a*670+R_{670}+b)/R_{670}*} 
                                                                                                \tab \cr
     \tab \eqn{(a^2+1)^{0.5}}                                                                   \tab \cr
Carter \tab \eqn{R_{695}/R_{420}}                                                               \tab Carter (1994)\cr
Carter2 \tab \eqn{R_{695}/R_{760}}                                                              \tab Carter (1994)\cr
Carter3 \tab \eqn{R_{605}/R_{760}}                                                              \tab Carter (1994)\cr
Carter4 \tab \eqn{R_{710}/R_{760}}                                                              \tab Carter (1994)\cr
Carter5 \tab \eqn{R_{695}/R_{670}}                                                              \tab Carter (1994)\cr
Carter6 \tab \eqn{R_{550}}                                                                      \tab Carter (1994)\cr
CI \tab \eqn{R_{675}\cdot R_{690}/R_{683}^2}{R_{675}*R_{690}/R_{683}^2}                         \tab Zarco-Tejada et al. \cr
   \tab                                                                                         \tab (2003)\cr
CI2 \tab \eqn{R_{760}/R_{700}-1}                                                                \tab Gitelson et al. (2003) \cr   
ClAInt \tab \eqn{\int_{600 nm}^{735 nm} R}                                                      \tab Oppelt and Mauser\cr
       \tab                                                                                     \tab (2004)\cr
CRI1 \tab \eqn{1/R_{515}-1/R_{550}}                                                             \tab Gitelson et al. (2003)\cr
CRI2 \tab \eqn{1/R_{515}-1/R_{770}}                                                             \tab Gitelson et al. (2003)\cr
CRI3 \tab \eqn{1/R_{515}-1/R_{550}\cdot R_{770}}{1/R_{515}-1/R_{550}*R_{770}}                   \tab Gitelson et al. (2003)\cr
CRI4 \tab \eqn{1/R_{515}-1/R_{700}\cdot R_{770}}{1/R_{515}-1/R_{700}*R_{770}}                   \tab Gitelson et al. (2003)\cr
D1 \tab \eqn{D_{730}/D_{706}}                                                                   \tab Zarco-Tejada et al. \cr
   \tab                                                                                         \tab (2003)\cr
D2 \tab \eqn{D_{705}/D_{722}}                                                                   \tab Zarco-Tejada et al. \cr
   \tab                                                                                         \tab (2003)\cr
Datt \tab \eqn{(R_{850}-R_{710})/(R_{850}-R_{680})}                                             \tab  Datt (1999b) \cr
Datt2 \tab \eqn{R_{850}/R_{710}}                                                                \tab  Datt (1999b) \cr
Datt3 \tab \eqn{D_{754}/D_{704}}                                                                \tab  Datt (1999b) \cr
Datt4 \tab \eqn{R_{672}/(R_{550}\cdot R_{708})}{R_{672}/(R_{550}*R_{708})}                      \tab  Datt (1998) \cr
Datt5 \tab \eqn{R_{672}/R_{550}}                                                                \tab  Datt (1998) \cr
Datt6 \tab \eqn{(R_{860})/(R_{550}\cdot R_{708})}{(R_{860})/(R_{550}*R_{708})}                  \tab  Datt (1998) \cr
Datt7 \tab \eqn{(R_{860} - R_{2218})/(R_{860} - R_{1928})}                                      \tab  Datt (1999a) \cr
Datt8 \tab \eqn{(R_{860} - R_{1788})/(R_{860} - R_{1928})}                                      \tab  Datt (1999a) \cr
DD \tab \eqn{(R_{749}-R_{720})-(R_{701}-R_{672})}                                               \tab le Maire et al. (2004)\cr
DDn \tab \eqn{2\cdot (R_{710}-R_{660}-R_{760})}{2*(R_{710}-R_{660}-R_{760})}                    \tab le Maire et al. (2008)\cr
DPI\tab \eqn{(D_{688}\cdot D_{710})/D_{697}^2}{(D_{688}*D_{710})/D_{697}^2}                     \tab Zarco-Tejada et al. \cr
   \tab                                                                                         \tab (2003)\cr
DWSI1 \tab \eqn{R_{800}/R_{1660}}\tab Apan et al. (2004)\cr
DWSI2 \tab \eqn{R_{1660}/R_{550}}\tab Apan et al. (2004)\cr
DWSI3 \tab \eqn{R_{1660}/R_{680}}\tab Apan et al. (2004)\cr
DWSI4 \tab \eqn{R_{550}/R_{680}}\tab Apan et al. (2004)\cr
DWSI5 \tab \eqn{(R_{800} + R_{550})/(R_{1660} + R_{680})}\tab Apan et al. (2004)\cr
EGFN \tab \eqn{(\max(D_{650:750})-\max(D_{500:550}))/}                                          \tab Penuelas et al. \cr     
     \tab \eqn{(\max(D_{650:750})+\max(D_{500:550}))}                                           \tab (1994)\cr
EGFR \tab \eqn{\max(D_{650:750})/\max(D_{500:550})}                                             \tab Penuelas et al. (1994)\cr
EVI \tab \eqn{2.5\cdot ((R_{800}-R_{670})/}{2.5*((R_{800}-R_{670})/}                            \tab Huete et al. (1997)\cr    
    \tab \eqn{(R_{800}-(6\cdot R_{670})-(7.5\cdot R_{475})+1))}{(R_{800}-(6*R_{670})-(7.5*R_{475})+1))} 
                                                                                                \tab \cr
GDVI\tab \eqn{(R_{800}^n-R_{680}^n) / (R_{800}^n+R_{680}^n)}**                                  \tab Wu (2014)\cr                                                                                             
GI \tab \eqn{R_{554}/R_{677}}                                                                   \tab Smith et al. (1995)\cr
Gitelson \tab \eqn{1/R_{700}}                                                                   \tab Gitelson et al. (1999)\cr
Gitelson2 \tab \eqn{(R_{750}-R_{800}/R_{695}-R_{740})-1}                                        \tab Gitelson et al. (2003)\cr
GMI1 \tab \eqn{R_{750}/R_{550}}                                                                 \tab Gitelson et al. (2003)\cr
GMI2 \tab \eqn{R_{750}/R_{700}}                                                                 \tab Gitelson et al. (2003)\cr
Green NDVI \tab \eqn{(R_{800}-R_{550})/(R_{800}+R_{550})}                                       \tab Gitelson et al. (1996)\cr
LWVI_1 \tab \eqn{(R_{1094}-R_{983}) / (R_{1094}+R_{983})}                                       \tab Galvao et al. (2005)\cr
LWVI_2 \tab \eqn{(R_{1094}-R_{1205}) / (R_{1094}+R_{1205})}                                     \tab Galvao et al. (2005)\cr
Maccioni \tab \eqn{(R_{780}-R_{710})/(R_{780}-R_{680})}                                         \tab Maccioni et al. (2001)\cr
MCARI \tab \eqn{((R_{700}-R_{670})-0.2\cdot (R_{700}-R_{550}))\cdot}{((R_{700}-R_{670})-0.2*(R_{700}-R_{550}))*}   
                                                                                                \tab Daughtry et al. (2000)\cr
      \tab \eqn{(R_{700}/R_{670})}                                                              \tab \cr
MCARI/OSAVI \tab                                                                                \tab Daughtry et al. (2000)\cr
MCARI2 \tab \eqn{((R_{750}-R_{705})-0.2\cdot (R_{750}-R_{550}))\cdot}{((R_{750}-R_{705})-0.2*(R_{750}-R_{550}))*}    
                                                                                                \tab Wu et al. (2008)\cr     
       \tab \eqn{ (R_{750}/R_{705})}                                                            \tab \cr
MCARI2/OSAVI2\tab                                                                               \tab Wu et al. (2008)\cr
mND705 \tab \eqn{(R_{750}-R_{705})/(R_{750}+R_{705}-2\cdot R_{445})}{(R_{750}-R_{705})/(R_{750}+R_{705}-2*R_{445})}  
                                                                                                \tab Sims and Gamon (2002)\cr
mNDVI \tab \eqn{(R_{800}-R_{680})/(R_{800}+R_{680}-2\cdot R_{445})}{(R_{800}-R_{680})/(R_{800}+R_{680}-2*R_{445})}    
                                                                                                \tab Sims and Gamon (2002)\cr
MPRI \tab \eqn{(R_{515}-R_{530})/(R_{515}+R_{530})}                                             \tab Hernandez-Clemente et al.\cr
     \tab                                                                                       \tab (2011)\cr   
mREIP \tab Red-edge inflection point using Gaussain fit                                         \tab Miller et al. (1990)\cr
MSAVI \tab \eqn{0.5 \cdot  (2\cdot R_{800}+1-}{0.5*(2*R_{800}+1-}                               \tab Qi et al. (1994)\cr      
      \tab \eqn{((2\cdot R_{800}+1)^2-8\cdot (R_{800}-R_{670}))^{0.5})}{((2*R_{800}+1)^2-8*(R_{800}-R_{670}))^{0.5})}  
                                                                                                \tab \cr
MSI \tab \eqn{R_{1600}/ R_{817}}                                                                \tab Hunt and Rock (1989)\cr
mSR \tab \eqn{(R_{800}-R_{445})/(R_{680}-R_{445})}                                              \tab Sims and Gamon (2002)\cr
mSR2 \tab \eqn{(R_{750}/R_{705})-1/(R_{750}/R_{705}+1)^{0.5}}                                   \tab Chen (1996)\cr
mSR705 \tab \eqn{(R_{750}-R_{445})/(R_{705}-R_{445})}                                           \tab Sims and Gamon (2002)\cr
MTCI \tab \eqn{(R_{754}-R_{709})/(R_{709}-R_{681})}                                             \tab Dash and Curran (2004)\cr
MTVI \tab \eqn{1.2\cdot (1.2\cdot (R_{800}-R_{550})-}{1.2*(1.2*(R_{800}-R_{550})-}              \tab Haboudane et al.\cr
     \tab \eqn{2.5\cdot (R_{670}-R_{550}))}{2.5* (R_{670}-R_{550}))}                            \tab (2004)\cr
NDLI\tab \eqn{(log(1/R_{1754}) - log(1/R_{1680}))/}                                             \tab Serrano et al. (2002) \cr 
    \tab \eqn{(log(1/R_{1754}) + log(1/R_{1680}))}                                              \tab \cr
NDNI\tab \eqn{(log(1/R_{1510}) - log(1/R_{1680}))/}                                             \tab Serrano et al. (2002) \cr 
    \tab \eqn{(log(1/R_{1510}) + log(1/R_{1680}))}                                              \tab \cr
NDVI \tab \eqn{(R_{800}-R_{680}) / (R_{800}+R_{680})}                                           \tab Tucker (1979)\cr
NDVI2 \tab \eqn{(R_{750}-R_{705})/(R_{750}+R_{705})}                                            \tab Gitelson and Merzlyak  \cr
      \tab                                                                                      \tab (1994)\cr
NDVI3 \tab \eqn{(R_{682}-R_{553})/(R_{682}+R_{553})}                                            \tab Gandia et al. (2004)\cr
NDWI \tab \eqn{(R_{860}-R_{1240}) / (R_{860}+R_{1240})}                                         \tab Gao (1996)\cr
NPCI \tab \eqn{(R_{680}-R_{430})/(R_{680}+R_{430})}                                             \tab Penuelas et al. (1994)\cr
OSAVI \tab \eqn{(1+0.16) \cdot  (R_{800}-R_{670})/}{(1+0.16)*(R_{800}-R_{670})/}                \tab Rondeaux et al.\cr
      \tab \eqn{(R_{800}+R_{670}+0.16)}                                                         \tab (1996)\cr
OSAVI2 \tab \eqn{(1+0.16) \cdot  (R_{750}-R_{705})/}{(1+0.16)*(R_{750}-R_{705})/}               \tab Wu et al. (2008)\cr
       \tab \eqn{(R_{750}+R_{705}+0.16)}                                                        \tab \cr
PARS \tab \eqn{R_{746}/R_{513}}                                                                 \tab Chappelle et al. (1992)\cr
PRI \tab \eqn{(R_{531}-R_{570})/(R_{531}+R_{570})}                                              \tab Gamon et al. (1992)\cr
PRI_norm \tab \eqn{\textnormal{PRI} \cdot (-1)/(\textnormal{RDVI}\cdot R_{700}/R_{670})}{PRI*(-1)/(RDVI*R_{700}/R_{670})}
                                                                                                \tab Zarco-Tejada et al. \cr
    \tab                                                                                        \tab (2013)\cr
PRI*CI2 \tab \eqn{\textnormal{PRI} \cdot \textnormal{CI}2}{PRI*CI2}                             \tab Garrity et al. (2011)\cr
PSRI \tab \eqn{(R_{678}-R_{500}/R_{750}}                                                        \tab Merzlyak et al. (1999)\cr
PSSR \tab \eqn{R_{800}/R_{635}}                                                                 \tab Blackburn (1998)\cr
PSND \tab \eqn{(R_{800}-R_{470})/(R_{800}-R_{470})}                                             \tab Blackburn (1998)\cr
PWI \tab \eqn{R_{900}/R_{970}}                                                                  \tab Penuelas et al. (1997)\cr
RDVI \tab \eqn{(R_{800}-R_{670})/\sqrt{R_{800}+R_{670}}}                                        \tab Roujean and Breon (1995)\cr
% RDVI \tab \eqn{(R_{800}-R_{670})/(R_{800}+R_{670})^{0.5}}                                     \tab Roujean and Breon (1995)\cr
REP_LE \tab Red-edge position through linear extrapolation.                                     \tab Cho and Skidmore (2006)\cr
REP_Li \tab \eqn{R_{re}=(R_{670}+R_{780})/2}                                                    \tab Guyot and Baret (1988)\cr
       \tab \eqn{700 + 40\cdot((R_{re} -R_{700})/(R_{740}-R_{700}))}{700 + 40*((R_{re} -R_{700})/(R_{740}-R_{700}))}\tab \cr
% REP_Li \tab \eqn{700 + 40((((R_{670}+R_{780})/2)-R_{700})/}                                     \tab Guyot and Baret (1988)\cr
%        \tab \eqn{(R_{740}-R_{700}))}                                                            \tab \cr
SAVI \tab \eqn{(1+L)\cdot (R_{800}-R_{670})/(R_{800}+R_{670}+L)}{(1+L)*(R_{800}-R_{670})/(R_{800}+R_{670}+L)} 
                                                                                                \tab Huete (1988)\cr
SIPI \tab \eqn{(R_{800}-R_{445})/(R_{800}-R_{680})}                                             \tab Penuelas et al. (1995),\cr
     \tab                                                                                       \tab Penuelas et al. (1995a) \cr
SPVI \tab \eqn{0.4\cdot 3.7\cdot (R_{800}-R_{670})-1.2\cdot }{0.4*3.7*(R_{800}-R_{670})-1.2*}   \tab Vincini et al. (2006)\cr  
     \tab \eqn{((R_{530}-R_{670})^2)^{0.5}}                                                     \tab \cr
SR \tab \eqn{R_{800}/R_{680}}                                                                   \tab Jordan (1969)\cr
SR1 \tab \eqn{R_{750}/R_{700}}                                                                  \tab Gitelson and Merzlyak  \cr
    \tab                                                                                        \tab (1997)\cr
SR2 \tab \eqn{R_{752}/R_{690}}                                                                  \tab Gitelson and Merzlyak  \cr
    \tab                                                                                        \tab (1997)\cr
SR3 \tab \eqn{R_{750}/R_{550}}                                                                  \tab Gitelson and Merzlyak  \cr
    \tab                                                                                        \tab (1997)\cr
SR4 \tab \eqn{R_{700}/R_{670}}                                                                  \tab McMurtey et al. (1994)\cr
SR5 \tab \eqn{R_{675}/R_{700}}                                                                  \tab Chappelle et al. (1992)\cr
SR6 \tab \eqn{R_{750}/R_{710}}                                                                  \tab Zarco-Tejada and Miller  \cr
    \tab                                                                                        \tab (1999)\cr
SR7 \tab \eqn{R_{440}/R_{690}}                                                                  \tab Lichtenthaler et al. (1996)\cr
SR8 \tab \eqn{R_{515}/R_{550}}                                                                  \tab Hernandez-Clemente et al. \cr
    \tab                                                                                        \tab (2012)\cr
SRPI \tab \eqn{R_{430}/R_{680}}                                                                 \tab Penuelas et al. (1995)\cr
SRWI \tab \eqn{R_{850}/R_{1240}}                                                                \tab Zarco-Tejada et al. \cr
    \tab                                                                                        \tab (2003)\cr
Sum_Dr1 \tab \eqn{\sum_{i=626}^{795} D1_i}                                                      \tab Elvidge and Chen (1995)\cr
Sum_Dr2 \tab \eqn{\sum_{i=680}^{780} D1_i}                                                      \tab Filella and Penuelas  \cr
    \tab                                                                                        \tab (1994)\cr
SWIR FI\tab \eqn{R_{2133}^2/(R_{2225} \cdot R_{2209}^3}                                         \tab Levin et al. (2007)\cr  
SWIR LI\tab \eqn{3.87  \cdot (R_{2210} - R_{2090}) - }                                          \tab Lobell et al. (2001)\cr
       \tab \eqn{27.51 \cdot (R_{2280} - R_{2090}) - 0.2}                                       \tab \cr  
SWIR SI\tab \eqn{-41.59 \cdot (R_{2210} - R_{2090}) + }                                          \tab Lobell et al. (2001)\cr
       \tab \eqn{1.24 \cdot (R_{2280} - R_{2090}) + 0.64}                                       \tab \cr 
SWIR VI\tab \eqn{37.72  \cdot (R_{2210} - R_{2090}) + }                                          \tab Lobell et al. (2001)\cr
       \tab \eqn{26.27 \cdot (R_{2280} - R_{2090}) + 0.57}                                       \tab \cr        
TCARI \tab \eqn{3\cdot ((R_{700}-R_{670})-0.2\cdot (R_{700}-R_{550})\cdot}{3*((R_{700}-R_{670})-0.2*R_{700}-R_{550})*} 
                                                                                                \tab Haboudane et al. (2002)\cr  
      \tab \eqn{(R_{700}/R_{670}))}                                                             \tab \cr
TCARI/OSAVI \tab  \eqn{\textnormal{TCARI} / \textnormal{OSAVI}}{TCARI/OSAVI}                    \tab Haboudane et al. (2002)\cr
TCARI2 \tab \eqn{3\cdot ((R_{750}-R_{705})-0.2\cdot (R_{750}-R_{550})\cdot}{3*((R_{750}-R_{705})-0.2*(R_{750}-R_{550})*}
                                                                                                \tab Wu et al. (2008)\cr  
       \tab \eqn{(R_{750}/R_{705}))}                                                            \tab \cr
TCARI2/OSAVI2 \tab \eqn{\textnormal{TCARI}2 / \textnormal{OSAVI}2}{TCARI2/OSAVI2}               \tab Wu et al. (2008)\cr
TGI\tab \eqn{-0.5 (190 (R_{670} - R_{550} ) - 120 (R_{670} - R_{480} ))}                        \tab Hunt et al. (2013)\cr
TVI \tab \eqn{0.5\cdot (120\cdot (R_{750}-R_{550})-}{0.5*(120*(R_{750}-R_{550})-}               \tab Broge and Leblanc \cr
    \tab \eqn{200\cdot (R_{670}-R_{550}))}{200*(R_{670}-R_{550}))}                              \tab (2000)\cr
Vogelmann \tab \eqn{R_{740}/R_{720}}                                                            \tab Vogelmann et al. (1993)\cr
Vogelmann2 \tab \eqn{(R_{734}-R_{747})/(R_{715}+R_{726})}                                       \tab Vogelmann et al. (1993)\cr
Vogelmann3 \tab \eqn{D_{715}/D_{705}}                                                           \tab Vogelmann et al. (1993)\cr
Vogelmann4 \tab \eqn{(R_{734}-R_{747})/(R_{715}+R_{720})}                                       \tab Vogelmann et al. (1993)\cr
% CI_Hanna \tab \eqn{R_{760}/R_{695}}\tab \cr
% REIP \tab \eqn{NULL}                                                                          \tab Collins (1978), Horler et al. (1983)\cr
% TCARI \tab \eqn{3\cdot ((R_{700}-R_{670})-0.2\cdot (R_{700}-R_{550})\cdot (R_{700}/R_{670}))} \tab \cr
% TVI \tab \eqn{0.5\cdot (120\cdot (R_{750}-R_{550})-200\cdot (R_{670}-R_{550}))}               \tab \cr
% WBI \tab \eqn{R_{970}/ R_{900}}                                                               \tab \cr

}

* For references please type: \code{hsdardocs("References.pdf")}.\cr
** For GDVI n must be defined appending an underscore and the intended exponent to the index name. E.g., for n = 2, the correct index name would be "GDVI_2". Note that GDVI-indices with n = {2, 3, 4} will be derived if all available indices are calculated. 
}
\subsection{Self-defining indices}{  
Self-defined indices may be passed using the following syntax:
\itemize{
  \item{Rxxx: }{Reflectance at wavelength 'xxx'. Note that R must be upper case.}
  \item{Dxxx: }{First derivation of reflectance values at wavelength 'xxx'. Note that D must be upper case.}
}
Using this syntax, complex indices can be easily defined. Note that the entire definition of the index must be passed as one character string. Consequently, the NDVI would be written as\cr
"(R800-R680)/(R800+R680)".
}
}
\value{
A vector containing indices values. If index is a vector with length > 1, a data frame with ncol = length(index) and nrow = number of spectra in x is returned.

If function is called without any arguments, return value will be a vector containing all available indices in alphabetical order.
}
\references{
See \code{hsdardocs("References.pdf")}
}
\author{
Hanna Meyer and Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{soilindex}}, \code{\link{derivative.speclib}}, \code{\link{rededge}}, \code{\link{get_reflectance}}
}
\examples{
## Example calculating NDVI
data(spectral_data)
ndvi <- vegindex(spectral_data, "NDVI")


## Example calculating all available indices
## Get available indices
avl <- vegindex()
vi <- vegindex(spectral_data, avl)

## Self-defined indices
## NDVI
vi <- vegindex(spectral_data, "(R800-R680)/(R800+R680)")
## NDNI
vi <- vegindex(spectral_data, 
               "(log(1/R1510) - log(1/R1680))/(log(1/R1510) + log(1/R1680))")
## D1
vi <- vegindex(spectral_data, "D730/D706")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
