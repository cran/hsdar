\name{caret::featurePlot-methods}
\docType{methods}
\alias{featurePlot-methods}
\alias{featurePlot,ANY-method}
\alias{featurePlot,.CaretHyperspectral-method}
\title{Methods for Function \code{featurePlot}}
\description{
Methods for function \code{featurePlot} in package \pkg{caret}
}
\section{Methods}{
\describe{

\item{\code{signature(x = ".CaretHyperspectral")}}{
  Wrapper method for \code{\link{featurePlot}}.
  \cr
  Note that \code{".CaretHyperspectral"} is a class union containing classes
  \Sexpr[echo=TRUE,results=rd]{
    paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ")
  }.
}
}}
\keyword{methods}
