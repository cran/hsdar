\name{caret::train-methods}
\docType{methods}
\alias{train-methods}
\alias{train,ANY-method}
\alias{train,.CaretHyperspectral-method}
% \alias{train.formula,formula,Speclib-method}
\title{Methods for Function \code{train}}
\description{
Methods for functions \code{train} and \code{train.formula} in package \pkg{caret}
}
\section{Methods}{
\describe{
  
\item{\code{signature(x = ".CaretHyperspectral")}}{
  Wrapper method for \code{\link{train}}.
  \cr
  Note that \code{".CaretHyperspectral"} is a class union containing classes
  \Sexpr[echo=TRUE,results=rd]{
    paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ")
  }.
}
\item{\code{signature(form = "formula", data = "Speclib")}}{
  Wrapper method for \code{\link{train.formula}} to be used with objects of
  class \code{Speclib}.
}
}}
\keyword{methods}
