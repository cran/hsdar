setMethod("$", signature(x = "Nri"),
          function(x, name)
  {
    slot(x, name)
  }
)

setMethod("wavelength", signature(object = "Nri"),
          function(object)
{
  return(object@wavelength)
}
)

setMethod("show", signature(object = "Nri"),
          function(object)
{
  x <- object
  cat(paste("Data: nri, dimension: ", dim(x$nri)[1], ", ",
            dim(x$nri)[2], ", ", dim(x$nri)[3], "\n", sep=""))
  print(x$nri)
  cat(paste("      wavelength of length =",
            length(wavelength(x)),"\n"))
  cat(paste("      fwhm",
            if (length(x$fwhm)==1) "is constant for all wavelength"
              else "for each wavelength","\n"))
  if (length(x@multivariate) > 0)
  {
    .print.glmnri(x@multivariate)
  }
  invisible(dim(x$nri))
}
)


setMethod("as.matrix", signature(x = "Nri"),
          function(x, ..., named_matrix = TRUE)
{
  mat <- matrix(x$nri, nrow = dim(x$nri)[3], byrow = TRUE, ...)
  if (named_matrix)
  {
    bnd_nam_ch <- eval(parse(text = as.character(dimnames(x$nri)[1])))
    bnd_nam <- as.vector(vapply(bnd_nam_ch, function(b1, b2) {
        paste(b2, "_", b1, sep = "")
      }, character(length = length(bnd_nam_ch)), bnd_nam_ch))
    colnames(mat) <- bnd_nam
    rownames(mat) <- eval(parse(text = as.character(dimnames(x$nri)[3])))
  }
  bnd_idx <- data.frame(b1 = rep.int(c(1:dim(x$nri)[1]), dim(x$nri)[1]),
                        b2 = c(sapply(c(1:dim(x$nri)[1]),
                                      function(x,n) rep.int(x,n), dim(x$nri)[1]))
                       )
  return(mat[, bnd_idx[,1] < bnd_idx[,2]])
}
)