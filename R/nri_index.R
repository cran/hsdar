setMethod("[", "Nri",
          function(x, i, ...)
{  
  x@nri <- distMat3D(x@nri[,,i], lower_tri = TRUE)
  if (ncol(SI(x)) == 1)
  {
    nam <- names(SI(x))
    tmp <- data.frame(XXX = SI(x, i = i))
    names(tmp) <- nam
    SI(x) <- tmp
  } else { 
    SI(x) <- SI(x, i = i)
  }
  return(x)  
})
