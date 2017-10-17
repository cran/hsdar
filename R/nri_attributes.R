setMethod("SI", signature(object = "Nri", i = "missing", j = "missing"), 
          function(object, i, j)
  return(.SI(object@SI))
)

setMethod("SI", signature(object = "Nri", i = "ANY", j = "missing"), 
          function(object, i, j)
  return(object@SI[i,])
)

setMethod("SI", signature(object = "Nri", i = "missing", j = "ANY"), 
          function(object, i, j)
  return(object@SI[,j])
)

setMethod("SI", signature(object = "Nri", i = "ANY", j = "ANY"), 
          function(object, i, j)
  return(object@SI[i,j])
)


setReplaceMethod("SI", signature(object = "Nri", value = "matrix"), 
                 function(object, value)
{
  object@SI <- new(".SI", value)
  return(object)
}
)

setReplaceMethod("SI", signature(object = "Nri", value = "data.frame"),
                 function(object, value)
{
  object@SI <- new(".SI", value)
  return(object)
}
)

setReplaceMethod("SI", signature(object = "Nri", value = "ANY"),
                 function(object, value)
{
  object@SI <- new(".SI", value)
  return(object)
}
)



