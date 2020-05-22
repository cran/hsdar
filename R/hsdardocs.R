hsdardocs <- function(doc)
{
if (doc == "References.pdf")
{
  doc <- file.path(system.file(package = "hsdar"), "doc", doc) 
  if (.Platform$OS.type == "windows")
  {
    shell.exec(doc)
  } else {
    system(paste(getOption("pdfviewer"), doc, "&"))
  }
}
if (toupper(doc) == "COPYRIGHT")
{
  doc <- file.path(system.file(package = "hsdar"), "COPYRIGHTS") 
  if (.Platform$OS.type == "windows")
  {
    file.show(doc)
  } else {
    system(paste(getOption("pager"), doc, "&"))
  }
}
if (doc == "Hsdar-intro.pdf")
{
  doc <- file.path(system.file(package = "hsdar"), "doc", doc) 
  if (.Platform$OS.type == "windows")
  {
    shell.exec(doc)
  } else {
    system(paste(getOption("pdfviewer"), doc, "&"))
  }
}
}


.applyInHelp <- function(fun_name, usage)
{
  if (usage)
  {
    if ("simplify" %in% names(formals(base::apply))) 
    {
      return(paste0("\\usage{\n",
                    "\\S4method{apply}{", fun_name, "}(X, MARGIN, FUN, ..., simplify = TRUE)\n",
                    "}"))
      return(", simplify = TRUE)")
    } else {
      return(paste0("\\usage{\n",
                    "\\S4method{apply}{", fun_name, "}(X, MARGIN, FUN, ...)\n",
                    "}"))
    }
  } else {
    if ("simplify" %in% names(formals(base::apply))) 
    {
      return("}\n\\item{simplify}{Currently ignored")
    } else {
      return("")
    }
  }
}




# .applyInHelp <- function(fun_name, usage)
# {
#   if (usage)
#   {
#     if ("simplify" %in% names(formals(base::apply))) 
#     {
#       return(paste0("\\S4method{apply}{", fun_name, "}(X, MARGIN, FUN, ..., simplify = TRUE)"))
#     } else {
#       return(paste0("\\S4method{apply}{", fun_name, "}(X, MARGIN, FUN, ...)"))
#     }
#   } else {
#     return("\\item{simplify}{Currently ignored}")
#   }
# }
