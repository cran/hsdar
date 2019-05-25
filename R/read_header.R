.pass_header_line <- function(line_str, in_list, tag_gobal)
{
  if (!in_list)
    line_str <- strsplit(line_str, "=")[[1]]
  is_var <- length(line_str) > 1
  if (is_var)
  {
    i_var <- 2
    tag <- trimws(line_str[1])
    if (gsub(" ", "", tag) == "correctionfactors")
      tag <- "correction_factors"
    c_func <- switch(gsub(" ", "", tag),
                     mapinfo = "c(\"",
                     description = "c(\"",
                     wavelength = "c(",
                     fwhm = "c(",
                     correction_factors = "c(",
                     bbl = "c(",
                     "")
    tag_gobal <- tag
  } else {
    i_var <- 1
    tag <- NULL
    c_func <- NULL
  }
  if (length(line_str) >  i_var)
  {
    for (k in c((i_var+1):length(line_str)))
      line_str[i_var] <- paste(line_str[i_var], line_str[k], sep = "=")
  }
  value <- gsub(" ", "", line_str[i_var])
  is_list <- length(grep("{", value, fixed = TRUE)) > 0
  value <- gsub("{", "", value, fixed = TRUE)
  is_list_end <- length(grep("}", value, fixed = TRUE)) > 0
  value <- gsub("}", "", value, fixed = TRUE)
  if (all(c(is_list_end, is_list)))
  {
    is_list_end <- TRUE
    is_list <- FALSE
  }
  if (!is.null(c_func))
    value <- paste0(c_func, if (nchar(value) > 0) value)
    
  c_end_func <- switch(gsub(" ", "", tag_gobal),
                       mapinfo = "\"",
                       description = "\"",
                       "") 
  return(list(tag = tag, value = value, is_var = is_var, is_list_start = is_list,
              is_list_end = is_list_end, c_end_func = c_end_func))
}
  
.convert_header_entries <- function(x)
{
  code <- try(eval(parse(text = x)), silent = TRUE)
  if (!inherits(code, "try-error"))
  {
    if (is.list(code))
    {  
      code <- lapply(code, .convert_header_entries)
    } else {
      code_num <- try(as.numeric(code), silent = TRUE)
      if (!inherits(code_num, "try-error"))
      {
        if (!any(is.na(code_num)))
          code <- code_num 
      }
    }
  } else {
    code <- x
  }
  wa <- warnings()
  return(code)
}

read_header <- function(file, ...)
{
  ow <- options("warn")
  options(warn = -1)
  con <- file(file, "r")
  content <- readLines(con)
  close(con)
  header_info <- list()
  i_content <- 0
  in_list <- FALSE
  tag_gobal <- ""
  for (i in 1:length(content))
  {
    line_str <- .pass_header_line(content[i], in_list, tag_gobal)
    
    if (in_list)
    {
      if (line_str$is_var)
      {
        header_info[[length(header_info)]] <- paste0(header_info[[length(header_info)]], if (!is.null(line_str$tag)) paste(line_str$tag, "="), line_str$value)
      } else {
        header_info[[length(header_info)]] <- paste0(header_info[[length(header_info)]], line_str$value)
      }
    } else {
      if (line_str$is_list_start)
      {
        in_list <- TRUE
        i_content <- i_content + 1
        header_info$xxxx <- line_str$value 
        names(header_info)[length(header_info)] <- line_str$tag
        tag_gobal <- line_str$tag
      } else {
        i_content <- i_content + 1
        if (line_str$value != "ENVI")
        {
          header_info$xxxx <- paste0(line_str$value)
          names(header_info)[length(header_info)] <- line_str$tag
        }
      }
    }
    if (line_str$is_list_end)
    {
      in_list <- FALSE
      header_info[[length(header_info)]] <- paste0(header_info[[length(header_info)]], line_str$c_end_func,  ")")
      tag_gobal <- ""
    }          
  }
  header_info <- lapply(header_info, .convert_header_entries)
  options(ow)      
  return(header_info)
}


