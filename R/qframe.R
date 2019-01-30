#'
#' Creates a qframe.
#'
#' @param ... these arguments are passed to \code{\link[base]{data.frame}}
#'
#' @export
qframe <- function(...) {
  
  df <- data.frame(...)
  
  as.qframe(df)
}

#'
#' Creates a \code{\link{qframe}} from a \code{\link[base]{data.frame}}.
#'
#' @param df the \code{\link[base]{data.frame}} to convert
#' 
#' @export
as.qframe <- function(df) {
  
  structure(.Data = df, class = c("qframe", class(df)))
  
}

do_binary <- function(string, queries) {
  
  Reduce(f = function(all, query){
    
    all | stringr::str_detect(string = string, pattern = query)
    
  }, x = queries, init = rep(x = FALSE, times = length(string)))
  
}

#'
#' Extract values from a \code{\link{qframe}}.
#'
#' @param x the qframe to extract data from
#' @param i elements to extract or replace, if a character vector, uses \code{\link[stringr]{str_detect}} to collect the elements
#' @param j elements to extract or replace, if a character vector, uses \code{\link[stringr]{str_detect}} to collect the elements 
#' @param drop see \code{\link[base]{data.frame}}
#' 
#' @export
`[.qframe` <- function(x, i, j, drop) {

  mdrop <- missing(drop) #see `[.data.frame`
  Narg <- nargs() - !mdrop #see `[.data.frame`
  has.j <- !missing(j) #see `[.data.frame`
  
  if (Narg < 3L) { #see `[.data.frame`
    if(!missing(i)) {
      if(all(is.character(i))) {
        
        i <- do_binary(string = colnames(x), queries = i)
        
      }
      
    }
    
  } else {
    
    if(!missing(i)) {
      
      if(all(is.character(i))) {
        
        i <- do_binary(string = rownames(x), queries = i)
        
      }
      
    }
    
    if(!missing(j)) {
      
      if(all(is.character(j))) {
        
        j <- do_binary(string = colnames(x), queries = j)
        
      }
      
    }
    
  }
  
  NextMethod("[")
  
}

#'
#' Replace values from a \code{\link{qframe}}.
#'
#' @param x the qframe to extract data from
#' @param i elements to extract or replace, if a character vector, uses \code{\link[stringr]{str_detect}} to collect the elements, see \code{\link[base]{data.frame}}
#' @param j elements to extract or replace, if a character vector, uses \code{\link[stringr]{str_detect}} to collect the elements, see \code{\link[base]{data.frame}}
#' @param value a suitable replacement value, see \code{\link[base]{data.frame}} 
#' 
#' @export
`[<-.qframe` <- function(x, i, j, value) {

  if(nargs() == 3) {
    
    if(!missing(i)) {

      if(all(is.character(i))) {
        
        i <- do_binary(string = colnames(x), queries = i)
        
      }
      
    }
    
  } else if(nargs() == 4) {
    
    if(!missing(i)) {
      
      if(all(is.character(i))) {
        
        i <- do_binary(string = rownames(x), queries = i)
        
      }
      
    }
    
    if(!missing(j)) {
      
      if(all(is.character(j))) {
        
        j <- do_binary(string = colnames(x), queries = j)
        
      }
      
    }
    
  }
 
  NextMethod("[<-")
}
