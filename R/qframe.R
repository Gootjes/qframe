#' @export
qframe <- function(...) {
  
  df <- data.frame(...)
  
  as.qframe(df)
}

#' @export
as.qframe <- function(df) {
  
  structure(.Data = df, class = c("qframe", class(df)))
  
}

do_binary <- function(string, queries) {
  
  Reduce(f = function(all, query){
    
    all | stringr::str_detect(string = string, pattern = query)
    
  }, x = queries, init = rep(x = FALSE, times = length(string)))
  
}

#' @export
`[.qframe` <- function(x, i, j, drop) {

  mdrop <- missing(drop)
  Narg <- nargs() - !mdrop
  has.j <- !missing(j)
  
  if (Narg < 3L) {
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
