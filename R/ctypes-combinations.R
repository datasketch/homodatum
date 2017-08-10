
#' @export
possibleCtypes <- function(ctypes, castable = FALSE){
  #ctypes <- c("Cat1","Cat2","Num3","Cat4")
  comb <- powerSet(ctypes)
  if(!castable) return(comb)
  map(comb, castable_list)
}


#' @export
possibleSubdata <- function(data,ctypes = NULL, permute = TRUE, castable = FALSE){
  comb <- powerSet(names(data))
  ctypes <- ctypes %||% guessCtypes(data)
  ctypes <- ctypes %>% set_names(names(data))
  subdata <- map(comb,function(c) ctypes[c])
  #if(!castable) return(subdata)
  #map(subdata, castable_list)
  if(!permute)
    return(subdata)
  #map(subdata, permuteVector)
  l <- map(subdata, permuteCtypes)
  l <- unlist(l, recursive = FALSE) %>% unname()
  map(l, function(x){
    list(
      ctype = x,
      ctypeStr = paste(x, collapse = "-"),
      #ftype = ctypesToFtype(x),
      ncol = length(x)
    )
  })
}

#' @export
whichSubdata <- function(data, cStr = NULL){
  l <- possibleSubdata(data, permute = TRUE) # TODO ctype casts
  if(is.null(cStr)) return(l)
  l %>% keep(~.$ctypeStr %in% cStr) %>% map("ctype")
}

#' @export
whichSubCtypes <- function(data, as_string = FALSE){
  l <- possibleSubdata(data, permute = TRUE) # TODO ctype casts
  l <- l %>% map("ctype") %>% map(unname) %>% unique()
  if(as_string) return(map(l, paste, collapse = "-"))
  l
}

#' @export
permuteCtypes <- function(ctypes, nms = NULL){
  nms <- nms %||% names(ctypes)
  y <- permuteVector(nms) %>% t() %>% as_tibble() %>% as.list()
  x <- permuteVector(ctypes) %>% t() %>% as_tibble() %>% as.list()
  map2(x,y, ~ set_names(.x, .y))
}



