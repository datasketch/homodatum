
#' @export
possibleCtypes <- function(ctypes, castable = FALSE, combine = FALSE){
  #ctypes <- c("Cat","Cat","Num","Cat")
  if(castable & !combine){
    return(castable_list(ctypes))
  }
  if(combine){
    comb <- powerSet(ctypes)
    if(!castable){
      return(comb)
    }else{
      l <- map(comb, castable_list)
      names(l) <- map(comb,paste,collapse = "-")
      return(l)
    }
  }
  ctypes
}


#' @export
possibleNamedCtypes <- function(namedCtypes, permute = TRUE, castable = FALSE){
  subdata <- powerSet(namedCtypes)
  if(!permute)
    permuteCtypes(subdata[[1]])
  l <- map(subdata, permuteCtypes)
  l <- unlist(l, recursive = FALSE) %>% unname()
  if(castable){
    l <- map(l, possibleCtypes, castable = TRUE)
  }
  names(l) <- map(l,function(x) paste0(names(x),collapse="|"))
  l
}

#' @export
possibleNamedCtypesStr <- function(namedCtypes, permute = TRUE, castable = FALSE){
  l <- possibleNamedCtypes(ctypes, permute = TRUE, castable = castable)
  if(!castable){
    ctypesStr <- map(l, paste, collapse = "-")
  }else{
    ctypesStr <- map(l, function(s) map_chr(transpose(s),paste, collapse = "-"))
  }
  ctypesStr
}


#' @export
whichSubdata <- function(data, outCtypes, castable = FALSE){
  ctypes <- guessCtypes(data, named = TRUE)
  if(castable && (ncol(data) > 5))
    stop("Too many columns for castable combinations")
  ctypesStr <- possibleNamedCtypesStr(ctypes, permute = TRUE, castable = castable)
  outCtypesStr <- paste(outCtypes, collapse = "-")
  #if(is.null(outCtypes)){
  #  cnames <- names(ctypesStr)
  #}else{
    cnames <- ctypesStr %>% keep(function(s){outCtypesStr %in% s}) %>% names
  #}
  strsplit(cnames,"|", fixed = TRUE)
}

# #' @export
# whichSubCtypes <- function(data, as_string = FALSE){
#   l <- possibleSubdata(data, permute = TRUE) # TODO ctype casts
#   l <- l %>% map("ctype") %>% map(unname) %>% unique()
#   if(as_string) return(map(l, paste, collapse = "-"))
#   l
# }

#' @export
permuteCtypes <- function(ctypes, nms = NULL){
  nms <- nms %||% names(ctypes)
  if(is.null(nms))
    stop("ctypes must have names")
  y <- permuteVector(nms) %>% t() %>% as_tibble() %>% as.list()
  x <- permuteVector(ctypes) %>% t() %>% as_tibble() %>% as.list()
  map2(x,y, ~ set_names(.x, .y))
}



