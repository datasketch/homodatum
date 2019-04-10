

# Constructor function for the class
ftype <- function(x, ...)  {
  args <- list(...)
  if(!"character" %in% class(x)) stop("x must be a character vector of ftypes or ctypes names")
  if(!all(x %in% availableCtypes())) stop("All ctypes must be valid ctypes, check availableCtypes()")
  if(length(x) > 1){
    # Check all elements of x are ctypes
    ftype <- sort(strsplit(ftype,"-")[[1]])
  } else{
    ftype <- x
  }
  ctypes <- x
  f <- list(ftype = ftype, ctypes = ctypes)
  # Do something here with x, args and put in something
  class(f) <- "ftype"
  return (f)
}

print.ftype <- function(f){
  cat("Ftype:  ", f$ftype, "\n")
  cat("Ctypes: ", f$ctypes)
}

#' @export
is_ftype <- function(x){
  inherits(x, "ftype")
}

to_ftype <- function(ctypes){
  if(!"ctype" %in% class(ctypes)) stop("Input must be ctypes")
  paste0(sort(strsplit(ftype,"-")[[1]]), collapse = "-")

}


# Generic methods
# cast <- function (x)  {
#   if(is.null(attr(x, "class"))){
#     print("no animal no milk")
#   }
#   if(any(c("ftype", "ctype") %in% attr(x, "class")))
#     UseMethod("milk", x)
# }
#
# cast <- function(object, ...){
#   UseMethod("cast")
# }

