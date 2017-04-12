

sample2 <- function(v, n,replace = TRUE, ...){
  if(length(v)==1) return(rep(v,5))
  sample(v,n,replace = replace, ...)
}


loremNames <- function(ncol){
  lorem0 <- "lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor
  incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
  quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
  consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
  cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
  proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  lorem1 <- gsub("[[:punct:]|\n]", "", lorem0)
  lorem1 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", lorem1, perl=TRUE)
  lorem2 <- unique(strsplit(lorem1," ",fixed = TRUE)[[1]])
  sample2(lorem2,ncol,replace = FALSE)
}

#' @export
letterNames <- function(n){
  if(n<27)
    return(letters[1:n])
  if(n<703){
    l2 <- expand(data_frame(A=letters,B=letters),A,B) %>% unite("l",A,B,sep="") %>% .$l
    return(c(letters,l2)[1:n])
  }
  if(n < 18279){ # 26 + 676 + 17576 = 18278
    l2 <- expand(data_frame(A=letters,B=letters),A,B) %>% unite("l",A,B,sep="") %>% .$l
    l3 <- expand(data_frame(A=letters,B=letters,C=letters),A,B,C) %>% unite("l",A,B,C,sep="") %>% .$l
    return(c(letters,l2,l3)[1:n])
  }
  stop("Cannot handle data with more than 18279 columns")
}

has_warning <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  length(warn) > 0
}
