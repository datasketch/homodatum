#' @export
powerSet <- function(set) {
  n <- length(set)
  masks <- 2^(1:n-1)
  l <- lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
  l[-1]
}

powerSet2 <- function(set) {
  n <- length(set)
  masks <- 2^(1:n-1)
  l <- lapply( 1:2^n-1, function(u) {
    x <- set[ bitwAnd(u, masks) != 0 ]
  } )
  l
}


permuteVector <- function(v){
  m <- matrix(v[permutations(length(v))],ncol=length(v))
  tibble::as_tibble(m)
}

permutations <- function(n){
  if(n==1){
    return(matrix(1))
  } else {
    sp <- permutations(n-1)
    p <- nrow(sp)
    A <- matrix(nrow=n*p,ncol=n)
    for(i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
    }
    return(A)
  }
}


remove_accents <- function (string) {
  accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûÂÊÎÔÛñÑç"
  translation <- "aeiouAEIOUaeiouyAEIOUYaeiouAEIOUaeiouAEIOUnNc"
  chartr(accents, translation, string)
}

col_ids_from_name <- function (x, sep = "_"){
  x <- gsub("[^[:alnum:]]", "_", x)
  x <- remove_accents(x)
  x <- tolower(x)
  x <- gsub("-+", "_", x)
  x <- gsub("[[:punct:]]+","_",x)
  x <- gsub("+[[:punct:]]$", "", x)
  x <- gsub("^-.", "", x)
  x


  # x <- gsub("[^[:alnum:]]", "-", x)
  # x <- remove_accents(tolower(x))
  # x <- gsub("-+", "-", x)
  # x <- gsub("^-.", "", x)
  # x

}


sample2 <- function(v, n,replace = TRUE, ...){
  if(length(v)==1) return(rep(v,5))
  sample(v,n,replace = replace, ...)
}



firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
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
  lorem2 <- unique(firstup(strsplit(lorem1," ",fixed = TRUE)[[1]]))
  sample2(lorem2,ncol,replace = FALSE)
}

#' @export
letterNames <- function(n){
  if(n<27)
    return(letters[1:n])
  if(n<703){
    l2 <- expand(tibble(A=letters,B=letters),A,B) %>%
      tidyr::unite("l",A,B,sep="") %>% .$l
    return(c(letters,l2)[1:n])
  }
  if(n < 18279){ # 26 + 676 + 17576 = 18278
    l2 <- expand(tibble(A=letters,B=letters),A,B) %>%
      tidyr::unite("l",A,B,sep="") %>% .$l
    l3 <- expand(tibble(A=letters,B=letters,C=letters),A,B,C) %>%
      tidyr::unite("l",A,B,C,sep="") %>% .$l
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

which_in <- function (x, y) x[x %in% y]

na_proportion <- function(x){
  if(length(x) < 4) return(0)
  sum(is.na(x))/length(x)
}

many_words_proportion <- function(x) sum(grepl("[^\\s]([ ]{1,})[^\\s]",x))/length(x)

na_to_chr <- function(x, na){
  x[is.na(x)] <- na
  x
}

insert_column <- function(d, vector, target, col_name){
  if(ncol(d) == 1){
    d[[col_name]] <- vector
    return(d)
  }
  new_col <- data.frame(vector, stringsAsFactors = FALSE)
  names(new_col) <- col_name
  if(target == ncol(d)){
    d[[col_name]] <- vector
    return(d)
  }
  cbind(d[,1:target,drop=FALSE], new_col, d[,(target+1):length(d),drop=FALSE])
}

make_slug <- function(x){
  x <- gsub("[^[:alnum:]]", "-", x)
  x <- remove_accents(tolower(x))
  x <- gsub("-+", "-", x)
  x <- gsub("+[[:punct:]]$", "", x)
  x <- gsub("^-.", "", x)
  x
}


sys_file <- function(...){
  system.file(..., package = "homodatum")
}


`%||%` <- function (x, y){
  suppressWarnings({
    if (is.empty(x))
      return(y)
    else if (is.null(x) || is.na(x))
      return(y)
    else if (class(x) == "character" && all(nchar(x) == 0))
      return(y)
    else x
  })
}

is.empty <- function (x) {
  !as.logical(length(x))
}


#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`



