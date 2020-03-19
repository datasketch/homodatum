isImgUrl <- function(x) all(grepl("^[http].+\\.((?i)jpg|png|gif|bmp|svg)$", x))


maybePct <- function(v){
  all(purrr::map(v, function(z) {all(z >= 0 && z<=1) }) %>% unlist() == TRUE)
}


guessCtype <- function(v){
  stop("guessCtype depricated. Please use guess_hdType()")

  if("data.frame" %in% class(v))
    v <- v %>% flatten
  v <- unique(v[!is.na(v)])
  if(length(v) == 0)
    return(hdType("___"))

  if(class(v) %in% c("integer","numeric")){
    ctype <- hdType("Num")
    if(all(v %in% 1500L:2200L)) ctype <- hdType("Yea")
    if(maybePct) ctype <- hdType("Pct")
    return(ctype)
  }
  if(class(v) == "Date")
    return(hdType("Dat"))
  if(class(v)!= "factor" & !has_warning(as.numeric(v))){
    return(hdType("Num"))
  }

  dth <- whichDTH(v)
  if(!is.null(dth))
    ctype <- dth
  else{
    v <- as.character(v)
    ctype <- hdType("Cat")
    if(ctype == hdType("Cat") && isImgUrl(v)){
      ctype <- hdType("Img")
    }
    if(ctype == hdType("Cat") && isTxType(v))
      ctype <- hdType("Txt")
  }
  ctype
}

isTxType <- function(v){
  #nwords <- function(x) vapply(strsplit(x, "\\W+"), length, integer(1))
  nwords <- function(x) vapply(strsplit(x, "[[:punct:] [^/]]"), length, integer(1))
  any(nchar(v) > 100) && any(nwords(v)>10)
}


#' @export
guessCtypes <- function(df, as_string = FALSE, named = FALSE){
  stop("guessCtypes depricated. Please use map(data, guess_hdType)")
  x <- purrr::map_chr(unname(df), guessCtype)
  if(as_string){
    return(paste(x, collapse="-"))
  }
  if(named)
    names(x) <- names(df)
  x
}

#' @export
guessCformats <- function(data){
  gc <- guessCtypes(data)
  defaultCformats[gc]
}


#' @export
guessFtype <- function(df){
  stop("guessFtype depricated. Please use guess_frType()")
  ctypes <- guessCtypes(df)
  ctypesToFtype(ctypes)
}

#' @export
ctypesToFtype <- function(ctypes, as_string = FALSE){
  f <- function(ctypes){
    ct <- count(tibble(ctypes = ctypes),ctypes)
    ct$n[ct$n == 1] <- ""
    ctv <- unite(ct,ctype,ctypes,n,sep="") %>% .[[1]] %>% sort()
    paste(ctv,collapse="-")
  }
  if(as_string){
    ctps <- strsplit(ctypes,"-")
    return(purrr::map_chr(ctps, f))
  }
  f(ctypes)
}


#' @export
forceCtypes <- function(df, ctypes, cformat = NULL){
  df <- as.data.frame(df)
  if(ncol(df)!= length(ctypes)) stop("number of df cols must be the same as col types length")
  for (i in seq_along(ctypes)){
    if(ctypes[i]=="Num"){df[,i]<- Num(df[,i])}
    if(ctypes[i]=="Yea"){df[,i]<- as.character(df[,i])}
    if(ctypes[i]=="Cat"){df[,i]<- Cat(df[,i])}
    if(ctypes[i]=="Txt"){df[,i]<- as.character(df[,i])}
    if(ctypes[i]=="Img"){
      if(!isImgUrl(df[,i])) stop ("Not an image Url")
      df[,i]<- as.character(df[,i])
    }
    if(ctypes[i]=="Dat"){df[,i]<- parseDatetime(df[,i],"Dat")}
    if(ctypes[i]=="Hms"){df[,i]<- parseDatetime(df[,i],"Hms")}
    if(ctypes[i]=="Dti"){df[,i]<- parseDatetime(df[,i],"Dti")}
    if(ctypes[i]=="Glt"){df[,i]<- as.numeric(df[,i])}
    if(ctypes[i]=="Gln"){df[,i]<- as.numeric(df[,i])}
    if(ctypes[i]=="Gnm"){df[,i]<- as.character(df[,i])}
  }
  as_tibble(df)
}
