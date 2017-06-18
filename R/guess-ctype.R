isImgUrl <- function(x) all(grepl("([^http\\s]+(\\.(?i)(jpg|png|gif|bmp|svg))$)",x))

guessCtype <- function(v){
  if("data.frame" %in% class(v))
    v <- v %>% flatten
  v <- unique(v[!is.na(v)])
  if(length(v) == 0)
    return("___")

  if(class(v) %in% c("integer","numeric")){
    ctype <- "Num"
    if(all(v %in% 1800:2200)) ctype <- "Yea"
    if(all(v >= 0 && v<=1) && !all(v %in% c(0,1))) ctype <- "Pct"
    # if(all(v %in% 1:31)) ctype <- "Dy"
    #if(all(v %in% 1:12)) ctype <- "Mn"
    return(ctype)
  }
  if(class(v) == "Date")
    return("Dat")
  if(class(v)!= "factor" & !has_warning(as.numeric(v))){
    return("Num")
  }

  dth <- whichDTH(v)
  if(!is.null(dth))
    ctype <- dth
  else{
    v <- as.character(v)
    ctype <- "Cat"
    if(ctype == "Cat" && isImgUrl(v)){
      ctype <- "Imt"
    }
    if(ctype == "Cat" && isTxType(v))
      ctype <- "Txt"
  }
  ctype
}

isTxType <- function(v){
  #nwords <- function(x) vapply(strsplit(x, "\\W+"), length, integer(1))
  nwords <- function(x) vapply(strsplit(x, "[[:punct:] [^/]]"), length, integer(1))
  any(nchar(v) > 100) && any(nwords(v)>10)
}


#' @export
guessCtypes <- function(df, as_string = FALSE){
  x <- purrr::map_chr(unname(df),guessCtype)
  if(as_string){
    return(paste(x,collapse="-"))
  }
  x
}

#' @export
guessCformats <- function(data){
  gc <- guessCtypes(data)
  defaultCformats[gc]
}


#' @export
guessFtype <- function(df){
  ctypes <- guessCtypes(df)
  ctypesToFtype(ctypes)
}

#' @export
ctypesToFtype <- function(ctypes, as_string = FALSE){
  if(as_string){
    ctypes <- strsplit(ctypes,"-")[[1]]
  }
  ct <- count(data_frame(ctypes = ctypes),ctypes)
  ct$n[ct$n == 1] <- ""
  ctv <- unite(ct,ctype,ctypes,n,sep="") %>% .[[1]] %>% sort()
  paste(ctv,collapse="-")
}


#' @export
forceCtypes <- function(df, ctypes, cformat = NULL){
  df <- as.data.frame(df)
  if(ncol(df)!= length(ctypes)) stop("number of df cols must be the same as col types length")
  for (i in seq_along(ctypes)){
    if(ctypes[i]=="Num"){df[,i]<- as.numeric(df[,i])}
    if(ctypes[i]=="Yea"){df[,i]<- as.character(df[,i])}
    if(ctypes[i]=="Cat"){df[,i]<- as.character(df[,i])}
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
  as_data_frame(df)
}
