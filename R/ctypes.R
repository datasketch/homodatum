
ctype <- function(x, ...)  {
  args <- list(...)
  if(!"character" %in% class(x)) stop("x must be a character value")

  if(!x %in% availableCtypes()) stop("Ctype must be valid, check availableCtypes()")
  ctypes <- x
  ct <- list(id = x, format = args$format)
  # Do something here with x, args and put in something
  class(f) <- "ctype"
  return (f)
}

print.ctype <- function(f){
  cat("Ctype:  ", f$id, "\n")
  cat("Format: ", f$format)
}

#' @export
is_ctype <- function(x){
  inherits(x, "ctype")
}






sampleCtypes <- function(ftype, as_df = FALSE){
  #ftype <- "Num2-Cat1-DatP"
  #ftype <- "Num-Cat-DatP"
  #ftype <- "Cat3-Num1"
  # ftype <- availableCtypeIds()
  avCtypes <- availableCtypes(as_df = TRUE)
  if(is.numeric(ftype)){
    n <- ftype
    ctypes <- sample_n(avCtypes,n,replace = TRUE, weight = weight)
  }
  if(is.character(ftype)){
    ft1 <- strsplit(ftype,"-",fixed = TRUE)[[1]]
    cts <- substring(ft1,1,3)
    reps <- substring(ft1,4)
    ctypes <- purrr::flatten_chr(map2(cts,reps,function(x,y){
      if(y == "P"){
        return(rep(x,sample(2:6,1)))
      }
      if(y == "") y = 1
      rep(x,as.numeric(y))
    }))
    if(!all(ctypes %in% availableCtypeIds())) stop("Wrong ftype")
    ctypes <- data_frame(id = ctypes,
                         label = avCtypes[[2]][match(ctypes, avCtypes[[1]])] )
  }
  if(as_df)
    return(ctypes)
  ctypes$id
}



#' @export
availableCtypes <- function(as_df = FALSE){

  ctypes <- 'id,label,weight
___,Null,1
Uid,Uid,1
Cat,Categorical,5
Bin,Binary,3
Seq,Sequential,3
Num,Numeric,5
Pct,Percentage,2
Dst,Distribution,2
Dat,Date,5
Yea,Year,1
Mon,Month,1
Day,Day,1
Wdy,Day of week,1
Ywe,Week in year,1
Dtm,Date time,1
Hms,Time HMS,1
Min,Minutes,1
Sec,Seconds,1
Hie,Hierarchy,1
Grp,Group,1
Txt,Text,1
Mny,Money,1
Gnm,Geo name,1
Gcd,Geo code,1
Glt,Geo latitude,1
Gln,Geo longitude,1
Img,Image,1
Aud,Audio,1
'
  ctypes <- read_csv(ctypes)
  if(as_df)
    return(ctypes)
  #set_names(as.list(ctypes$id),ctypes$label)
  ctypes$id
}

#' @export
availableCtypeIds <- function(allowEmpty = TRUE) {
  ctypes <- unname(availableCtypes())
  if(!allowEmpty) return(ctypes[ctypes != "___"])
  ctypes
}


#' @export
availableCformats <- function(){
  list(
    "___" = "",
    "Cat" = "",
    "Oca" = "json_levels",
    "Num" = "Numeric",
    "Pct" = "Percentage",
    "Dst" = "Distribution",
    "Dat" = "yyyy-mm-dd hh:mm:ss",
    "Yea" = "",
    "Mon" = "",
    "Day" = "",
    "Wdy" = "language",
    "Dtm" = c("yyyy-mm-dd","unixTimeStamp"),
    "Hms" = "HH:MM:SS",
    "Min" = "Minutes",
    "Sec" = "Seconds",
    "Hie" = "label_code_order",
    "Grp" = "label_code",
    "Txt" = c("plain","html","markdown"),
    "Mny" = "ISO-4217",
    "Gcd" = c("ISO 3166-1 alpha-2","ISO 3166-1 alpha-3","DANE"),
    "Gnm" = "",
    "Glt" = c("degrees-decimal"),
    "Gln" = c("degrees-decimal"),
    "Img" = c("imageUrl","file"),
    "Aud" = "Audio"
  )
}

defaultCformats <- map(availableCformats(),1)

getDefaultCformats <- function(ctypes){
  l <- lapply(ctypes, function(ctype){
    defaultCformats[[ctype]]
  })
  unlist(l)
}

