


# v <- c("2014-03-01","2043-04-04","2014-04-04")
# isDate(v)
# isDatetime(v)
# isTime(v)
# whichDTH(v)
# v <- c("2014-03-01","2043-04-04","20140404")
# parseDatetime(v, "D")
# v <- c("2014-03-01 5:04:00","2043-04-04 5:04:00","2014-04-04 5:04:00")
# parseDatetime(v, "T")
# v <- c("04:00","13:05:00","5:04:00")
# parseDatetime(v, "H")


parseDatetime <- function(v, datetimeType){
  v0 <- v[!is.na(v)]
  v0 <- v
  if(datetimeType == "Dat"){
    #format <- guess_formats(v0, "ymd")
    #guess_formats(v0, "ymd",print_matches = TRUE)
    outVals <- as.Date(v, format = "%Y-%m-%d") # NO GUESSING
  }
  if(datetimeType == "Hms"){
    format <- guess_formats(v0, "HMS")
    v <- as.POSIXct(v, format = format)
    outVals <- format(v, format="%H:%M:%S")
  }
  if(datetimeType == "Dtm"){
    format <- guess_formats(v0, "ymd HMS")
    outVals <- as.POSIXct(v, format = format)
  }
  outVals
}


guessDateFormat <- function(v) parseDateTime(v)$format


whichDTH <-function(x){
  d <- c(isDate(x),isDatetime(x), isTime(x))
  if(!any(d)) return(NULL)
  #Reduce(`||`,d)
  dth <- c("Dat","Dtm","Hms")
  dth[d]
}

isD_format <- function(format, expectedFormat){
  function(v){
    v <-as.character(v)
    if(class(v) == "Date"){
      return(TRUE)
    }
    guess = guess_formats(v,format)
    if(all(guess %in% c("%Y-%Om-%d","%Y-%m-%d" )) & format =="%Y-%m-%d")
      return(TRUE)
    if(length((guess))< length(v))
      #stop("Dates do not seem to have the same format")
      return(FALSE)
    if(unique(guess) != expectedFormat)
      return(FALSE)
    !is.null(guess)
  }
}

isDate <- function(v){
  if("Date" %in% class(v)){
    return(TRUE)
  }
  guess <- guess_formats(v,"%Y-%m-%d")
  if(is.null(guess)){
    return(FALSE)
  }else{
    if(all(guess %in% c("%Y-%Om-%d","%Y-%m-%d")))
      return(TRUE)
    if(length((guess))< length(v))
      #stop("Dates do not seem to have the same format")
      return(FALSE)
    if(unique(guess) != expectedFormat)
      return(FALSE)
  }
  TRUE
}
isTime <- isD_format("HMS","%H:%M:%S")
isDatetime <- isD_format("ymd HMS","%Y-%m-%d %H:%M:%S")


