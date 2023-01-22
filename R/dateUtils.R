
isDate <- function(v){
  if("Date" %in% class(v)){
    return(TRUE)
  }
  if(any(grepl("[a-zA-Z]", v)))
    return(FALSE)
  guess_date <- try(parseDate(v), silent = TRUE)
  if(inherits(guess_date, "try-error")){
    return(FALSE)
  }
  if(inherits(guess_date, "try-warning")){
    return(FALSE)
  }
  if(dstools::na_proportion(guess_date) > 0.6){ # failed to parse 60% of dates
    return(FALSE)
  }
  # if(length((guess))< length(v))
  #   #stop("Dates do not seem to have the same format")
  #   return(FALSE)
  # if(unique(guess_date) != expectedFormat)
  #   return(FALSE)
  TRUE
}

parseDate <- function(v, format = NULL){
  #v0 <- v[!is.na(v)]
  v0 <- v
  format <- makeup::guess_date_fmt(v0)
  as.Date(v, format = format)
}

### Importar de makeup
# guess_date_fmt <- function(sample){
#   #locale <- locale %||% guess_date_locale(sample)
#   format_orders <- c("ymd", "mdY", "dmy", "BdY", "Bdy","dBY","dbY", "bdY", "bdy",
#                      "Bd","bd", "dB","db")
#   fmts <- lubridate::guess_formats(sample, format_orders)
#   fmts_count <- rev(sort(table(names(fmts)))) # table apparently does not return results sorted
#   fmt_nm <- names(fmts_count[1]) # take the most popular
#   fmt <- fmts[fmt_nm]
#   fmt
# }



parseDatetime <- function(v, datetimeType){
  v0 <- v[!is.na(v)]
  v0 <- v
  if(datetimeType == "Dat"){
    format <- makeup::guess_date_fmt(v0)
    outVals <- as.Date(v, format = format)
  }
  if(datetimeType == "Hms"){
    format <- lubridate::guess_formats(v0, "HMS")
    v <- as.POSIXct(v, format = format)
    outVals <- format(v, format="%H:%M:%S")
  }
  if(datetimeType == "Dtm"){
    format <- lubridate::guess_formats(v0, "ymd HMS")
    outVals <- as.POSIXct(v, format = format)
  }
  outVals
}


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
    guess <- lubridate::guess_formats(v,format)
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



isTime <- isD_format("HMS","%H:%M:%S")

isDatetime <- isD_format("ymd HMS","%Y-%m-%d %H:%M:%S")


