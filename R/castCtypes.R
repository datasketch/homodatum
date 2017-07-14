
#' cast_ctypes
#' Convert Ctypes
#' @name cast_ctypes
#' @param data frame or a fringe.
#' @param from string of Ctypes.
#' @param to string of Ctypes.
#' @export
#' @return data frame or a fringe according to input.
#' @examples
#' cast_ctypes(iris,"","")
cast_ctypes <- function(data, from, to){
  f <- fringe(data)
  data <- f$d
  weekdays <- weekdays(data$a)
  return(weekdays)
}

#' @export
castable_list <- function(ctypes){
  f <- function(ctype){
    c(ctype,castable_ctype(ctype))
  }
  casts <- map(ctypes,f)
  #do.call("crossing_",casts)
  as_tibble(expand.grid(casts))
}

castable_ctype <- function(ctype){
  cc <- castable_ctypes()
  cc %>% filter(from == ctype) %>% pull()
}

cast_ctype <- function(from, to,...){
  if(castable_ctypes(from, to))
    do.call(paste0("cast_",from,to),list(...))

}

castable_ctypes <- function(){
  casts <- "from,to
Cat,Uid
Cat,Oca
Cat,Txt
Cat,Bin
Num,Uid
Num,Pct
Num,Dst
Dat,Yea
Dat,Mon
Dat,Day
Dat,Wdy
Dtm,Dat
Dtm,Hms
Hms,Min
Hms,Min
Hms,Sec
Oca,Uid
Oca,Num
Pct,Num
"
  read_csv(casts)

}



cast_Da2Ca_weekdays <- function(data, ...){
  f <- fringe(data)
  data <- f$d
  weekdays <- weekdays(data$a)
  return(weekdays)
}

cast_Da2Ca_months <- function(data, ...){
  f <- fringe(data)
  data <- f$d
  months <- months(data$a)
  return(months)
}

cast_Da2Ca_years <- function(data, ...){
  f <- fringe(data)
  data <- f$d
  years <- year(data$a)
  return(years)
}

cast_Da2Ca_qtr <- function(data, ...){
  f <- fringe(data)
  data <- f$d
  year_qtr <- as.yearqtr(data$a)
  return(year_qtr)
}

cast_Da2Ca_weeks <- function(data, ...){
  f <- fringe(data)
  data <- f$d
  weeks <- as.numeric(format(data$a+3, "%U"))
  return(weeks)
}

cast_Da2Ca_weekend <- function(data, ...){
  f <- fringe(data)
  data <- f$d
  weekdays <- weekdays(data$a)
  weekend <- ifelse(weekdays == "Saturday"|weekdays == "Sunday", 1, 0)
  return(weekend)
}

