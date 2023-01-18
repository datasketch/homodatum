#'
#' #' cast_hdtypes
#' #' Convert hdTypes
#' #' @name cast_hdtypes
#' #' @param data frame or a fringe.
#' #' @param from string of hdtypes.
#' #' @param to string of hdtypes.
#' #' @export
#' #' @return data frame or a fringe according to input.
#' #' @examples
#' #' cast_hdtypes(iris,"","")
#' cast_hdtypes <- function(data, from, to){
#'   f <- fringe(data)
#'   data <- f$d
#'   weekdays <- weekdays(data$a)
#'   return(weekdays)
#' }
#'
#' #' @export
#' castable_list <- function(hdtypes){
#'   f <- function(ctype){
#'     c(ctype,castable_ctype(ctype))
#'   }
#'   casts <- purrr::map(hdtypes,f)
#'   #do.call("crossing_",casts)
#'   tibble::as_tibble(expand.grid(casts,stringsAsFactors = FALSE))
#' }
#'
#' castable_ctype <- function(ctype){
#'   cc <- castable_hdtypes()
#'   cc %>% dplyr::filter(from == ctype) %>% dplyr::pull()
#' }
#'
#' cast_ctype <- function(from, to,...){
#'   if(castable_hdtypes(from, to))
#'     do.call(paste0("cast_",from,to),list(...))
#'
#' }
#'
#' castable_hdtypes <- function(){
#'   tibble::tribble(~from, ~to,
#'                   "Cat","Uid",
#'                   "Cat","Seq",
#'                   "Cat","Txt",
#'                   "Cat","Bin",
#'                   "Num","Uid",
#'                   "Num","Pct",
#'                   "Num","Dst",
#'                   "Dat","Yea",
#'                   "Dat","Mon",
#'                   "Dat","Day",
#'                   "Dat","Wdy",
#'                   "Dtm","Dat",
#'                   "Dtm","Hms",
#'                   "Hms","Min",
#'                   "Hms","Min",
#'                   "Hms","Sec",
#'                   "Seq","Uid",
#'                   "Seq","Num",
#'                   "Pct","Num"
#'   )
#' }
#'
#'
#'
#' cast_Da2Ca_weekdays <- function(data, ...){
#'   f <- fringe(data)
#'   data <- f$d
#'   weekdays <- weekdays(data$a)
#'   return(weekdays)
#' }
#'
#' cast_Da2Ca_months <- function(data, ...){
#'   f <- fringe(data)
#'   data <- f$d
#'   months <- months(data$a)
#'   return(months)
#' }
#'
#' cast_Da2Ca_years <- function(data, ...){
#'   f <- fringe(data)
#'   data <- f$d
#'   years <- lubridate::year(data$a)
#'   return(years)
#' }
#'
#' cast_Da2Ca_qtr <- function(data, ...){
#'   f <- fringe(data)
#'   data <- f$d
#'   year_qtr <- zoo::as.yearqtr(data$a)
#'   return(year_qtr)
#' }
#'
#' cast_Da2Ca_weeks <- function(data, ...){
#'   f <- fringe(data)
#'   data <- f$d
#'   weeks <- as.numeric(format(data$a+3, "%U"))
#'   return(weeks)
#' }
#'
#' cast_Da2Ca_weekend <- function(data, ...){
#'   f <- fringe(data)
#'   data <- f$d
#'   weekdays <- weekdays(data$a)
#'   weekend <- ifelse(weekdays == "Saturday"|weekdays == "Sunday", 1, 0)
#'   return(weekend)
#' }
