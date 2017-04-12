#' cast_Da2Ca_weekdays : title.
#' Convert Data type to Category weekdays
#' @name cast_Da2Ca_weekdays
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
cast_Da2Ca_weekdays <- function(data, ...){
  f <- fringe(data)
  data <- f$d
  weekdays <- weekdays(data$a)
  return(weekdays)
}

#' cast_Da2Ca_months : title.
#' Convert Data type to Category months
#' @name cast_Da2Ca_months
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
cast_Da2Ca_months <- function(data, ...){
  f <- fringe(data)
  data <- f$d
  months <- months(data$a)
  return(months)
}

#' cast_Da2Ca_years : title.
#' Convert Data type to Category years
#' @name cast_Da2Ca_years
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
cast_Da2Ca_years <- function(data, ...){
  f <- fringe(data)
  data <- f$d
  years <- year(data$a)
  return(years)
}

#' cast_Da2Ca_qtr : title.
#' Convert Data type to Category quaters
#' @name cast_Da2Ca_qtr
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
cast_Da2Ca_qtr <- function(data, ...){
  f <- fringe(data)
  data <- f$d
  year_qtr <- as.yearqtr(data$a)
  return(year_qtr)
}

#' cast_Da2Ca_weeks : title.
#' Convert Data type to Category weeks
#' @name cast_Da2Ca_weeks
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
cast_Da2Ca_weeks <- function(data, ...){
  f <- fringe(data)
  data <- f$d
  weeks <- as.numeric(format(data$a+3, "%U"))
  return(weeks)
}

#' cast_Da2Ca_weekend : title.
#' Convert Data type to Category weekend
#' @name cast_Da2Ca_weekend
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
cast_Da2Ca_weekend <- function(data, ...){
  f <- fringe(data)
  data <- f$d
  weekdays <- weekdays(data$a)
  weekend <- ifelse(weekdays == "Saturday"|weekdays == "Sunday", 1, 0)
  return(weekend)
}

