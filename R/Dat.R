
new_Dat <- function(x = character(), format = NULL,
                    skip_stats = FALSE){
  vctrs::vec_assert(x, character())
  if(is.null(format) & !all(is.na(x))){
    date_orders <- c("dmy","mdy","ymd")
    guess_fmt <- lubridate::guess_formats(x[!is.na(x)],date_orders)
    fmt <- guess_fmt[vctrs::vec_in(names(guess_fmt), date_orders)][1] # take first guess
  }else{
    fmt <- format
  }
  d <- as.Date(x, format = fmt %||% "%Y-%Om-%d")
  stats <- NULL

  nms <- names(x)
  stats <- NULL
  if(!skip_stats){
    summary <- table(x,useNA = "always") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(dist = n/sum(n), names = c(nms, NA)) %>%
      dplyr::rename(category = x)
    stats <- list(
      n_unique = length(unique(x)),
      n_na = sum(is.na(x)),
      pct_na = sum(is.na(x))/length(x),
      summary = summary
    )
  }

  # vctrs::vec_assert(format, ptype = character(), size = 1)
  vctrs::vec_assert(d, vctrs::new_date())
  vctrs::new_vctr(d, format = unname(fmt), order = names(fmt),
           stats = stats, class = "hd_Dat")
}

Dat_formats <- function(order = c("dmy","mdy","ymd"), sep = "-"){
  mdy <- c("%m", "%d", "%Y")
  c("YYYY-MM-DD", "MM/DD/YYYY")
}

#' @title Date Vectors
#' @description Creates objects of type "hd_Dat". hd_Dat objects contain three main attributes: categories, n_categories and stats. With categories you can check wich values are valid for the variable. With n_categories you can check how many of valid values are valid for the variable. With the last attribute, stats, you can check different basic operations to describe the varaible (n_unique, n_na, pct_na and summary).
#'
#' @param x object to be created as Dat type
#'
#' @param format a character vector of date-time formats. Default is "%-m/%-d/%Y"
#' @param skip_stats a logical evaluating to TRUE or FALSE indicating whether variable stats should be calculated and added to the hd_Dat object. The stats are n of unique categories, n of NA values, percentage of NA values and a frequency table.
#'
#' @examples
#' x <-   c("2000-12-04","2010-20-04")
#' dat <- Dat(x, format = "%Y-%d-%m")
#' dat
#' class(dat)
#' attr(dat, "stats")
#'
#' @export
Dat <- function(x = character(), format = NULL, skip_stats = FALSE) {
  if(inherits(x, "POSIXct")){
    x <- as.character(x)
  }
  if(vctrs::vec_is(x, vctrs::new_date())){
    x <- as.character(x)
  }
  x <- vctrs::vec_cast(x, character())
  # format <- vec_recycle(vctrs::vec_cast(format, character()), format)
  new_Dat(x, format = format,
          skip_stats = skip_stats)
}

#' @title Date Vectors
#' @description test for objects of type "hd_Dat"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hd_Dat or not.
#'
#' @examples
#' x <-   c("2000-12-04","2010-20-04")
#' dat <- Dat(x, format = "%Y-%d-%m")
#' is_Dat(dat)
#'
#' @export
is_Dat <- function(x) {
  inherits(x, "hd_Dat")
}

# Methods

## Format method
Dat_get_format <- function(x) attr(x, "format")
Dat_get_order <- function(x) attr(x, "order")

Dat_show <- function(x, format = NULL){
  if(all(is.na(x))) return(sprintf(fmt = "%s", as.character(x)))
  date <- vctrs::new_date(x)
  format(date, format %||% Dat_get_format(x))
}

Dat_get_isodate <- function(x) as.character(vctrs::new_date(vctrs::vec_data(x)))

Dat_get_stats <-  function(x){
  if(!is_Dat(x)) stop("x must be a Dat")
  attr(x, "stats")
}


#' @export
format.hd_Dat <- function(x, ...) {
  ## Check if prints as ISO, otherwise show with given format
  if(all(Dat_show(x[!is.na(x)]) == as.character(vctrs::new_date(x[!is.na(x)])))){
    info <- ""
  }else{
    info <- paste0(" (", Dat_show(x), ")")
  }
  sprintf(paste0(vctrs::new_date(x),info))
}

#' @export
vec_ptype_abbr.hd_Dat <- function(x, ...) {
  "Dat"
}
# vec_ptype_full.hd_Dat <- function(x, ...) {
#   paste0("Dat<", format(x), ">")
# }



# Coercion

#' @rdname vctrs-compat
#' @method vec_ptype2 hd_Dat
#' @export
#' @export vec_ptype2.hd_Dat
vec_ptype2.hd_Dat <- function(x, y, ...) UseMethod("vec_ptype2.hd_Dat", y)

#' @method vec_ptype2.hd_Dat default
#' @export
vec_ptype2.hd_Dat.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}



# A Dat combined with a Dat returns a Dat... what happens when they have different formats?
# TODO default to ISO

#' @method vec_ptype2.hd_Dat hd_Dat
#' @export
vec_ptype2.hd_Dat.hd_Dat <- function(x, y, ...) {
  new_Dat(format = "%Y-%Om-%d")
}
# vec_ptype2.hd_Dat.hd_Dat <- function(x, y, ...) new_Dat()


# Dat and character return character

#' @method vec_ptype2.hd_Cat character
#' @export
vec_ptype2.hd_Dat.character <- function(x, y, ...) Dat()

#' @method vec_ptype2.character hd_Dat
#' @export
vec_ptype2.character.hd_Dat <- function(x, y, ...) Dat()
# vec_ptype2.hd_Dat.new_date <- function(x, y, ...) Dat()
# vec_ptype2.new_date.hd_Dat <- function(x, y, ...) Dat()

# Casting

#' @rdname vctrs-compat
#' @method vec_cast hd_Dat
#' @export
#' @export vec_cast.hd_Cat
vec_cast.hd_Dat <- function(x, to, ...) UseMethod("vec_cast.hd_Dat")

#' @method vec_cast.hd_Dat default
#' @export
vec_cast.hd_Dat.default <- function(x, to, ...) vec_default_cast(x, to)

# Coerce Dat to Dat

#' @method vec_cast.hd_Dat hd_Dat
#' @export
vec_cast.hd_Dat.hd_Dat <- function(x, to, ...) x
# vec_cast.hd_Dat.hd_Dat <- function(x, to, ...) {
#   new_Dat(vctrs::vec_data(x), format = NULL)
# }

#' @method vec_cast.hd_Dat character
#' @export
vec_cast.hd_Dat.character <- function(x, to, ...) Dat(x)

#' #' @method vec_cast.character hd_Dat
#' #' @export
#' vec_cast.character.hd_Dat <- function(x, to, ...) Dat_show(x)

#' @method as.character hd_Dat
#' @export
as.character.hd_Dat <- function(x) Dat_show(x)



#' @method vec_cast.hd_Dat date
#' @export
vec_cast.hd_Dat.date <- function(x, to, ...) vctrs::new_date(vctrs::vec_data(x))

#' #' @method vec_cast.date hd_Dat
#' #' @export
vec_cast.date.hd_Dat <- function(x, to, ...) as.character(vctrs::new_date(vctrs::vec_data(x)))

#' @method as.Date hd_Dat
#' @export
as.Date.hd_Dat <- function(x) as.Date(vec_data(x), origin = "1970-01-01")



#' @method as.double hd_Dat
#' @export
as.double.hd_Dat <- function(x) as.double(vec_data(x))



#' @export
as_Dat <- function(x) {
  vctrs::vec_cast(x, new_Dat())
}


