
new_Dat <- function(x = character(), format = NULL,
                    skip_stats = FALSE){
  vec_assert(x, character())
  if(is.null(format) & !all(is.na(x))){
    date_orders <- c("dmy","mdy","ymd")
    guess_fmt <- guess_formats(x,date_orders)
    fmt <- guess_fmt[vec_in(names(guess_fmt), date_orders)][1] # take first guess
  }else{
    fmt <- format
  }
  d <- as.Date(x, format = fmt %||% "%Y-%Om-%d")
  stats <- NULL
  if(!skip_stats && !all(is.na(d))){
    stats <- list(
      min = min(d, na.rm = TRUE),
      max = max(d, na.rm = TRUE)
    )
  }
  # vec_assert(format, ptype = character(), size = 1)
  vec_assert(d, new_date())
  new_vctr(d, format = unname(fmt), order = names(fmt),
           stats = stats, class = "hd_Dat")
}

Dat_formats <- function(order = c("dmy","mdy","ymd"), sep = "-"){
  mdy <- c("%m", "%d", "%Y")
  c("YYYY-MM-DD", "MM/DD/YYYY")
}

Dat <- function(x = character(), format = NULL, skip_stats = FALSE) {
  x <- vec_cast(x, character())
  # format <- vec_recycle(vec_cast(format, character()), format)
  new_Dat(x, format = format, skip_stats = skip_stats)
}

is_Dat <- function(x) {
  inherits(x, "hd_Dat")
}

# Methods

## Format method
Dat_get_format <- function(x) attr(x, "format")
Dat_get_order <- function(x) attr(x, "order")

Dat_show <- function(x, format = NULL){
  if(all(is.na(x))) return(sprintf(fmt = "%s", as.character(x)))
  date <- new_date(x)
  format(date, format %||% Dat_get_format(x))
}

Dat_get_isodate <- function(x) as.character(new_date(vec_data(x)))

Dat_get_stats <-  function(x){
  if(!is_Dat(x)) stop("x must be a Dat")
  attr(x, "stats")
}


format.hd_Dat <- function(x, ...) {
  sprintf(paste0(new_date(x)," (", Dat_show(x), ")"))
}

vec_ptype_abbr.hd_Dat <- function(x, ...) {
  "Dat"
}
# vec_ptype_full.hd_Dat <- function(x, ...) {
#   paste0("Dat<", format(x), ">")
# }



# Coercion
vec_ptype2.hd_Dat <- function(x, y, ...) UseMethod("vec_ptype2.hd_Dat", y)
vec_ptype2.hd_Dat.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

vec_cast.vctrs_Dat <- function(x, to, ...) UseMethod("vec_cast.vctrs_decimal")
vec_cast.vctrs_decimal.default <- function(x, to, ...) vec_default_cast(x, to)

# A Dat combined with a Dat returns a Dat... what happens when they have different formats?
# TODO default to ISO
vec_ptype2.vctrs_Dat.vctrs_Dat <- function(x, y, ...) {
  new_Dat(digits = max(digits(x), digits(y)))
}
vec_cast.vctrs_decimal.vctrs_decimal <- function(x, to, ...) {
  new_decimal(vec_Data(x), digits = digits(to))
}


vec_ptype2.hd_Dat.hd_Dat <- function(x, y, ...) new_Dat()
# Dat and character return character
vec_ptype2.hd_Dat.character <- function(x, y, ...) character()
vec_ptype2.character.hd_Dat <- function(x, y, ...) character()

# Casting
vec_cast.vctrs_Dat <- function(x, to, ...) UseMethod("vec_cast.hc_Dat")
vec_cast.vctrs_percent.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Dat to Dat
vec_cast.hd_Dat.hd_Dat <- function(x, to, ...) x
vec_cast.hd_Dat.character <- function(x, to, ...) percent(x)
vec_cast.character.hd_Dat <- function(x, to, ...) vec_data(x)

as_Dat <- function(x) {
  vec_cast(x, new_Dat())
}


