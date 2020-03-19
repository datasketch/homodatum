
new_Dat <- function(x = character(), format = NULL,
                    skip_stats = FALSE){
  vec_assert(x, character())
  if(is.null(format) & !all(is.na(x))){
    date_orders <- c("dmy","mdy","ymd")
    guess_fmt <- guess_formats(x[!is.na(x)],date_orders)
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
  if(vctrs::vec_is(x, new_date())){
    x <- as.character(x)
  }
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
  ## Check if prints as ISO, otherwise show with given format
  if(all(Dat_show(x[!is.na(x)]) == as.character(new_date(x[!is.na(x)])))){
    info <- ""
  }else{
    info <- paste0(" (", Dat_show(x), ")")
  }
  sprintf(paste0(new_date(x),info))
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

vec_cast.hd_Dat <- function(x, to, ...) UseMethod("vec_cast.hd_Dat")
vec_cast.hd_Dat.default <- function(x, to, ...) vec_default_cast(x, to)

# A Dat combined with a Dat returns a Dat... what happens when they have different formats?
# TODO default to ISO
vec_ptype2.hd_Dat.hd_Dat <- function(x, y, ...) {
  new_Dat(format = "%Y-%Om-%d")
}
vec_cast.hd_Dat.hd_Dat <- function(x, to, ...) {
  new_Dat(vec_Data(x), format = NULL)
}


vec_ptype2.hd_Dat.hd_Dat <- function(x, y, ...) new_Dat()
# Dat and character return character
vec_ptype2.hd_Dat.character <- function(x, y, ...) Dat()
vec_ptype2.character.hd_Dat <- function(x, y, ...) Dat()
# vec_ptype2.hd_Dat.new_date <- function(x, y, ...) Dat()
# vec_ptype2.new_date.hd_Dat <- function(x, y, ...) Dat()

# Casting
vec_cast.vctrs_Dat <- function(x, to, ...) UseMethod("vec_cast.hc_Dat")
vec_cast.vctrs_Dat.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Dat to Dat
vec_cast.hd_Dat.hd_Dat <- function(x, to, ...) x
vec_cast.hd_Dat.character <- function(x, to, ...) Dat(x)
vec_cast.character.hd_Dat <- function(x, to, ...) Dat_show(x)
vec_cast.hd_Dat.date <- function(x, to, ...) new_date(vec_data(x))
vec_cast.character.hd_Dat <- function(x, to, ...) Dat(x)

as_Dat <- function(x) {
  vec_cast(x, new_Dat())
}


