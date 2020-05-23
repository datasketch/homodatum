
new_Txt <- function(x = character(),
                    skip_stats = FALSE){
  vctrs::vec_assert(x, character())
  stats <- NULL

  if(!skip_stats){
    stats <- list(
      chars = nchar(x),
      words = nwords(x)
    )
  }
  vctrs::new_vctr(x, stats = stats, class = "hd_Txt")
}

Txt <- function(x = character(), skip_stats = FALSE) {
  x <- vctrs::vec_cast(x, character())
  new_Txt(x, skip_stats = skip_stats)
}

is_Txt <- function(x) {
  inherits(x, "hd_Txt")
}

# Methods

## Format method

#' @export
format.hd_Txt <- function(x, ...) {
  more <- "(…)"
  sprintf(fmt = "%s(…)", substr(x, 1, 20))
}

#' @export
vec_ptype_abbr.hd_Txt <- function(x, ...) {
  "Txt"
}


nwords <- function(x){
  no_na <- x
  no_na[is.na(x)] <- ""
  lengths(strsplit(no_na, "\\W+"))
}

# Coercion
# vec_ptype2.hd_Txt <- function(x, y, ...) UseMethod("vec_ptype2.hd_Txt", y)
# vec_ptype2.hd_Txt.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
#   vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
# }
# A Txt combined with a Txt returns a Txt

#' @export
vec_ptype2.hd_Txt.hd_Txt <- function(x, y, ...) new_Txt()

# Txt and character return double

#' @export
vec_ptype2.hd_Txt.character <- function(x, y, ...) character()

#' @export
vec_ptype2.character.hd_Txt <- function(x, y, ...) character()

# Casting
# vec_cast.vctrs_Txt <- function(x, to, ...) UseMethod("vec_cast.hc_Txt")
# vec_cast.vctrs_Txt.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Txt to Txt

#' @export
vec_cast.hd_Txt.hd_Txt <- function(x, to, ...) x

#' @export
vec_cast.hd_Txt.character <- function(x, to, ...) Txt(x)

#' @export
vec_cast.character.hd_Txt <- function(x, to, ...) vctrs::vec_data(x)

as_Txt <- function(x) {
  vctrs::vec_cast(x, new_Txt())
}




Txt_get_stats <-  function(x){
  if(!is_Txt(x)) stop("x must be a Txt")
  attr(x, "stats")
}

