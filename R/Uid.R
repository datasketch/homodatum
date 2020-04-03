
new_Uid <- function(x = character()){
  vctrs::vec_assert(x, character())
  vctrs::new_vctr(x, class = "hd_Uid")
}

Uid <- function(x = character()) {
  x <- vctrs::vec_cast(x, character())
  new_Uid(x)
}

is_Uid <- function(x) {
  inherits(x, "hd_Uid")
}

# Methods

## Format method

#' @export
format.hd_Uid <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x) * 100, 3))
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
  out
}

#' @export
vec_ptype_abbr.hd_Uid <- function(x, ...) {
  "Uid"
}

# Coercion
vec_ptype2.hd_Uid <- function(x, y, ...) UseMethod("vec_ptype2.hd_Uid", y)
vec_ptype2.hd_Uid.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A Uid combined with a Uid returns a Uid
vec_ptype2.hd_Uid.hd_Uid <- function(x, y, ...) new_Uid()
# Uid and character return double
vec_ptype2.hd_Uid.character <- function(x, y, ...) character()
vec_ptype2.character.hd_Uid <- function(x, y, ...) character()

# Casting
vec_cast.vctrs_percent <- function(x, to, ...) UseMethod("vec_cast.hc_Uid")
vec_cast.vctrs_percent.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Uid to Uid
vec_cast.hd_Uid.hd_Uid <- function(x, to, ...) x
vec_cast.hd_Uid.character <- function(x, to, ...) percent(x)
vec_cast.character.hd_Uid <- function(x, to, ...) vctrs::vec_data(x)

as_Uid <- function(x) {
  vctrs::vec_cast(x, new_Uid())
}


