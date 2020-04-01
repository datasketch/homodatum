
new_Img <- function(x = character()){
  vctrs::vec_assert(x, character())
  vctrs::new_vctr(x, class = "hd_Img")
}

Img <- function(x = character()) {
  x <- vctrs::vec_cast(x, character())
  new_Img(x)
}

is_Img <- function(x) {
  inherits(x, "hd_Img")
}

# Methods

## Format method

format.hd_Img <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x) * 100, 3))
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
  out
}

vec_ptype_abbr.hd_Img <- function(x, ...) {
  "Img"
}

# Coercion
vec_ptype2.hd_Img <- function(x, y, ...) UseMethod("vec_ptype2.hd_Img", y)
vec_ptype2.hd_Img.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A Img combined with a Img returns a Img
vec_ptype2.hd_Img.hd_Img <- function(x, y, ...) new_Img()
# Img and character return double
vec_ptype2.hd_Img.character <- function(x, y, ...) character()
vec_ptype2.character.hd_Img <- function(x, y, ...) character()

# Casting
vec_cast.vctrs_percent <- function(x, to, ...) UseMethod("vec_cast.hc_Img")
vec_cast.vctrs_percent.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Img to Img
vec_cast.hd_Img.hd_Img <- function(x, to, ...) x
vec_cast.hd_Img.character <- function(x, to, ...) percent(x)
vec_cast.character.hd_Img <- function(x, to, ...) vctrs::vec_data(x)

as_Img <- function(x) {
  vctrs::vec_cast(x, new_Img())
}


