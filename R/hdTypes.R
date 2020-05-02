
new_hdType <- function(x = character()){
  vctrs::vec_assert(x, character())
  if(!all(vctrs::vec_data(x) %in% available_hdTypes(as_character = TRUE))){
    stop("hdType must be one of: ", paste(collapse = ", "))
  }
  vctrs::new_vctr(x, class = "hdType")
}

hdType <- function(x = character()) {
  x <- vctrs::vec_cast(x, character())
  new_hdType(x)
}

#' @export
is_hdType <- function(x) {
  inherits(x, "hdType")
}

#' @export
is_any_hdType <- function(x){
  sum(grepl("hd_", class(x))) > 0
}

#' @export
which_hdType <- function(x){
  gsub("hd_","",class(x)[grep("hd_", class(x))])
}

#' @export
is_hdTibble <- function(d){
  all(purrr::map_lgl(d, is_any_hdType))
}

#' @export
get_hdTibble_hdTypes <- function(d){
  hdType(purrr::map_chr(d, which_hdType))
}

# Methods

## Format method

#' @export
format.hdType <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hdType <- function(x, ...) {
  "hdType"
}

# Coercion

#' @method vec_ptype2 hdType
#' @export
vec_ptype2.hdType <- function(x, y, ...) UseMethod("vec_ptype2.hdType", y)

#' @method vec_ptype2.hdType default
#' @export
vec_ptype2.hdType.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# A hdType combined with a hdType returns a hdType

#' @method vec_ptype2.hdType hdType
#' @export
vec_ptype2.hdType.hdType <- function(x, y, ...) new_hdType()

# # hdType and character return hdType

#' @method vec_ptype2.hdType character
#' @export
vec_ptype2.hdType.character <- function(x, y, ...) character()

#' @method vec_ptype2.character hdType
#' @export
vec_ptype2.character.hdType <- function(x, y, ...) character()

# Casting

#' @method vec_cast hdType
#' @export
vec_cast.hdType <- function(x, to, ...) UseMethod("vec_cast.hdType")

#' @method vec_cast.hdType default
#' @export
vec_cast.hdType.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce hdType to hdType

#' @method vec_cast.hdType hdType
#' @export
vec_cast.hdType.hdType <- function(x, to, ...) x

#' @method vec_cast.hdType character
#' @export
vec_cast.hdType.character <- function(x, to, ...) hdType(x)

#' @method vec_cast.character hdType
#' @export
vec_cast.character.hdType <- function(x, to, ...) vctrs::vec_data(x)

#' @export
as_hdType <- function(x) {
  vctrs::vec_cast(x, new_hdType())
}


