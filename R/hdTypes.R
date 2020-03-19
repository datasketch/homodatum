
new_hdType <- function(x = character()){
  vec_assert(x, character())
  if(!all(vec_data(x) %in% available_hdTypes(as_character = TRUE))){
    stop("hdType must be one of: ", paste(collapse = ", "))
  }
  new_vctr(x, class = "hdType")
}

hdType <- function(x = character()) {
  x <- vec_cast(x, character())
  new_hdType(x)
}

is_hdType <- function(x) {
  inherits(x, "hdType")
}

is_any_hdType <- function(x){
  sum(grepl("hd_", class(x))) > 0
}

which_hdType <- function(x){
  gsub("hd_","",class(x)[grep("hd_", class(x))])
}

# Methods

## Format method

format.hdType <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

vec_ptype_abbr.hdType <- function(x, ...) {
  "hdType"
}

# Coercion
vec_ptype2.hdType <- function(x, y, ...) UseMethod("vec_ptype2.hdType", y)
vec_ptype2.hdType.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A hdType combined with a hdType returns a hdType
vec_ptype2.hdType.hdType <- function(x, y, ...) new_hdType()
# # hdType and character return hdType
vec_ptype2.hdType.character <- function(x, y, ...) hdType()
vec_ptype2.character.hdType <- function(x, y, ...) hdType()

# Casting
vec_cast.vctrs_hdType <- function(x, to, ...) UseMethod("vec_cast.hdType")
vec_cast.vctrs_hdType.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce hdType to hdType
vec_cast.hdType.hdType <- function(x, to, ...) x
vec_cast.hdType.character <- function(x, to, ...) hdType(x)
vec_cast.character.hdType <- function(x, to, ...) hdType(x)

as_hdType <- function(x) {
  vec_cast(x, new_hdType())
}

