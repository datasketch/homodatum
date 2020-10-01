
new_Img <- function(x = character()){
  vctrs::vec_assert(x, character())
  formats <- tools::file_ext(x)
  if(!all(na.omit(formats) %in% c("png", "PNG", "jpg", "JPG", "jpeg", "JPEG", "svg", "SVG")))
    stop("Unknown image formats")
  format <- paste0(na.omit(unique(formats)), collapse = "|")
  if(nchar(format) == 0 && !all(is.na(x))) stop("Files with no extension")
  vctrs::new_vctr(x, format = format, class = "hd_Img")
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

#' @export
format.hd_Img <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hd_Img <- function(x, ...) {
  "Img"
}

# Coercion

# Coercion

#' @rdname vctrs-compat
#' @method vec_ptype2 hd_Img
#' @export
#' @export vec_ptype2.hd_Img
vec_ptype2.hd_Img <- function(x, y, ...) UseMethod("vec_ptype2.hd_Img", y)

#' @method vec_ptype2.hd_Img default
#' @export
vec_ptype2.hd_Img.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}


# A Img combined with a Img returns a Img

#' @method vec_ptype2.hd_Img hd_Img
#' @export
vec_ptype2.hd_Img.hd_Img <- function(x, y, ...) Img()

# Img and character return character

#' @method vec_ptype2.hd_Img character
#' @export
vec_ptype2.hd_Img.character <- function(x, y, ...) character()

#' @method vec_ptype2.character hd_Img
#' @export
vec_ptype2.character.hd_Img <- function(x, y, ...) character()

# Casting

#' @rdname vctrs-compat
#' @method vec_cast hd_Img
#' @export
#' @export vec_cast.hd_Img
vec_cast.hd_Img <- function(x, to, ...) UseMethod("vec_cast.hd_Img")

#' @method vec_cast.hd_Img default
#' @export
vec_cast.hd_Img.default <- function(x, to, ...) vec_default_cast(x, to)

# Coerce Img to Img

#' @method vec_cast.hd_Img hd_Img
#' @export
vec_cast.hd_Img.hd_Img <- function(x, to, ...) x

#' @method vec_cast.hd_Img character
#' @export
vec_cast.hd_Img.character <- function(x, to, ...) Img(x)

#' @method as.character hd_Img
#' @export
as.character.hd_Img <- function(x) vctrs::vec_data(x)


#' @export
as_Img <- function(x) {
  vctrs::vec_cast(x, new_Img())
}


has_img_ext <- function(x){

  all()
}

