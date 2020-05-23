
new_Gln <- function(x = double(), skip_stats = FALSE){
  vctrs::vec_assert(x, double())
  stats <- NULL
  if(length(x) == 0)
    skip_stats <- TRUE
  if(!skip_stats){
    stats <- list(
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  }
  vctrs::new_vctr(x, stats = stats, class = "hd_Gln")
}

#' @export
Gln <- function(x = double()) {
  x <- vctrs::vec_cast(x, double())
  if(any(x < -180 || x > 180, na.rm = TRUE))
    stop("Latitude must be between -180 and 180")

  new_Gln(x)
}

#' @export
is_Gln <- function(x) {
  inherits(x, "hd_Gln")
}

# Methods

## Format method

#' @export
format.hd_Gln <- function(x, ...) {
  formatC(x)
}

#' @export
vec_ptype_abbr.hd_Gln <- function(x, ...) {
  "Gln"
}

# # Coercion
# vec_ptype2.hd_Gln <- function(x, y, ...) UseMethod("vec_ptype2.hd_Gln", y)
# vec_ptype2.hd_Gln.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
#   vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
# }
# A Gln combined with a Gln returns a Gln

#' @export
vec_ptype2.hd_Gln.hd_Gln <- function(x, y, ...) new_Gln()

# Gln and double return double

#' @export
vec_ptype2.hd_Gln.double <- function(x, y, ...) double()

#' @export
vec_ptype2.double.hd_Gln <- function(x, y, ...) double()

# Casting
# vec_cast.hd_Gln <- function(x, to, ...) UseMethod("vec_cast.hd_Gln")
# vec_cast.hd_Gln.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Gln to Gln

#' @export
vec_cast.hd_Gln.hd_Gln <- function(x, to, ...) x

#' @export
vec_cast.hd_Gln.double <- function(x, to, ...) Gln(x)

#' @export
vec_cast.double.hd_Gln <- function(x, to, ...) vctrs::vec_data(x)

# Coerce Gln to character
vec_cast.hd_Gln.character <- function(x, to, ...) Gln(as.numeric(x))
# vec_cast.character.hd_Gln <- function(x, to, ...) as.character(vctrs::vec_data(x))


#' @export
as_Gln <- function(x) {
  vctrs::vec_cast(x, new_Gln())
}

#' @export
as.character.hd_Gln <- function(x) as.character(vec_data(x))


#' @export
Gln_get_stats <-  function(x){
  if(!is_Gln(x)) stop("x must be a Cat")
  attr(x, "stats")
}



