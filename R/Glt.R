
new_Glt <- function(x = double(), skip_stats = FALSE){
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
  vctrs::new_vctr(x, stats = stats, class = "hd_Glt")
}

#' @export
Glt <- function(x = double()) {
  x <- vctrs::vec_cast(x, double())
  if(any(x < -90 || x > 90, na.rm = TRUE))
    stop("Latitude must be between -90 and 90")
  new_Glt(x)
}

#' @export
is_Glt <- function(x) {
  inherits(x, "hd_Glt")
}

# Methods

## Format method

#' @export
format.hd_Glt <- function(x, ...) {
  formatC(x)
}

#' @export
vec_ptype_abbr.hd_Glt <- function(x, ...) {
  "Glt"
}

# Coercion
# vec_ptype2.hd_Glt <- function(x, y, ...) UseMethod("vec_ptype2.hd_Glt", y)
# vec_ptype2.hd_Glt.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
#   vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
# }
# A Glt combined with a Glt returns a Glt

#' @export
vec_ptype2.hd_Glt.hd_Glt <- function(x, y, ...) new_Glt()
# Glt and double return double

#' @export
vec_ptype2.hd_Glt.double <- function(x, y, ...) double()

#' @export
vec_ptype2.double.hd_Glt <- function(x, y, ...) double()

# Casting
# vec_cast.hd_Glt <- function(x, to, ...) UseMethod("vec_cast.hd_Glt")
# vec_cast.hd_Glt.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Glt to Glt

#' @export
vec_cast.hd_Glt.hd_Glt <- function(x, to, ...) x

#' @export
vec_cast.hd_Glt.double <- function(x, to, ...) Glt(x)

#' @export
vec_cast.double.hd_Glt <- function(x, to, ...) vctrs::vec_data(x)

# # Coerce Glt to character
# vec_cast.hd_Glt.character <- function(x, to, ...) Glt(as.numeric(x))
# vec_cast.character.hd_Glt <- function(x, to, ...) as.character(vctrs::vec_data(x))

#' @export
as.character.hd_Glt <- function(x) as.character(vec_data(x))



#' @export
as_Glt <- function(x) {
  vctrs::vec_cast(x, new_Glt())
}



#' @export
Glt_get_stats <-  function(x){
  if(!is_Glt(x)) stop("x must be a Cat")
  attr(x, "stats")
}



