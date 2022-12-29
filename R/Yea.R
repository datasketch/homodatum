
new_Yea <- function(x = integer(), skip_stats = FALSE){
  vctrs::vec_assert(x, integer())
  stats <- NULL
  if(!skip_stats){
    stats <- list(
      is_sequence = all(diff(sort(x[!is.na(x)])) == 1),
      n_na = sum(is.na(x)),
      prop_na = sum(is.na(x))/length(x)
    )
  }
  vctrs::new_vctr(x, stats = stats, class = "hd_Yea")
}

#' @title Year Vectors
#' @description Creates objects of type "hd_Yea". hd_Yea objects contain a "stats" attribute, with the following information: NA values (n_na), percentage weight of NA values (prop_na) and if (TRUE or FALSE) the set of given values conform a sequence (is_sequence).
#'
#' @param x object to be created as Yea type
#'
#' @examples
#' x <- c(1800:1803, NA)
#' year_values <- Yea(x)
#' year_values
#' class(year_values)
#' attr(year_values, "stats")$is_sequence
#'
#' @export
Yea <- function(x = integer()) {
  x <- vctrs::vec_cast(x, integer())
  new_Yea(x)
}

#' @title Year Vectors
#' @description test for objects of type "hd_Yea"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hd_Yea or not.
#'
#' @examples
#' x <- c(1800:1803, NA)
#' year_values <- Yea(x)
#' is_Yea(year_values)
#'
#' @export
is_Yea <- function(x) {
  inherits(x, "hd_Yea")
}

# Methods

## Format method

#' @export
format.hd_Yea <- function(x, ...) {
  formatC(x)
}

#' @export
vec_ptype_abbr.hd_Yea <- function(x, ...) {
  "Yea"
}

# Coercion
# vec_ptype2.hd_Yea <- function(x, y, ...) UseMethod("vec_ptype2.hd_Yea", y)
# vec_ptype2.hd_Yea.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
#   vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
# }
# A Yea combined with a Yea returns a Yea

#' @export
vec_ptype2.hd_Yea.hd_Yea <- function(x, y, ...) new_Yea()

# Yea and integer return integer

#' @export
vec_ptype2.hd_Yea.integer <- function(x, y, ...) integer()

#' @export
vec_ptype2.integer.hd_Yea <- function(x, y, ...) integer()

# Casting
# vec_cast.hd_Yea <- function(x, to, ...) UseMethod("vec_cast.hc_Yea")
# vec_cast.hd_Yea.default <- function(x, to, ...) vec_default_cast(x, to)

# Coerce Yea to Yea

#' @export
vec_cast.hd_Yea.hd_Yea <- function(x, to, ...) x

#' @export
vec_cast.hd_Yea.integer <- function(x, to, ...) Yea(x)

#' @export
vec_cast.integer.hd_Yea <- function(x, to, ...) vctrs::vec_data(x)
# Coerce Yea to character
# vec_cast.hd_Yea.character <- function(x, to, ...) Yea(as.integer(x))
# vec_cast.character.hd_Yea <- function(x, to, ...) as.character(vctrs::vec_data(x))

#' @export
as.character.hd_Yea <- function(x) as.character(vec_data(x))


#' @export
as_Yea <- function(x) {
  vctrs::vec_cast(x, new_Yea())
}



Yea_get_stats <-  function(x){
  if(!is_Yea(x)) stop("x must be a Cat")
  attr(x, "stats")
}



