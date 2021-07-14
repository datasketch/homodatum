
new_Gcd <- function(x = character(), categories = NULL,
                    skip_stats = FALSE){
  vctrs::vec_assert(x, character())
  categories <- categories %||% unique(x[!is.na(x)])
  nms <- names(x)
  stats <- NULL
  if(!skip_stats){
    stats <- table(x,useNA = "always") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(dist = n/sum(n), names = c(nms, NA)) %>%
      dplyr::rename(category = x)
  }
  vctrs::new_vctr(x, categories = categories,
                  n_categories = length(categories),
                  stats = stats, class = "hd_Gcd")
}


#' @export
Gcd <- function(x = character(), categories = NULL, skip_stats = FALSE) {
  # x <- vctrs::vec_cast(x, character())
  x <- as.character(x)
  new_Gcd(x, categories = categories, skip_stats = skip_stats)
}

#' @export
is_Gcd <- function(x) {
  inherits(x, "hd_Gcd")
}

# Methods

## Format method

#' @export
format.hd_Gcd <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hd_Gcd <- function(x, ...) {
  "Gcd"
}

# Coercion

#' @rdname vctrs-compat
#' @method vec_ptype2 hd_Gcd
#' @export
#' @export vec_ptype2.hd_Gcd
vec_ptype2.hd_Gcd <- function(x, y, ...) UseMethod("vec_ptype2.hd_Gcd", y)

#' @method vec_ptype2.hd_Gcd default
#' @export
vec_ptype2.hd_Gcd.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}


# A Gcd combined with a Gcd returns a Gcd

#' @method vec_ptype2.hd_Gcd hd_Gcd
#' @export
vec_ptype2.hd_Gcd.hd_Gcd <- function(x, y, ...) new_Gcd()

# Gcd and character return double

#' @method vec_ptype2.hd_Gcd character
#' @export
vec_ptype2.hd_Gcd.character <- function(x, y, ...) character()

#' @method vec_ptype2.character hd_Gcd
#' @export
vec_ptype2.character.hd_Gcd <- function(x, y, ...) character()

# Casting

#' @rdname vctrs-compat
#' @method vec_cast hd_Gcd
#' @export
#' @export vec_cast.hd_Gcd
vec_cast.hd_Gcd <- function(x, to, ...) UseMethod("vec_cast.hd_Gcd")

#' @method vec_cast.hd_Gcd default
#' @export
vec_cast.hd_Gcd.default <- function(x, to, ...) vec_default_cast(x, to)

# Coerce Gcd to Gcd

#' @method vec_cast.hd_Gcd hd_Gcd
#' @export
vec_cast.hd_Gcd.hd_Gcd <- function(x, to, ...) x

#' @method vec_cast.hd_Gcd character
#' @export
vec_cast.hd_Gcd.character <- function(x, to, ...) Gcd(x)

#' @method vec_cast.character hd_Gcd
#' @export
vec_cast.character.hd_Gcd <- function(x, to, ...) vctrs::vec_data(x)

#' @export
as_Gcd <- function(x) {
  x <- as.character(x)
  vctrs::vec_cast(x, new_Gcd())
}


#' @export
Gcd_get_categories <- function(x){
  if(!is_Gcd(x)) stop("x must be a Gcd")
  attr(x, "categories")
}

#' @export
Gcd_get_n_categories <- function(x){
  if(!is_Gcd(x)) stop("x must be a Gcd")
  attr(x, "n_categories")
}

#' @export
Gcd_get_stats <-  function(x){
  if(!is_Gcd(x)) stop("x must be a Gcd")
  attr(x, "stats")
}

