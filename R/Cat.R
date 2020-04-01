
new_Cat <- function(x = character(), categories = NULL,
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
           stats = stats, class = "hd_Cat")
}


#' @export
Cat <- function(x = character(), categories = NULL, skip_stats = FALSE) {
  # x <- vctrs::vec_cast(x, character())
  x <- as.character(x)
  new_Cat(x, categories = categories, skip_stats = skip_stats)
}

#' @export
is_Cat <- function(x) {
  inherits(x, "hd_Cat")
}

# Methods

## Format method

format.hd_Cat <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

vec_ptype_abbr.hd_Cat <- function(x, ...) {
  "Cat"
}

# Coercion
vec_ptype2.hd_Cat <- function(x, y, ...) UseMethod("vec_ptype2.hd_Cat", y)
vec_ptype2.hd_Cat.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A Cat combined with a Cat returns a Cat
vec_ptype2.hd_Cat.hd_Cat <- function(x, y, ...) new_Cat()
# Cat and character return double
vec_ptype2.hd_Cat.character <- function(x, y, ...) character()
vec_ptype2.character.hd_Cat <- function(x, y, ...) character()

# Casting
vec_cast.vctrs_Cat <- function(x, to, ...) UseMethod("vec_cast.hd_Cat")
vec_cast.vctrs_Cat.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Cat to Cat
vec_cast.hd_Cat.hd_Cat <- function(x, to, ...) x
vec_cast.hd_Cat.character <- function(x, to, ...) Cat(x)
vec_cast.character.hd_Cat <- function(x, to, ...) vctrs::vec_data(x)

#' @export
as_Cat <- function(x) {
  vctrs::vec_cast(x, new_Cat())
}


#' @export
Cat_get_categories <- function(x){
  if(!is_Cat(x)) stop("x must be a Cat")
  attr(x, "categories")
}

#' @export
Cat_get_n_categories <- function(x){
  if(!is_Cat(x)) stop("x must be a Cat")
  attr(x, "n_categories")
}

#' @export
Cat_get_stats <-  function(x){
  if(!is_Cat(x)) stop("x must be a Cat")
  attr(x, "stats")
}

