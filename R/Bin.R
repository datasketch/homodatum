
new_Bin <- function(x = character(), categories = NULL,
                    skip_stats = FALSE){
  vec_assert(x, character())
  categories <- categories %||% unique(x[!is.na(x)])
  if(length(categories) > 2){
    stop("Bin must have at most 2 categories")
  }
  nms <- names(x)
  stats <- NULL
  if(!skip_stats){
    stats <- table(x,useNA = "always") %>%
      as_tibble() %>%
      mutate(dist = n/sum(n), names = c(nms, NA)) %>%
      rename(category = x)
  }
  new_vctr(x, categories = categories,
           n_categories = length(categories),
           stats = stats, class = "hd_Bin")
}

Bin <- function(x = character(), categories = NULL, skip_stats = FALSE) {
  x <- vec_cast(x, character())
  new_Bin(x, categories = categories, skip_stats = skip_stats)
}

is_Bin <- function(x) {
  inherits(x, "hd_Bin")
}

# Methods

## Format method

format.hd_Bin <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

vec_ptype_abbr.hd_Bin <- function(x, ...) {
  "Bin"
}

# Coercion
vec_ptype2.hd_Bin <- function(x, y, ...) UseMethod("vec_ptype2.hd_Bin", y)
vec_ptype2.hd_Bin.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A Bin combined with a Bin returns a Bin
vec_ptype2.hd_Bin.hd_Bin <- function(x, y, ...) new_Bin()
# Bin and character return double
vec_ptype2.hd_Bin.character <- function(x, y, ...) character()
vec_ptype2.character.hd_Bin <- function(x, y, ...) character()

# Casting
vec_cast.vctrs_Bin <- function(x, to, ...) UseMethod("vec_cast.hd_Bin")
vec_cast.vctrs_Bin.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Bin to Bin
vec_cast.hd_Bin.hd_Bin <- function(x, to, ...) x
vec_cast.hd_Bin.character <- function(x, to, ...) Bin(x)
vec_cast.character.hd_Bin <- function(x, to, ...) vec_data(x)

as_Bin <- function(x) {
  vec_cast(x, new_Bin())
}


Bin_get_categories <- function(x){
  if(!is_Bin(x)) stop("x must be a Bin")
  attr(x, "categories")
}

Bin_get_n_categories <- function(x){
  if(!is_Bin(x)) stop("x must be a Bin")
  attr(x, "n_categories")
}

Bin_get_stats <-  function(x){
  if(!is_Bin(x)) stop("x must be a Bin")
  attr(x, "stats")
}

