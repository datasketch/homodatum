
new_Yea <- function(x = integer(), skip_stats = FALSE){
  vec_assert(x, integer())
  stats <- NULL
  if(!skip_stats){
    stats <- list(
      is_sequence = all(diff(sort(x[!is.na(x)])) == 1),
      n_na = sum(is.na(x)),
      prop_na = sum(is.na(x))/length(x)
    )
  }
  new_vctr(x, stats = stats, class = "hd_Yea")
}

Yea <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_Yea(x)
}

is_Yea <- function(x) {
  inherits(x, "hd_Yea")
}

# Methods

## Format method

format.hd_Yea <- function(x, ...) {
  formatC(x)
}

vec_ptype_abbr.hd_Yea <- function(x, ...) {
  "Yea"
}

# Coercion
vec_ptype2.hd_Yea <- function(x, y, ...) UseMethod("vec_ptype2.hd_Yea", y)
vec_ptype2.hd_Yea.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A Yea combined with a Yea returns a Yea
vec_ptype2.hd_Yea.hd_Yea <- function(x, y, ...) new_Yea()
# Yea and integer return integer
vec_ptype2.hd_Yea.integer <- function(x, y, ...) integer()
vec_ptype2.integer.hd_Yea <- function(x, y, ...) integer()

# Casting
vec_cast.vctrs_percent <- function(x, to, ...) UseMethod("vec_cast.hc_Yea")
vec_cast.vctrs_percent.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Yea to Yea
vec_cast.hd_Yea.hd_Yea <- function(x, to, ...) x
vec_cast.hd_Yea.integer <- function(x, to, ...) Yea(x)
vec_cast.integer.hd_Yea <- function(x, to, ...) vec_data(x)

as_Yea <- function(x) {
  vec_cast(x, new_Yea())
}



Yea_get_stats <-  function(x){
  if(!is_Yea(x)) stop("x must be a Cat")
  attr(x, "stats")
}


