
new_Num <- function(x = double(), skip_stats = FALSE){
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
  vctrs::new_vctr(x, stats = stats, class = "hd_Num")
}

#' @export
Num <- function(x = double()) {
  x <- vctrs::vec_cast(x, double())
  new_Num(x)
}

#' @export
is_Num <- function(x) {
  inherits(x, "hd_Num")
}

# Methods

## Format method

format.hd_Num <- function(x, ...) {
  formatC(x)
}

vec_ptype_abbr.hd_Num <- function(x, ...) {
  "Num"
}

# Coercion
vec_ptype2.hd_Num <- function(x, y, ...) UseMethod("vec_ptype2.hd_Num", y)
vec_ptype2.hd_Num.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A Num combined with a Num returns a Num
vec_ptype2.hd_Num.hd_Num <- function(x, y, ...) new_Num()
# Num and double return double
vec_ptype2.hd_Num.double <- function(x, y, ...) double()
vec_ptype2.double.hd_Num <- function(x, y, ...) double()

# Casting
vec_cast.hd_Num <- function(x, to, ...) UseMethod("vec_cast.hd_Num")
vec_cast.hd_Num.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Num to Num
vec_cast.hd_Num.hd_Num <- function(x, to, ...) x
vec_cast.hd_Num.double <- function(x, to, ...) Num(x)
vec_cast.double.hd_Num <- function(x, to, ...) vctrs::vec_data(x)
# Coerce Num to character
vec_cast.hd_Num.character <- function(x, to, ...) Num(as.numeric(x))
vec_cast.character.hd_Num <- function(x, to, ...) as.character(vctrs::vec_data(x))


#' @export
as_Num <- function(x) {
  vctrs::vec_cast(x, new_Num())
}



#' @export
Num_get_stats <-  function(x){
  if(!is_Num(x)) stop("x must be a Cat")
  attr(x, "stats")
}



