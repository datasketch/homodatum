
new_Bin <- function(x = character(), categories = NULL,
                    skip_stats = FALSE){
  vctrs::vec_assert(x, character())
  categories <- categories %||% unique(x[!is.na(x)])
  if(length(categories) > 2){
    stop("Bin must have at most 2 categories")
  }
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
           stats = stats, class = "hd_Bin")
}

#' @export
Bin <- function(x = character(), categories = NULL, skip_stats = FALSE) {
  # x <- vctrs::vec_cast(x, character())
  x <- as.character(x)
  new_Bin(x, categories = categories, skip_stats = skip_stats)
}

#' @export
is_Bin <- function(x) {
  inherits(x, "hd_Bin")
}

# Methods

## Format method

#' @export
format.hd_Bin <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hd_Bin <- function(x, ...) {
  "Bin"
}

# Coercion


#' @rdname vctrs-compat
#' @method vec_ptype2 hd_Bin
#' @export
#' @export vec_ptype2.hd_Bin
vec_ptype2.hd_Bin <- function(x, y, ...) UseMethod("vec_ptype2.hd_Bin", y)

#' @method vec_ptype2.hd_Bin default
#' @export
vec_ptype2.hd_Bin.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# A Bin combined with a Bin returns a Bin

#' @method vec_ptype2.hd_Bin hd_Bin
#' @export
vec_ptype2.hd_Bin.hd_Bin <- function(x, y, ...) new_Bin()

# Bin and character return character

#' @method vec_ptype2.hd_Bin character
#' @export
vec_ptype2.hd_Bin.character <- function(x, y, ...) character()

#' @method vec_ptype2.character hd_Bin
#' @export
vec_ptype2.character.hd_Bin <- function(x, y, ...) character()

# Casting

#' @rdname vctrs-compat
#' @method vec_cast hd_Bin
#' @export
#' @export vec_cast.hd_Bin
vec_cast.hd_Bin <- function(x, to, ...) UseMethod("vec_cast.hd_Bin")

#' @method vec_cast.hd_Bin default
#' @export
vec_cast.hd_Bin.default <- function(x, to, ...) vec_default_cast(x, to)


# Coerce Bin to Bin: TODO need to make sure Cats equivalence
# Ex. Yes/No  yes/no -> Yes/No

#' @method vec_cast.hd_Bin hd_Bin
#' @export
vec_cast.hd_Bin.hd_Bin <- function(x, to, ...) x

#' @method vec_cast.hd_Bin character
#' @export
vec_cast.hd_Bin.character <- function(x, to, ...) Bin(x)

#' @method vec_cast.character hd_Bin
#' @export
vec_cast.character.hd_Bin <- function(x, to, ...) vctrs::vec_data(x)

#' @export
as_Bin <- function(x) {
  vctrs::vec_cast(x, new_Bin())
}


#' @export
Bin_get_categories <- function(x){
  if(!is_Bin(x)) stop("x must be a Bin")
  attr(x, "categories")
}

#' @export
Bin_get_n_categories <- function(x){
  if(!is_Bin(x)) stop("x must be a Bin")
  attr(x, "n_categories")
}

#' @export
Bin_get_stats <-  function(x){
  if(!is_Bin(x)) stop("x must be a Bin")
  attr(x, "stats")
}

