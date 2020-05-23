
new_Seq <- function(x = character(), categories = NULL,
                    order = NULL,
                    skip_stats = FALSE){
  vctrs::vec_assert(x, character())
  categories <- categories %||% unique(x[!is.na(x)])
  nms <- names(x)
  if(is.null(order)){
    order <- categories[1:length(categories)]
  }
  stats <- NULL
  if(!skip_stats){
    stats <- table(x,useNA = "always") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(dist = n/sum(n), names = c(nms, NA)) %>%
      dplyr::rename(category = x)
  }
  vctrs::new_vctr(x, categories = categories, order = order,
           n_categories = length(categories),
           stats = stats, class = "hd_Seq")
}

Seq <- function(x = character(), order = NULL,
                categories = NULL, skip_stats = FALSE) {
  # x <- vctrs::vec_cast(x, character())
  x <- as.character(x)
  new_Seq(x, order = order, categories = categories, skip_stats = skip_stats)
}

is_Seq <- function(x) {
  inherits(x, "hd_Seq")
}

# Methods

## Format method

#' @export
format.hd_Seq <- function(x, ...) {
  order <- paste0(Seq_get_order(x), collapse = "<")
  cat(
    sprintf(fmt = "%s", x),"\n",
    #sprintf(fmt = "%s",order),
    order)
}

#' @export
vec_ptype_abbr.hd_Seq <- function(x, ...) {
  "Seq"
}

# Coercion
# vec_ptype2.hd_Seq <- function(x, y, ...) UseMethod("vec_ptype2.hd_Seq", y)
# vec_ptype2.hd_Seq.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
#   vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
# }
# A Seq combined with a Seq returns a Seq

#' @export
vec_ptype2.hd_Seq.hd_Seq <- function(x, y, ...) new_Seq()

# Seq and character return double

#' @export
vec_ptype2.hd_Seq.character <- function(x, y, ...) character()

#' @export
vec_ptype2.character.hd_Seq <- function(x, y, ...) character()

# Casting
# vec_cast.vctrs_Seq <- function(x, to, ...) UseMethod("vec_cast.hd_Seq")
# vec_cast.vctrs_Seq.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Seq to Seq

#' @export
vec_cast.hd_Seq.hd_Seq <- function(x, to, ...) x

#' @export
vec_cast.hd_Seq.character <- function(x, to, ...) Seq(x)

#' @export
vec_cast.character.hd_Seq <- function(x, to, ...) vctrs::vec_data(x)

#' @export
as.character.hd_Seq <- function(x) as.character(vec_data(x))



as_Seq <- function(x) {
  vctrs::vec_cast(x, new_Seq())
}


Seq_get_categories <- function(x){
  if(!is_Seq(x)) stop("x must be a Seq")
  attr(x, "categories")
}

Seq_get_order <- function(x){
  if(!is_Seq(x)) stop("x must be a Seq")
  attr(x, "order")
}


Seq_get_n_categories <- function(x){
  if(!is_Seq(x)) stop("x must be a Seq")
  attr(x, "n_categories")
}

Seq_get_stats <-  function(x){
  if(!is_Seq(x)) stop("x must be a Seq")
  attr(x, "stats")
}

