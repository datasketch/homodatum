
new_Pct <- function(x = double(), skip_stats = FALSE){
  vctrs::vec_assert(x, double())

  stats <- NULL
  if(length(x) == 0)
    skip_stats <- TRUE
  if(!skip_stats){
    stats <- list(
      n_unique = length(unique(x)),
      n_na = sum(is.na(x)),
      pct_na = sum(is.na(x))/length(x),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  }

  vctrs::new_vctr(x, class = "hd_Pct")
}

Pct <- function(x = double()) {

  # x <- tryCatch(vctrs::vec_cast(x, double()),
  x <- tryCatch(as.numeric(x),
                warning = function(w) {
                  x_no_na <- !is.na(x)
                  if (all(grepl("([^%]*%[^%]*[0-9]+)|([0-9]+[^%]*%.*)", x[x_no_na]))) {
                    x[x_no_na] <- gsub("\\,", ".", x[x_no_na])
                    x[x_no_na] <- regmatches(x[x_no_na], regexpr("[0-9]*\\.*[0-9]+", x[x_no_na]))
                    as.numeric(x, double())/100
                  } else{
                    w
                  }
                },
                error = function(e) {
                  x_no_na <- !is.na(x)
                  if (all(grepl("([^%]*%[^%]*[0-9]+)|([0-9]+[^%]*%.*)", x[x_no_na]))) {
                    x[x_no_na] <- gsub("\\,", ".", x[x_no_na])
                    x[x_no_na] <- regmatches(x[x_no_na], regexpr("[0-9]*\\.*[0-9]+", x[x_no_na]))
                    as.numeric(x, double())/100
                  } else{
                    e
                  }
                })
  new_Pct(x)
}

#' @export
is_Pct <- function(x) {
  inherits(x, "hd_Pct")
}

# Methods

## Format method

#' @export
format.hd_Pct <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x) * 100, 3))
  #out <- formatC(x)
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
  out
}

#' @export
vec_ptype_abbr.hd_Pct <- function(x, ...) {
  "Pct"
}

# Coercion
vec_ptype2.hd_Pct <- function(x, y, ...) UseMethod("vec_ptype2.hd_Pct", y)
vec_ptype2.hd_Pct.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A Pct combined with a Pct returns a Pct
vec_ptype2.hd_Pct.hd_Pct <- function(x, y, ...) new_Pct()
# Pct and double return double
vec_ptype2.hd_Pct.double <- function(x, y, ...) double()
vec_ptype2.double.hd_Pct <- function(x, y, ...) double()

# Casting


# vec_cast.hd_Pct <- function(x, to, ...) UseMethod("vec_cast.hd_Pct")
# vec_cast.hd_Pct.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Pct to Pct

#' @export
vec_cast.hd_Pct.hd_Pct <- function(x, to, ...) x

#' @export
vec_cast.hd_Pct.double <- function(x, to, ...) Pct(x)

#' @export
vec_cast.double.hd_Pct <- function(x, to, ...) vctrs::vec_data(x)
# Coerce Pct to character

#' @export
vec_cast.hd_Pct.character <- function(x, to, ...) Pct(as.numeric(x))

# vec_cast.character.hd_Pct <- function(x, to, ...) as.character(vctrs::vec_data(x))

#' @export
as.character.hd_Pct <- function(x) as.character(vec_data(x))


as_Pct <- function(x) {
  vctrs::vec_cast(x, new_Pct())
}


#' @export
Pct_get_stats <-  function(x){
  if(!is_Pct(x)) stop("x must be a Pct")
  attr(x, "stats")
}

