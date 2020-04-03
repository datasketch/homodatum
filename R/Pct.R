
new_Pct <- function(x = double()){
  vctrs::vec_assert(x, double())
  vctrs::new_vctr(x, class = "hd_Pct")
}

Pct <- function(x = double()) {

  x <- tryCatch(vctrs::vec_cast(x, double()),
                error = function(e) {
                  x_no_na <- x[!is.na(x)]
                  if(all(grepl("%$",x_no_na))){
                    vctrs::vec_cast(gsub("%","", x), double())/100
                  }else{
                    e
                  }
                })
  new_Pct(x)
}

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
vec_cast.hd_Pct <- function(x, to, ...) UseMethod("vec_cast.hd_Pct")
vec_cast.hd_Pct.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Pct to Pct
vec_cast.hd_Pct.hd_Pct <- function(x, to, ...) x
vec_cast.hd_Pct.double <- function(x, to, ...) Pct(x)
vec_cast.double.hd_Pct <- function(x, to, ...) vctrs::vec_data(x)
# Coerce Pct to character
# vec_cast.hd_Pct.character <- function(x, to, ...) Pct(as.numeric(x))
vec_cast.character.hd_Pct <- function(x, to, ...) as.character(vctrs::vec_data(x))


as_Pct <- function(x) {
  vctrs::vec_cast(x, new_Pct())
}


