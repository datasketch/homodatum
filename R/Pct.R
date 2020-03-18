
new_Pct <- function(x = double()){
  vec_assert(x, double())
  new_vctr(x, class = "hd_Pct")
}

Pct <- function(x = double()) {

  x <- tryCatch(vec_cast(x, double()),
                error = function(e) {
                  if(all(grepl("%$",x))){
                    vec_cast(gsub("%","", x), double())/100
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

format.hd_Pct <- function(x, ...) {
  out <- formatC(signif(vec_data(x) * 100, 3))
  #out <- formatC(x)
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
  out
}

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
vec_cast.vctrs_percent <- function(x, to, ...) UseMethod("vec_cast.hc_Pct")
vec_cast.vctrs_percent.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce Pct to Pct
vec_cast.hd_Pct.hd_Pct <- function(x, to, ...) x
vec_cast.hd_Pct.double <- function(x, to, ...) percent(x)
vec_cast.double.hd_Pct <- function(x, to, ...) vec_data(x)

as_Pct <- function(x) {
  vec_cast(x, new_Pct())
}


