
new_hdType <- function(x = character()){
  vctrs::vec_assert(x, character())
  av <- available_hdTypes(as_character = TRUE)
  if(!all(vctrs::vec_data(x) %in% av)){
    stop(x, "hdType must be one of: ", paste(av, collapse = ", "))
  }
  vctrs::new_vctr(x, class = "hdType")
}

hdType <- function(x = character()) {
  x <- vctrs::vec_cast(x, character())
  new_hdType(x)
}



# hdType ------------------------------------------------------------------

#' @title hdType Vectors
#'
#' @description test for objects of type "hdType"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hdType or not.
#'
#' @examples
#'
#' some_value <- hdType("Cat")
#' is_hdType(some_value)
#'
#' @export
is_hdType <- function(x) {
  inherits(x, "hdType")
}


#' @title hdType Vectors
#'
#' @description test for objects of type "hdType"
#'
#' @param x hdType object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is one of the [available_hdTypes()] or not.
#'
#' @examples
#'
#' some_cat_value <- Cat("value1")
#' is_any_hdType(some_cat_value)
#'
#' @export
is_any_hdType <- function(x){
  sum(grepl("hd_", class(x))) > 0
}



#' @title hdType Vectors
#'
#' @description Detect which hdType is the value.
#'
#' @param x hdType object to be coerced or tested
#'
#' @return returns the name of the hdType value. You can see the valid hdTypes with [available_hdTypes()]
#'
#' @examples
#'
#' some_cat_value <- Cat("value1")
#' which_hdType(some_cat_value)
#'
#' @export
which_hdType <- function(x){
  if(is_any_hdType(x)){
    gsub("hd_","",class(x)[grep("hd_", class(x))])
  } else {
    cat("The value is not a valid hdType.")
  }
}


# hdType ------------------------------------------------------------------

#' @title hdType Vectors
#'
#' @description coerces its argument to a hdType. It is an abbreviated form of hdType.
#'
#' @param x object to be coerced
#'
#' @return attempts to coerce its argument to hdType type
#'
#' @examples
#'
#' some_chr_value <- "Cat"
#' class(some_chr_value)
#'
#' some_hdt_value <- as_hdType(some_chr_value)
#' class(some_hdt_value)
#'
#' @export
as_hdType <- function(x) {
  if(any(class(x) == "factor")){
    x <- as.character(x)
  }
  vctrs::vec_cast(x, new_hdType())
}




# Methods

## Format method

#' @export
format.hdType <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hdType <- function(x, ...) {
  "hdType"
}

# Coercion

#' @method vec_ptype2 hdType
#' @export
vec_ptype2.hdType <- function(x, y, ...) UseMethod("vec_ptype2.hdType", y)

#' @method vec_ptype2.hdType default
#' @export
vec_ptype2.hdType.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# A hdType combined with a hdType returns a hdType

#' @method vec_ptype2.hdType hdType
#' @export
vec_ptype2.hdType.hdType <- function(x, y, ...) new_hdType()

# # hdType and character return hdType

#' @method vec_ptype2.hdType character
#' @export
vec_ptype2.hdType.character <- function(x, y, ...) character()

#' @method vec_ptype2.character hdType
#' @export
vec_ptype2.character.hdType <- function(x, y, ...) character()

# Casting

#' @method vec_cast hdType
#' @export
vec_cast.hdType <- function(x, to, ...) UseMethod("vec_cast.hdType")

#' @method vec_cast.hdType default
#' @export
vec_cast.hdType.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce hdType to hdType

#' @method vec_cast.hdType hdType
#' @export
vec_cast.hdType.hdType <- function(x, to, ...) x

#' @method vec_cast.hdType character
#' @export
vec_cast.hdType.character <- function(x, to, ...) hdType(x)

#' @method vec_cast.character hdType
#' @export
vec_cast.character.hdType <- function(x, to, ...) vctrs::vec_data(x)


#' @method as.character hdType
#' @export
as.character.hdType <- function(x) as.character(vctrs::vec_data(x))



