
new_frType <- function(x = character()){
  vctrs::vec_assert(x, character())
  if(length(x)>1){
    hdTypes <- lapply(strsplit(x, "-", fixed = TRUE), hdType)
    group <- get_frGroup(x)
  } else if (length(x) == 1){
    hdTypes <- hdType(strsplit(x, "-", fixed = TRUE)[[1]])
    group <- get_frGroup(x)
  } else {
    hdTypes <- hdType()
    group <- NULL
  }
  vctrs::new_vctr(x, hdTypes = hdTypes, group = group, class = "frType")
}

#' @export
get_frGroup <- function(frType_str){
  ctps <- strsplit(frType_str,"-")
  f <- function(ctypes){
    ct <- dplyr::count(tibble::tibble(ctypes = ctypes),ctypes)
    ct$n[ct$n == 1] <- ""
    ctv <- tidyr::unite(ct,ctype,ctypes,n,sep="") %>% .[[1]] %>% sort()
    paste(ctv,collapse="-")
  }
  purrr::map_chr(ctps, f)
}

#' @export
expand_frGroup <- function(frGroup){
  ft1 <- strsplit(frGroup,"-",fixed = TRUE)[[1]]
  cts <- substring(ft1,1,3)
  reps <- substring(ft1,4)
  purrr::flatten_chr(purrr::map2(cts,reps,function(x,y){
    # if(y == "P"){ # Are we still doing NumP things?
    #   return(rep(x,sample(2:6,1)))
    # }
    if(y == "") y = 1
    rep(x,as.numeric(y))
  }))
}


#' @title frType Vectors
#'
#' @description Creates or coerces objects of type "frType"
#'
#' @param x object to be created or coerced
#'
#' @return returns a frType value
#'
#' @examples
#'
#' frType("Cat")
#'
#' @export
frType <- function(x = character()) {
  if(is_hdType(x)){
    # x <- vctrs::vec_cast(x, character()) # Not working!
    x <- vctrs::vec_data(x)
    x <- paste(x, collapse = "-")
  }
  x <- vctrs::vec_cast(x, character())
  new_frType(x)
}



#' @title frType Vectors
#'
#' @description Creates or test for objects of type "frType"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type frType or not.
#'
#' @examples
#'
#' value <- frType("Cat")
#' is_frType(value)
#'
#' @export
is_frType <- function(x) {
  inherits(x, "frType")
}


#' @title frType Vectors
#'
#' @description coerces its argument to a frType. It is an abbreviated form of frType.
#'
#' @param x object to be coerced
#'
#' @return attempts to coerce its argument to frType type
#'
#' @examples
#'
#' some_chr_value <- "Cat"
#' class(some_chr_value)
#'
#' some_frt_value <- as_frType(some_chr_value)
#' class(some_frt_value)
#'
#' @export
as_frType <- function(x) {
  vctrs::vec_cast(x, new_frType())
}



#' @title frType Vectors
#'
#' @description a grouped way of reading frTypes values
#'
#' @param x several (more than one) available frTypes values
#'
#' @return a grouped view of given frTypes values
#'
#' @examples
#'
#' x <- c("Cat-Num-Cat")
#' fr <- frType(x)
#' frType_group(fr)
#'
#' @export
frType_group <- function(x){
  if(!is_frType(x)) stop("x must be a frType")
  attr(x, "group")
}



#' @title frType Vectors
#'
#' @description convert frTypes value(s) into hdType
#'
#' @param x an available frType value
#'
#' @return an hdType value
#'
#' @examples
#'
#' x <- frType("Cat")
#' class(x)
#'
#' x_hdt <- frType_hdTypes(x)
#' class(x_hdt)
#'
#' @export
frType_hdTypes <- function(x, chr = FALSE){
  if(!is_frType(x)) stop("x must be a frType")
  hdt <- attr(x, "hdTypes")
  if(chr) hdt <- as.character(hdt)
  hdt
}



#' @title frType Vectors
#'
#' @description convert frTypes value(s) into character
#'
#' @param x an available frType value, frType dataframe or hd_tbl dataframe
#'
#' @return a character value
#'
#' @examples
#'
#' x <- frType("Cat")
#' class(x)
#'
#' x_chr <- frType_str(x)
#' class(x_chr)
#'
#' @export
frType_str <- function(x){
  if(is_frType(x)){
    return(paste(vctrs::vec_data(frType_hdTypes(x)),collapse = "-"))
  }
  if("data.frame" %in% class(x) || "hd_tbl" %in% class(x)){
    return(paste0(purrr::map_chr(x, which_hdType), collapse = "-"))
  }
}





# Methods

## Format method

#' @export
format.frType <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.frType <- function(x, ...) {
  "frType"
}

# Coercion

#' @method vec_ptype2 frType
#' @export
vec_ptype2.frType <- function(x, y, ...) UseMethod("vec_ptype2.frType", y)

#' @method vec_ptype2.frType default
#' @export
vec_ptype2.frType.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A frType combined with a frType returns a frType

#' @method vec_ptype2.frType frType
#' @export
vec_ptype2.frType.frType <- function(x, y, ...) new_frType()

# # frType and character return double

#' @method vec_ptype2.frType character
#' @export
vec_ptype2.frType.character <- function(x, y, ...) frType()

#' @method vec_ptype2.character frType
#' @export
vec_ptype2.character.frType <- function(x, y, ...) frType()

# Casting

#' @method vec_cast frType
#' @export
vec_cast.frType <- function(x, to, ...) UseMethod("vec_cast.frType")

#' @method vec_cast.frType default
#' @export
vec_cast.frType.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce frType to frType

#' @method vec_cast.frType frType
#' @export
vec_cast.frType.frType <- function(x, to, ...) x

#' @method vec_cast.frType character
#' @export
vec_cast.frType.character <- function(x, to, ...) frType(x)

#' @method vec_cast.character frType
#' @export
vec_cast.character.frType <- function(x, to, ...) vctrs::vec_data(x)





