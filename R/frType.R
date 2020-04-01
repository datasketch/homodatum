
new_frType <- function(x = character()){
  vctrs::vec_assert(x, character())
  hdTypes <- hdType(strsplit(x, "-", fixed = TRUE)[[1]])
  group <- get_frGroup(x)
  vctrs::new_vctr(x, hdTypes = hdTypes, group = group, class = "frType")
}

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



frType <- function(x = character()) {
  x <- vctrs::vec_cast(x, character())
  new_frType(x)
}

is_frType <- function(x) {
  inherits(x, "frType")
}

# Methods

## Format method

format.frType <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

vec_ptype_abbr.frType <- function(x, ...) {
  "frType"
}

# Coercion
vec_ptype2.frType <- function(x, y, ...) UseMethod("vec_ptype2.frType", y)
vec_ptype2.frType.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A frType combined with a frType returns a frType
vec_ptype2.frType.frType <- function(x, y, ...) new_frType()
# # frType and character return double
vec_ptype2.frType.character <- function(x, y, ...) frType()
vec_ptype2.character.frType <- function(x, y, ...) frType()

# Casting
vec_cast.vctrs_frType <- function(x, to, ...) UseMethod("vec_cast.frType")
vec_cast.vctrs_frType.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce frType to frType
vec_cast.frType.frType <- function(x, to, ...) x
vec_cast.frType.character <- function(x, to, ...) frType(x)
vec_cast.character.frType <- function(x, to, ...) vctrs::vec_data(x)

as_frType <- function(x) {
  vctrs::vec_cast(x, new_frType())
}



frType_group <- function(x){
  if(!is_frType(x)) stop("x must be a frType")
  attr(x, "group")
}

frType_hdTypes <- function(x){
  if(!is_frType(x)) stop("x must be a frType")
  attr(x, "hdTypes")
}

frType_str <- function(x){
  if(is_frType(x)){
    return(paste(vctrs::vec_data(frType_hdTypes(x)),collapse = "-"))
  }
  if("data.frame" %in% class(x)){
    return(paste0(purrr::map_chr(x, which_hdType), collapse = "-"))
  }
}

#' @export
force_frType <- function(df, frtype){

  if(!is_frType(frtype)){
    frtype <- frType(frtype)
  }
  hdtypes <- frType_hdTypes(frtype)
  hdtypes_str <- vctrs::vec_data(hdtypes)

  df <- as.data.frame(df)
  # HERE GO ALL CASTS WITH GIVEN frType
  dd <- purrr::map2(df, hdtypes_str, function(x1,y1){
    do.call(y1, list(x1))
  })
  dd %>% tibble::as_tibble()
}




