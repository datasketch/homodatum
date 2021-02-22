
#' @export
guess_hdType <- function(v){
  # if("data.frame" %in% class(v))
  #   v <- v %>% flatten

  if(is_any_hdType(v)){
    return(hdType(which_hdType(v)))
  }

  v <- unique(v[!is.na(v)])
  if(length(v) == 0)
    return(hdType("___"))

  if(any(class(v) %in% c("integer","numeric"))){
    ctype <- hdType("Num")
    if(all(v %in% 1500L:2200L)) ctype <- hdType("Yea")
    if(maybePct(v)) ctype <- hdType("Pct")
    return(ctype)
  }
  if(any(class(v) == "Date") | any(c("POSIXt", "POSIXct") %in% class(v)))
    return(hdType("Dat"))

  if(class(v)!= "factor" & !has_warning(as.numeric(v))){
    return(hdType("Num"))
  }

  #dth <- whichDTH(v)
  # if(!is.null(dth))
  #   ctype <- hdType(dth)
  if(isDate(v)){
    return(hdType("Dat"))
  }
  else{
    v <- as.character(v)
    if(maybeNum(v)){
      return(hdType("Num"))
    }
    if(maybePct(v)){
      return(hdType("Pct"))
    }
    ctype <- hdType("Cat")
    if(ctype == hdType("Cat") && maybeImgUrl(v)){
      ctype <- hdType("Img")
    }
    if(ctype == hdType("Cat") && maybeTxt(v))
      ctype <- hdType("Txt")
  }
  ctype
}

maybeNum <- function(v){
  v0 <- gsub(",",".",v)
  v0 <- gsub("\\.","",v0)

  nums <- tryCatch(as.numeric(v0),
                   error=function(e) e, warning=function(w) w
                   )
  if(inherits(nums, "warning")){
    return(FALSE)
  }
  if(na_proportion(nums) > 0.8){
    return(FALSE)
  }
  TRUE
}

has_decimal_comma <- function(v){
  v0 <- gsub("[0-9\\.]","", v)
  v1 <- gsub("-", "", v0)
  #has_commas <- grepl(",",v0)
  has_other_punct <- grepl("([-])|[[:punct:]]", gsub(",","", v1))
  if(any(has_other_punct)) return(FALSE)
  TRUE
}


maybePct <- function(v){
  if(is.numeric(v)){
    return(all(purrr::map(v, function(z) {all(z >= 0 && z <= 1) }) %>% unlist() == TRUE))
  }
  if(is.character(v)){
    return(all(grepl("([^%]*%[^%]*[0-9]+)|([0-9]+[^%]*%.*)", v[!is.na(v)])))
    # return(all(grepl("%", v[!is.na(v)])))
  }
}

maybeImgUrl <- function(x) all(grepl("^[http].+\\.((?i)jpg|png|gif|bmp|svg)$", x))

maybeTxt <- function(v){
  nwords <- function(x) vapply(strsplit(x, "[[:punct:] [^/]]"), length, integer(1))
  any(nchar(v) > 100) && any(nwords(v)>10)
}

