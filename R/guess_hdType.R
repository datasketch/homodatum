
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

  if(class(v) %in% c("integer","numeric")){
    ctype <- hdType("Num")
    if(all(v %in% 1500L:2200L)) ctype <- hdType("Yea")
    if(maybePct(v)) ctype <- hdType("Pct")
    return(ctype)
  }
  if(class(v) == "Date")
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




maybePct <- function(v){
  if(is.numeric(v)){
    return(all(purrr::map(v, function(z) {all(z >= 0 && z<=1) }) %>% unlist() == TRUE))
  }
  if(is.character(v)){
    return(all(grepl("%", v[!is.na(v)])))
  }
}

maybeImgUrl <- function(x) all(grepl("^[http].+\\.((?i)jpg|png|gif|bmp|svg)$", x))

maybeTxt <- function(v){
  nwords <- function(x) vapply(strsplit(x, "[[:punct:] [^/]]"), length, integer(1))
  any(nchar(v) > 100) && any(nwords(v)>10)
}

