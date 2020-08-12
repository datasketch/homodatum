



#' @export
is_hdtibble <- function(d){
  all(purrr::map_lgl(d, is_any_hdType))
}

#' @export
hdtibble <- function(df, frtype = NULL){

  if(is.null(frtype)){
    frtype <- guess_frType(df)
  }
  if(!is_frType(frtype)){
    frtype <- frType(frtype)
  }
  hdtypes <- frType_hdTypes(frtype)
  hdtypes_str <- vctrs::vec_data(hdtypes)

  df <- as.data.frame(df)
  # HERE GO ALL CASTS WITH GIVEN frType
  dd <- purrr::map2(df, hdtypes_str, function(x1,y1){
    if(y1 == "___") return(x1)
    do.call(y1, list(x1))
  })
  d <- dd %>% tibble::as_tibble()
  class(d) <- c(class(d), "hd_tbl")
  d
}



hdtibble_frType <- function(d){
  frType(hdType(purrr::map_chr(d, which_hdType)))
}



#' @export
hdtibble_hdTypes <- function(d){
  hdType(purrr::map_chr(d, which_hdType))
}



