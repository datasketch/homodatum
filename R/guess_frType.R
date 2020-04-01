

guess_frType <- function(data, as_string = FALSE){
  hdtypes <- purrr::map_chr(data, guess_hdType)
  out <- frType(paste(hdtypes, collapse = "-"))
  if(as_string){
    out <- vctrs::vec_data(out)
  }
  out
}

