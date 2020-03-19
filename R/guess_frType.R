

guess_frType <- function(data, as_string = FALSE){
  hdtypes <- map_chr(data, guess_hdType)
  out <- frType(paste(hdtypes, collapse = "-"))
  if(as_string){
    out <- vec_data(out)
  }
  out
}

