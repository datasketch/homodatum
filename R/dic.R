
#' @export
create_dic <- function(d, frtype = NULL){
  if(is.null(frtype)){
    if(is_hdtibble(d)){
      frtype <- hdtibble_frType(d)
    }else{
      frtype <- guess_frType(d)
    }
  }
  if(!is_frType(frtype))
    frtype <- frType(frtype)
  ids <- col_ids_from_name(names(d))
  dic <-tibble::tibble(id = ids, label = names(d),
                       hdType = frType_hdTypes(frtype))
  dic

}
