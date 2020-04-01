
#' @export
make_dic <- function(d, frtype = NULL){
  if(is.null(frtype))
    frtype <- guess_frType(d)
  if(!is_frType(frtype))
    frtype <- frType(frtype)
  ids <- col_ids_from_name(names(d))

  dic <-tibble::tibble(id = ids, label = names(d),
                       hdType = frType_hdTypes(frtype))
  names(d) <-ids
  list(data = force_frType(d, frtype), dic = dic,
       frtype = frtype, group = frType_group(frtype))
}
