
#' @export
make_dic <- function(d, ctypes = NULL){
  if(is.null(ctypes))
    ctypes <- guess_frType(d)
  ids <- col_ids_from_name(names(d))
  ## TODO format
  dic <- tibble(id = ids, label = names(d), ctype = ctypes)
  names(d) <-ids
  list(data = forceCtypes(d, ctypes), dic = dic)
}
