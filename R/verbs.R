
#' @export
fr_select <- function(f, ids = NULL, labels = NULL){
  d <- fringe_data(f)
  dic <- fringe_dic(f)
  if(!is.null(labels)  && is.null(ids)){
    idx <- match(labels, dic$label)
    if(any(is.na(idx))) stop("labels not found in data")
    new_dic <- dic %>% dplyr::slice(idx)
    idx <- match(new_dic$id, names(d))
    new_data <- d %>% dplyr::select(all_of(idx))
  }else if(!is.null(ids) && is.null(labels)){
    idx <- match(ids, dic$id)
    if(any(is.na(idx))) stop("ids not found in data")
    new_dic <- dic %>% dplyr::slice(idx)
    idx <- match(new_dic$id, names(d))
    new_data <- d %>% dplyr::select(all_of(idx))
  } else{
    stop("need ids or labels to select")
  }
  # OJO... drop meta???
  fringe(new_data, dic = new_dic, name = f$name, description = f$description)
}


