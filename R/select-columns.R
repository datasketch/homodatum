
#' @export
select_columns <- function(f, columns){

  dic <- fringe_dic(f)
  by_id <- all(columns %in% dic$id)
  by_label <- all(columns %in% dic$label)
  if(!by_id && !by_label){
    stop("Need to select columns by id or label")
  }
  if(!by_id && by_label){
    columns <- dic |>
      filter(label %in% columns) |>
      pull(id)
  }
  dic2 <- dic |> filter(id %in% columns)
  fringe(f$data[columns], dic = dic2,
         name = f$name, description = f$description)
}

#' @export
hdtypes_subset <- function(x, frtype = NULL, group = NULL){
  if(is_fringe(x)){
    dic <- fringe_dic(x)
    hdtypes <- dic$hdType
    names(hdtypes) <- dic$label
  }else if(is_hdType(x)){
    hdtypes <- x
    if(is.null(names(x)))
      stop("hdtype must be named")
  }
  subs <- sub_hdTypesVars(hdtypes, frtype = frtype, group = group)
  subs
}

#' @export
fringe_subset_columns <- function(f, frtype = NULL, group = NULL){
  subs <- hdtypes_subset(f, frtype = frtype, group = group)
  sub_cols <- names(subs[[1]])
  select_columns(f, columns = sub_cols)
}


