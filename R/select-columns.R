
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
subset_columns <- function(f, frtype = NULL, group = NULL){
  dic <- fringe_dic(f)
  hdtypes <- dic$hdType
  names(hdtypes) <- dic$label
  subs <- sub_hdTypesVars(hdtypes, frtype = frtype, group = group)
  sub_cols <- names(subs[[1]])
  select_columns(f, columns = sub_cols)
}
