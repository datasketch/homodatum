
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

