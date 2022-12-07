
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
      dplyr::pull(id)
  }
  dic2 <- dic |> dplyr::filter(id %in% columns)
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

#' @export
suggest_columns <- function(f, frtype = NULL, group = NULL){
  stats <- fringe_stats(f)
  dic <- fringe_dic(f, stats = TRUE)
  suggest <- NULL
  # if(stats$ncol > 4){
  # }else{
  # }
  if(frtype == "Cat-Num"){
    suggest_cat <- dic_suggest_cat(dic, n = 1)
    suggest_num <- dic_suggest_num(dic, n = 1, random = TRUE)
    suggest <- c(suggest_cat, suggest_num)
  }
  suggest
}


dic_suggest_cat <- function(dic, n = 1, random = FALSE){
  if(is.null(dic$stats))
    stop("Need dic with stats")
  cats <- dic |>
    dplyr::filter(hdType == "Cat")
  cats$n_unique <- purrr::map_dbl(cats$stats, "n_unique")
  cats <- cats |>
    dplyr::filter(n_unique > 1, n_unique < 11)
  if(!random){
   cats <- cats |> dplyr::slice(1:n)
  }else{
    cats <- cats |> dplyr::sample_n(n)
  }
  columns <- cats |> dplyr::pull(id)
  names(columns) <- cats |> dplyr::pull(label)
  columns
}

dic_suggest_num <- function(dic, n = 1, random = FALSE){
  nums <- dic |> dplyr::filter(hdType == "Num")
  if(!random){
    nums <- nums |> dplyr::slice(1)
  }else{
    nums <- nums |> dplyr::sample_n(1)
  }
  columns <- nums |> dplyr::pull(id)
  names(columns) <- nums |> dplyr::pull(label)
  columns
}
