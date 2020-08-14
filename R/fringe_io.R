
#' @export

fringe_read <- function(path){

  d <- readr::read_csv(paste0(path, ".csv"), col_types = readr::cols())
  dic <- readr::read_csv(paste0(path, ".dic.csv"), col_types = readr::cols())
  meta <- yaml::yaml.load_file(paste0(path, ".yaml"))
  additional_meta <- meta
  additional_meta$name <- NULL
  additional_meta$description <- NULL
  str(additional_meta)
  fringe(d, dic = dic,
         name = meta$name, description = meta$description,
         meta = additional_meta)

}


#' @export
fringe_write <- function(x, path = "", overwrite_dic = FALSE){
  if(!is_fringe(x))
    stop("x is not a fringe")
  # vctrs::vec_assert(x, new_fringe())
  readr::write_csv(x$data, file.path(path,paste0(x$slug,".csv")))
  dic_path <- file.path(path,paste0(x$slug,".dic.csv"))
  if(file.exists(dic_path) && !overwrite_dic ){
    stop("Cannot overwrite dic")
  }
  readr::write_csv(x$dic, dic_path)
  y <- list(name = x$name, description = x$description)
  y <- modifyList(y, x$meta)
  yaml::write_yaml(y, file.path(path, paste0(x$slug,".yaml")))
}

#' @export
fringe_write_json <- function(x, path = "", overwrite_dic = FALSE){
  if(!is_fringe(x))
    stop("x is not a fringe")

  path <- file.path(path,paste0(x$slug,".json"))

  d <- fringe_data(x)
  dic <- purrr::map_df(x$dic, as_baseType)

  preview <- fringe_data(x, labels = TRUE)

  y <- list(name = x$name, description = x$description)
  y <- modifyList(y, x$meta)

  l <- list(info = y, data = d, dic = dic, preview = head(preview, 1000))
  jsonlite::write_json(l, path, auto_unbox = TRUE)

}

