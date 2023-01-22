
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
fringe_meta <- function(x){
  y <- list(name = x$name, description = x$description)
  modifyList(y, x$meta)
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
  y <- fringe_meta(x)
  yaml::write_yaml(y, file.path(path, paste0(x$slug,".yaml")))
}

#' @export
fringe_write_json <- function(x, path = "", overwrite_dic = FALSE, preview_nrows = 100){
  if(!is_fringe(x))
    stop("x is not a fringe")

  path.meta <- file.path(path,paste0(x$slug,".meta.json"))
  path <- file.path(path,paste0(x$slug,".json"))

  d <- fringe_data(x)
  dic <- purrr::map_df(x$dic, as_baseType)
  dic.complete <- dic %>% dplyr::select(-hdType)

  preview <- fringe_data(x, labels = TRUE)

  y <- fringe_meta(x)

  l.meta <- list(info = y, dic = dic, preview = head(preview, preview_nrows))
  l <- list(dic = dic.complete, data = d)

  jsonlite::write_json(l.meta, path.meta, auto_unbox = TRUE)
  jsonlite::write_json(l, path, auto_unbox = TRUE)

}


#' @export
#' @importFrom dstools %||%
fringe_write_xlsx <- function(x, path = "", overwrite_dic = FALSE,
                              credits = NULL, more_info = NULL){
  if(!is_fringe(x))
    stop("x is not a fringe")

  path <- file.path(path, paste0(x$slug,".xlsx"))

  d <- fringe_data(x)
  dic <- purrr::map_df(x$dic, as_baseType)
  dic$hdType <- NULL

  info <- fringe_meta(x)
  info <- unlist(info)
  info <- data.frame(label = names(info), value = info)
  credits <- credits %||% list(label = "credits", value = "")
  info <- rbind(credits, info, more_info)
  names(info) <- c("", "")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data")
  openxlsx::addWorksheet(wb, "Dictionary")
  openxlsx::addWorksheet(wb, "Info")

  openxlsx::writeDataTable(wb, 1, d)
  openxlsx::writeDataTable(wb, 2, dic)
  openxlsx::writeData(wb, 3, info)
  ## Not run:
  openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)

}

