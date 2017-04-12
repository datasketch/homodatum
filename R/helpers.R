
#' @export
move_first <- function(d,name){
  d[c(name, names(d)[!names(d) %in% name])]
}

#' @export
nested_to_list <- function(d){
  d <- d %>% arrange_(.dots=names(d)[1])
  l <- d[[2]]
  names(l) <- d[[1]]
  l
}



