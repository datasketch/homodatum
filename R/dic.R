

#' @title Dictionary
#' @description `create_dic()` Creates a data.frame dictionary identifying column id (with cleaned variable names), label and homodatum variable type
#'
#' @param d The data set for which the user is creating the dictionary for
#' @param frtype pre-defined fringe types (check available_hdTypes() for the complete list)
#'
#' @return a data frame with three columns: id, label and hdType
#' @export
#'
#' @examples
#' d <- mtcars
#' new_dic <- create_dic(d)
create_dic <- function(d, frtype = NULL){
  if(is.null(frtype)){
    if(is_hdtibble(d)){
      frtype <- hdtibble_frType(d)
    }else{
      frtype <- guess_frType(d)
    }
  }
  if(!is_frType(frtype))
    frtype <- frType(frtype)
  ids <- col_ids_from_name(names(d))
  dic <-tibble::tibble(id = ids, label = names(d),
                       hdType = frType_hdTypes(frtype))
  dic

}
