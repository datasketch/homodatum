
new_fringe <- function(x = new_data_frame(),
                       frtype = NULL,
                       name = NULL,
                       description = NULL,
                       slug = NULL,
                       meta = NULL){
  # vctrs::vec_assert(x, data.frame())
  dd <- make_dic(x, frtype = frtype)
  dd$name <- name
  dd$description <- description
  dd$slug <- slug
  dd$meta <- meta
  vctrs::new_list_of(dd, class = "fringe")
}


#' @export
fringe <- function(x = new_data_frame(), frtype = NULL,
                   name = NULL, description = NULL,
                   slug = NULL, ...) {
  if(is_fringe(x)) return(x)
  # x <- vctrs::vec_cast(x, data.frame())
  name <- name %||% deparse(substitute(x))
  description <- description %||% ""
  slug <- slug %||% make_slug(name)
  new_fringe(x, frtype = frtype, name = name,
             description = description,
             slug = slug,
             meta = list(...))
}

#' @export
is_fringe <- function(x) {
  inherits(x, "fringe")
}

# Methods

## Format method

# format.fringe <- function(x, ...) {
  # x1 <- paste(capture.output(vctrs::vec_data(x)),collapse="    \n")
  #cat(paste0(x, "\n"), sep = "")
  #sprintf(fmt = "%s", x)
  # print(format(x))
  # paste0(format(vctrs::vec_data(x)), " m")
  # sprintf(fmt = "%s", vctrs::vec_data(x))
  # paste0(capture.output(tibble::as_tibble(vctrs::vec_data(x))),collapse = "\n")
# }

vec_ptype_abbr.fringe <- function(x, ...) {
  "fringe"
}
#
# # Coercion
# vec_ptype2.frType <- function(x, y, ...) UseMethod("vec_ptype2.frType", y)
# vec_ptype2.frType.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
#   vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
# }
# # A frType combined with a frType returns a frType
# vec_ptype2.frType.frType <- function(x, y, ...) new_frType()
# # # frType and character return double
# vec_ptype2.frType.character <- function(x, y, ...) frType()
# vec_ptype2.character.frType <- function(x, y, ...) frType()
#
# # Casting
# vec_cast.vctrs_frType <- function(x, to, ...) UseMethod("vec_cast.frType")
# vec_cast.vctrs_frType.default <- function(x, to, ...) vec_default_cast(x, to)
# # Coerce frType to frType
# vec_cast.frType.frType <- function(x, to, ...) x
# vec_cast.frType.character <- function(x, to, ...) frType(x)
# vec_cast.character.frType <- function(x, to, ...) vctrs::vec_data(x)
#
# as_frType <- function(x) {
#   vctrs::vec_cast(x, new_frType())
# }

#' @export
write_fringe <- function(x, path = "", overwrite_dic = FALSE){
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



