
new_fringe <- function(d = new_data_frame(),
                       dic = NULL,
                       frtype = NULL,
                       name = NULL,
                       description = NULL,
                       slug = NULL,
                       meta = NULL){

  if(is.null(dic)){
    dic <- create_dic(d, frtype = frtype)
  } else {
    dic$hdType <- dic$hdType %||% frType_hdTypes(guess_frType(d))
    dic$hdType <- as_hdType(dic$hdType)
    dic <- tibble::as_tibble(dic)
  }
  names(d) <- dic$id
  if(!is_hdtibble(d)){
    d <- hdtibble(d, frtype = paste0(dic$hdType, collapse = "-"))
  }
  dd <- list(data = d, dic = dic)
  dd$frtype <- frType(paste0(dic$hdType, collapse = "-"))
  dd$group <- frType_group(dd$frtype)
  dd$name <- name
  dd$description <- description
  dd$slug <- slug
  dd$meta <- meta
  dd$stats <- calculateFringeStats(d)
  class(dd) <- "fringe"
  dd
}


#' @export
fringe <- function(x = new_data_frame(),
                   frtype = NULL, dic = NULL,
                   name = NULL, description = NULL,
                   slug = NULL,
                   meta = NULL,
                   ...) {
  if(is_fringe(x)) return(x)
  name <- name %||% deparse(substitute(x))
  description <- description %||% ""
  slug <- slug %||% make_slug(name)
  new_fringe(x, frtype = frtype,
             dic = dic,
             name = name,
             description = description,
             slug = slug,
             meta = modifyList(meta %||% list(), list(...)))
}

#' @export
is_fringe <- function(x) {
  inherits(x, "fringe")
}


calculateFringeStats <- function(x){
  list(nrow = nrow(x), ncol = ncol(x))
}

#' @export
fringe_stats <- function(f){
  f$stats
}

#' @export
fringe_update_meta <- function(f, ...){
  fixed <- c("data", "dic", "frtype", "group")
  args <- list(...)
  if(any(names(args) %in% fixed)){
    warning("Cannot update ",
            paste0(names(args)[names(args) %in% fixed], collapse = ", "),
            ". Removing from meta.")
    args <- args[!names(args) %in% fixed]
  }

  info <- list(name = args$name %||% f$name,
               description = args$description %||% f$description,
               slug = args$slug %||% make_slug(args$name),
               meta = args[!names(args) %in% c("name", "description","slug")])
  f <- modifyList(f, info)
  f$meta$sources <- args$sources %||% f$meta$sources
  f
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


#' @export
as_baseType <- function(x){
  ## TODO Cat as factors???
  UseMethod("as_baseType")
}

#' @export
as_baseType.default <- function(x){
  vctrs::vec_data(x)
}

#' @export
as_baseType.hd_Dat <- function(x){
  vctrs::new_date((vctrs::vec_data(x)))
}

#' @export
fringe_labels <- function(f){
  labels <- f$dic$label
  names(labels) <- letterNames(f$stats$ncol)
  labels
}

#' @export
fringe_ids <- function(f){
  f$dic$id
}

#' @export
fringe_column <- function(f, column){
  idx <- NULL
  if(is.numeric(column)){
    idx <- column
  }else{
    if(is.character(column)){
      idx <- match(column, fringe_labels(f))
      if(is.na(idx)){
        idx <- match(column, fringe_ids(f))
      }
      if(column %in% letters){
        d <- fringe_d(f)
        return(d[[column]])
      }
    }
  }
  if(is.null(idx)) stop("column not found")
  fringe_d(f)[[idx]]
}


#'@export
fringe_d <- function(f){
  purrr::map_df(f$data, as_baseType) %>%
    purrr::set_names(letterNames(nrow(f$dic)))
}

#'@export
fringe_data <- function(f, labels = FALSE){
  data <- fringe_d(f)
  if(labels){
    names(data) <- fringe_dic(f)$label
  }else{
    names(data) <- fringe_dic(f)$id
  }
  class(data) <- class(data)[class(data) != "hd_tbl"]
  data
}

#'@export
fringe_hdTibble <- function(f){
  data <- f$data
  if(!"hd_tbl" %in% class(data)){
    class(data) <- c(class(data), "hd_tbl")
  }
  data
}

#' @export
fringe_dic <- function(f, id_letters = FALSE){
  dic <- f$dic
  if(id_letters)
    dic$id_letters <- letterNames(nrow(dic))
  dic
}

#'@export
fringe_hdTypes <- function(fr, named = FALSE){
  x <- fr$dic$hdType
  if(named) names(x) <- fr$dic$id
  x
}




#' #' @export
#' force_hdTypes <- function(df, hdTypes){
#'   df <- as.data.frame(df)
#'   if(ncol(df)!= length(hdTypes)) stop("number of df cols must be the same as col types length")
#'   for (i in seq_along(hdTypes)){
#'     if(hdTypes[i]=="Num"){df[,i]<- as.numeric(df[,i])}
#'     if(hdTypes[i]=="Yea"){df[,i]<- as.character(df[,i])}
#'     if(hdTypes[i]=="Cat"){df[,i]<- as.character(df[,i])}
#'     if(hdTypes[i]=="Txt"){df[,i]<- as.character(df[,i])}
#'     if(hdTypes[i]=="Img"){
#'       if(!isImgUrl(df[,i])) stop ("Not an image Url")
#'       df[,i]<- as.character(df[,i])
#'     }
#'     if(hdTypes[i]=="Dat"){df[,i]<- parseDatetime(df[,i],"Dat")}
#'     if(hdTypes[i]=="Hms"){df[,i]<- parseDatetime(df[,i],"Hms")}
#'     if(hdTypes[i]=="Dti"){df[,i]<- parseDatetime(df[,i],"Dti")}
#'     if(hdTypes[i]=="Glt"){df[,i]<- as.numeric(df[,i])}
#'     if(hdTypes[i]=="Gln"){df[,i]<- as.numeric(df[,i])}
#'     if(hdTypes[i]=="Gnm"){df[,i]<- as.character(df[,i])}
#'   }
#'   as_tibble(df)
#' }


