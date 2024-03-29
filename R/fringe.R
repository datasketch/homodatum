#' @importFrom dstools %||%
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
  dd$group <- get_frGroup(dd$frtype)
  dd$name <- name
  dd$description <- description
  dd$slug <- slug
  dd$meta <- meta
  dd$stats <- calculate_fringe_stats(d, dic)
  class(dd) <- "fringe"
  dd
}





#' @title Create a Fringe data frame
#' @description Create a Fringe object from a data frame. The main value of a fringe is its metadata. When creating it, fringe will add to the data frame the following information:
#'
#' - data: original data frame data. When it is created, the fringe will convert original variable R types onto homodatum ones (Num, Cat, Pct, etc. -see [available_hdTypes()])
#' - dic: A diccionary is created with three variable characteristics: id, label and hdType
#' - frType: Shows all variable types based in homodatum schema
#' - group: A grouped view of frType
#' - name: Name for the fringe data frame, setted on _name_ argument
#' - description: Description for the fringe data frame, setted on _description_ argument
#' - slug: a custom slug can be added
#' - stats: Depending on the variable type given by homodatum, the fringe will generate different kind of statistics: nrow, ncol, n_unique, n_na, pct_na, min, max
#' @param x A data frame
#' @param frtype The type of fringe to create
#' @param dic a custom variable dictionary can be added. [create_dic()] can help you with that.
#' @param name a custom name can be added
#' @param nam a custom description can be added
#' @param slug a custom slug can be added. If not, fringe will try creating one.
#' @param meta Custom Metadata can be added
#'
#' @examples
#' fringe(mtcars, frtype = "Num", name = "MTCars")
#'
#' @return A Fringe object
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

#' @title Fringe data frame
#' @description test for objects of type "fringe"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type fringe or not.
#'
#' @examples
#' some_df <- fringe(mtcars)
#' is_fringe(some_df)
#'
#' @export
is_fringe <- function(x) {
  inherits(x, "fringe")
}


calculate_fringe_stats <- function(d, dic){

  stats <- purrr::map(d, function(col){
    hdtype <- which_hdType(col)
    if(length(hdtype) != 0){
      do.call(paste0(hdtype, "_get_stats"), list(col))
    }else{
      NA
    }
  })

  list(nrow = nrow(d), ncol = ncol(d), col_stats = stats)
}

#' @export
fringe_stats <- function(f){
  f$stats
}

#' @export
#' @importFrom dstools %||%
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
fringe_dic <- function(f, id_letters = FALSE, stats = FALSE){
  dic <- f$dic
  if(id_letters)
    dic$id_letters <- letterNames(nrow(dic))
  if(stats){
    col_stats <- fringe_stats(f)$col_stats
    dic$stats <- unname(col_stats)
  }
  dic
}

#'@export
fringe_hdTypes <- function(fr, named = FALSE){
  x <- fr$dic$hdType
  if(named) names(x) <- fr$dic$id
  x
}

#'@export
fringe_frtype <- function(fr){
  as.character(fr$frtype)
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


