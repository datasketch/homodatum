

#' @export
fringeske<- function(ctypes=NULL,cformats=NULL,cnames = NULL,
                     name = NULL, description = NULL,
                     validators = NULL, sample_data = NULL, useCnames = TRUE){
  if(!is.null(sample_data))
    name <- name %||% deparse(substitute(sample_data))
  fringeske <- FringeSke$new(
    ctypes=ctypes,
    cformats=cformats,
    cnames=cnames,
    name = name,
    description = description,
    validators = validators,
    sample_data = sample_data,
    useCnames = useCnames)
  fringeske
}



#' #' @export
#' sameFringes <- function(f1,f2){
#'   all(
#'     identical(getCnames(f1),getCnames(f2)),
#'     identical(getCtypes(f1),getCtypes(f2)),
#'     identical(getCformats(f1),getCformats(f2)),
#'     identical(f1$dic_$d,f2$dic_$d),
#'     identical(f1$d,f2$d)
#'   )
#' }
#'
#' #' @export
#' getDatafringe <- function(fringe, withNames = TRUE){
#'   if(!isFringe(fringe)) stop('class is not Fringe')
#'   if(withNames) return(fringe$data)
#'   else return(fringe$d)
#' }
#'
#' #' @export
#' getCnames <- function(fringe){
#'   if(isFringe(fringe))
#'     return(fringe$dic_$d$id)
#'   if(is.data.frame(fringe))
#'     return(names(fringe))
#' }
#'
#' #' @export
#' getClabels <- function(fringe){
#'   if(isFringe(fringe))
#'     return(fringe$dic_$d$label)
#'   if(is.data.frame(fringe))
#'     return(names(fringe))
#' }
#'
#' #' @export
#' getCdescriptions <- function(fringe){
#'   if(!isFringe(fringe)) stop('class is not Fringe')
#'   fringe$dic_$d$cdescription
#' }
#'
#' #' @export
#' getCtypes <- function(fringe, cols = NULL){
#'   if(!isFringe(fringe))
#'     fringe <- fringe(fringe)
#'   fringe$dic_$d$ctype
#' }
#'
#' #' @export
#' getCformats <- function(fringe){
#'   if(!isFringe(fringe))
#'     fringe <- fringe(fringe)
#'   fringe$dic_$d$cformat
#' }


#' @export
getCaCnames <- function(fringe, n = 4){
  d <- getDatafringe(fringe)
  nvals <- sapply(d,function(c)length(unique(c)))
  names(nvals[nvals <= n])
}

#' @export
getFtype <- function(fringe){
  if(!isFringe(fringe))
    fringe <- fringe(fringe)
  fringe$ftype
}




#' @export
fringeHasFringeSkeleton <- function(fringe,fringeSke){
  # Check ctypes and cnames
  cfringe <- getCnames(fringe)
  names(cfringe) <- getCtypes(fringe)
  cske <- fringeSke$cnames
  names(cske) <- fringeSke$ctypes
  ctypesCnamesCheck <- identical(cfringe,cske)

  # Check validators
  validators <- fringeSke$validators
  if(paste(validators,collapse="") != ""){
    validatorsTmp <- lapply(validators,function(v){strsplit(v,":",fixed=TRUE)[[1]]})
    validatorCheck <- lapply(validatorsTmp,function(v){
      cols <- strsplit(v[-1],"|",fixed=TRUE)[[1]]
      type <- v[1]
      colValidate(fringe,type = type,cols = cols)
    })
    validatorCheck <- all(unlist(validatorCheck))
  } else{
    validatorCheck <- TRUE
  }
  # Return validations
  ctypesCnamesCheck && validatorCheck
}


#' @export
validValidators <- function(validators){
  #validators <- c("fringeColVal_greaterThan0:fdsafs","fringeColVal_unique:fdsafds")
  if(length(validators) == 1 && validators == "") return(TRUE)
  v <- strsplit(validators,":")
  v <- purrr::map(v,function(i){i[[1]]})
  fringevalf <- paste0("fringeVal_",fringeValidateFuns())
  colvalf <- paste0("fringeColVal_",fringeColValidateFuns())
  all(v %in% c(fringevalf,colvalf))
}




#' @export
setCnames <- function(t,cnames, idx = NULL){
  if(!isFringe(t))
    stop("fringe must be a Fringe")
  t$setCnames(cnames,idx = idx)
  t
}

#' @export
setCdescriptions <- function(t,cdescriptions, idx = NULL){
  if(!isFringe(t))
    stop("fringe must be a Fringe")
  t$setCdescriptions(cdescriptions,idx = idx)
  t
}

