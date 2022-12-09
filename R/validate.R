#'
#'
#' #' @export
#' parseValidator <- function(str){
#'   str
#' }
#'
#'
#'
#' #' @export
#' fringeValidateFuns <- function(){
#'   fringeVal_funs <- as.character(lsf.str("package:fringer"))
#'   fringeVal_funs <- fringeVal_funs[grepl("^fringeVal_",fringeVal_funs)]
#'   gsub("fringeVal_","",fringeVal_funs, fixed = TRUE)
#' }
#'
#'
#' #' fringeVal_hasFtype
#' #' @name fringeVal_hasFtype
#' #' @description fringeVal_hasFtype
#' #' @export
#' fringeVal_hasFtype <- function(fringe,ftype){
#'   if(missing(ftype)) stop("need ftype as a parameter")
#'   identical(ftype,getFtype(fringe))
#' }
#'
#' #' fringeVal_hasAnyFtype
#' #' @name fringeVal_hasAnyFtype
#' #' @description fringeVal_hasAnyFtype
#' #' @export
#' fringeVal_hasAnyFtype <- function(fringe,ftype){
#'   if(missing(ftype)) stop("need ftype as a parameter")
#'   any(getFtype(fringe) %in% ftype)
#' }
#'
#' #' fringeVal_hasCtypes
#' #' @name fringeVal_hasCtypes
#' #' @description fringeVal_hasCtypes
#' #' @export
#' fringeVal_hasCtypes <- function(fringe,ctypes){
#'   if(missing(ctypes)) stop("need ctypes as a parameter")
#'   identical(getCtypes(fringe),ctypes)
#' }
#'
#' #' fringeVal_allNumeric
#' #' @name fringeVal_allNumeric
#' #' @description fringeVal_allNumeric
#' #' @export
#' fringeVal_allNumeric <- function(fringe){
#'   identical(unique(getCtypes(fringe)),"Nu")
#' }
#'
#'
#' #' fringeVal_hasColnames
#' #' @name fringeVal_hasColnames
#' #' @description fringeVal_hasColnames
#' #' @export
#' fringeVal_hasColnames <- function(fringe,cnames){
#'   if(missing(cnames)) stop("need cnames as a parameter")
#'   identical(getCnames(fringe),cnames)
#' }
#'
#' #' fringeVal_colnamesInFringe
#' #' @name fringeVal_colnamesInFringe
#' #' @description fringeVal_colnamesInFringe
#' #' @export
#' fringeVal_colnamesInFringe <- function(fringe,cols){
#'   if(missing(cols)) stop("need cnames as a parameter")
#'   cols %in% getCnames(fringe)
#' }
#'
#'
#' #' @export
#' fringeValidate <- function(t, validation, ...){
#'   if(!isFringe(t)) stop("must be a fringe")
#'   args <- list(...)
#'   availableValidations <- fringeValidateFuns()
#'   if(!validation %in% availableValidations)
#'     stop("no validation with that name")
#'   fun <- paste0("fringeVal_",validation)
#'   do.call(fun,c(t,list(...)))
#' }
#'
#'
#' ### COLUMN VALIDATORS
#'
#'
#'
#' #' @export
#' fringeColValidateFuns <- function(){
#'   colVal_funs <- as.character(lsf.str("package:fringer"))
#'   colVal_funs <- colVal_funs[grepl("^fringeColVal_",colVal_funs)]
#'   colVal_funs <- gsub("fringeColVal_","",colVal_funs, fixed = TRUE)
#'   colVal_funs
#' }
#'
#' #' fringeColVal_unique
#' #' @name fringeColVal_unique
#' #' @description fringeColVal_unique
#' #' @export
#' fringeColVal_unique <- function(fringe,cols){
#'   data <- getDatafringe(fringe)
#'   all(!duplicated(data[cols]))
#' }
#'
#' #' fringeColVal_greaterThan0
#' #' @name fringeColVal_greaterThan0
#' #' @description fringeColVal_greaterThan0
#' #' WORKS_WITH_CTYPE: N
#' #' @export
#' fringeColVal_greaterThan0 <- function(fringe,cols){
#'   data <- getDatafringe(fringe)
#'   data <- data[cols]
#'   all(sapply(data,function(i) i>0))
#' }
#'
#' #' fringeColVal_hasGenderLevelsEs
#' #' @name fringeColVal_hasGenderLevelsEs
#' #' @description fringeColVal_hasGenderLevelsEs
#' #' @export
#' fringeColVal_hasGenderLevelsEs <- function(fringe,cols){
#'   data <- getDatafringe(fringe)
#'   f <- function(i){i %in% c("Masculino","Femenino","")}
#'   all(sapply(data,f))
#' }
#'
#' #' fringeColVal_different
#' #' @name fringeColVal_different
#' #' @description fringeColVal_different
#' #' @export
#' fringeColVal_different <- function(fringe,cols){
#'   data <- getDatafringe(fringe)
#'   l <- lapply(cols,function(c){
#'     length(unique(data[,c])) == length(data[,c])
#'   })
#'   all(unlist(l))
#' }
#'
#' #' fringeColVal_hasCtype
#' #' @name fringeColVal_hasCtype
#' #' @description fringeColVal_hasCtype
#' #' @export
#' fringeColVal_hasCtype <- function(fringe,cols,ctype){
#'   ctypes <- getCtypes(fringe)
#'   idx <- match(cols,getCnames(fringe))
#'   ctypes <- ctypes[idx]
#'   fringe <- selectFringeCols(fringe,cols)
#'   all(getCtypes(fringe) %in% ctype)
#' }
#'
#'
#'
#' #' @export
#' @importFrom dstools %||%
#' fringeColValidate <- function(t, cols = NULL, validation, ...){
#'   availableValidations <- fringeColValidateFuns()
#'   if(!validation %in% availableValidations)
#'     stop("no validation with that name")
#'   if(!isFringe(t)) stop("must be a fringe")
#'   cols <- cols %||% getCnames(t)
#'   if(class(cols) %in% c("numeric","integer"))
#'     cols <- getCnames(t)[cols]
#'   if(!all(cols %in% getCnames(t)))
#'     stop('cols not in fringe')
#'   args <- list(...)
#'   fun <- paste0("fringeColVal_",validation)
#'   p <- c(list(t,cols),args)
#'   do.call(fun,p)
#' }
#'
#'
#'
#'
#'
#'
#'
