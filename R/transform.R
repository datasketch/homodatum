#'
#' #' sampleFringe
#' #' @export
#' sampleFringe <- function(ftype,...){
#'   d <- sample_data(ftype,...)
#'   fringe(d)
#' }
#'
#'
#' #' @export
#' keep_not_na_FringeRows <- function(fringeIn){
#'   if(!class(fringeIn)[1] %in% c("Fringe","data.frame"))
#'     stop("fringe must be either a Fringe of a data.frame")
#'   if(!isFringe(fringeIn)) fringe <- fringe(fringeIn)
#'   else fringe <- fringeIn
#'   data <- fringe$data %>% discard_any_na_rows()
#'   dic <- fringe$dic_$d
#'   fringe(data,dic)
#' }
#'
#' #' @export
#' keepFringeRows <- function(fringeIn,col,values){
#'   if(!class(fringeIn)[1] %in% c("Fringe","data.frame"))
#'     stop("fringe must be either a Fringe of a data.frame")
#'   if(!isFringe(fringeIn)) fringe <- fringe(fringeIn)
#'   else fringe <- fringeIn
#'   if(class(col) %in% c("numeric","integer"))
#'     cols <- getCnames(fringe)[col]
#'   if(!all(col %in% getCnames(fringe)))
#'     stop("col not in fringe",
#'          paste(col[!col %in% getCnames(fringe)],collapse="\n"))
#'   colPos <- match(col,getCnames(fringe))
#'   colLetter <- letterNames(colPos)[colPos]
#'   filter_criteria <- interp(~ col %in% values, col = as.name(getCnames(fringe)[colPos]))
#'   data <- fringe$data %>% filter_(filter_criteria)
#'   dic <- fringe$dic_$d
#'   fringe(data,dic)
#' }
#'
#'
#' #' @export
#' selectFringeCols <- function(fringeIn,cols){
#'   if(is.null(fringeIn)) return()
#'   if(!class(fringeIn)[1] %in% c("Fringe","data.frame"))
#'     stop("fringe must be either a Fringe of a data.frame")
#'   if(!isFringe(fringeIn)) fringe <- fringe(fringeIn)
#'   else fringe <- fringeIn
#'   if(class(cols) %in% c("numeric","integer"))
#'     cols <- getCnames(fringe)[cols]
#'   if(! all(cols %in% getCnames(fringe)))
#'     stop("cols: ",cols[!cols %in% getCnames(fringe)]," not in fringe ",fringe$name)
#'   d <- getDatafringe(fringe)
#'   dic <- fringe$dic_$d %>% filter(id %in% cols) %>%
#'     slice(match(cols, id)) # added to rearrange dictionary given cols
#'   out <- d[cols]
#'   if(isFringe(fringeIn)) return(fringe(out,dic))
#'   out
#' }
#'
#' #' @export
#' selectFringeCtypes <- function(f,ctypes){
#'   dic <- selectDicCtypes(f,ctypes)
#'   data <- f$data %>%  select_(.dots = dic$id)
#'   fringe(data,dic)
#' }
#'
#' #' @export
#' selectDicCtypes <- function(f,ctypes, as_list = FALSE, filter = NULL){
#'   out <- f$dic_$d %>% filter(ctype %in% ctypes)
#'   if(!is.null(filter)){
#'     if(!filter %in% names(out)) stop("Filter not in diccionary")
#'     filter_criteria <- interp(~ filter == TRUE, filter = as.name(filter))
#'     out <- out %>% filter_(filter_criteria)
#'   }
#'   if(as_list){
#'     # setNames(transpose(out),out$id) # in case we want the full dic as list
#'     out_list <- as.list(setNames(out$id, out$label))
#'     return(out_list)
#'   }
#'   out
#' }
#'
#'
#' #' @export
#' joinFringes <- function(fx,fy,prefixX = NULL, prefixY = NULL, type = "full",...){
#'   if(!type %in% c("full","inner","left","right","semi","anti"))
#'     stop("join type not known")
#'   prefixX <- prefixX %||% fx$name
#'   prefixY <- prefixY %||% fy$name
#'   if(type == "full"){
#'     #d <- full_join(fx$data,fy$data)
#'     d <- full_join(fx$data,fy$data,...)
#'   }
#'   if(type == "inner"){
#'     d <- inner_join(fx$data,fy$data,...)
#'   }
#'   if(type == "left"){
#'     d <- left_join(fx$data,fy$data,...)
#'   }
#'   if(type == "right"){
#'     d <- right_join(fx$data,fy$data,...)
#'   }
#'   if(type == "semi"){
#'     d <- semi_join(fx$data,fy$data,...)
#'   }
#'   if(type == "anti"){
#'     d <- anti_join(fx$data,fy$data,...)
#'   }
#'   joinBy <- intersect(getCnames(fx),getCnames(fy))
#'   fxNames <- setdiff(getCnames(fx),getCnames(fy))
#'   fyNames <- setdiff(getCnames(fy),getCnames(fx))
#'   dicx <- fx$dic_$d
#'   idx <- dicx$id %in% fxNames
#'   if(is.null(dicx$join_group)){
#'     dicx$join_group <- NA
#'     dicx$join_group[idx] <- prefixX
#'   }
#'   dicy <- fy$dic_$d
#'   idx <- dicy$id %in% fyNames
#'   if(is.null(dicy$join_group)) {
#'     dicy$join_group <- NA
#'     dicy$join_group[idx] <- prefixY
#'   }
#'   dic <- bind_rows(dicx,dicy) %>% distinct(id,.keep_all = TRUE)
#'   fringe(d, dic = dic)
#' }
#'
#'
