
#' @export
sub_fringe_cols <- function(fr, frtype = NULL, group = NULL,
                            show_hdType = FALSE){
  if(!is_fringe(fr)){
    fr <- fringe(fr)
  }
  hdtypes <- fringe_hdTypes(fr, named = TRUE)
  subs <- sub_hdTypesVars(hdtypes, frtype = frtype, group = group)
  if(show_hdType) return(subs)
  lapply(subs, names)
}



sub_hdTypesVars <- function(namedHdTypes, frtype = NULL, group = NULL){

  pw <- powerSet(namedHdTypes)
  all_possible <- lapply(pw, function(x) {
    list(
      group = get_frGroup(paste(x,collapse = "-")),
      frtype = paste(x,collapse = "-"),
      permutations = hdTypes_permute(x)
    )
  })
  if(!is.null(frtype)){
    sub <- unlist(purrr::map(all_possible, "permutations"), recursive = FALSE)
    sub <- sub %>% purrr::keep(~ paste0(., collapse = "-") %in% frtype)
    return(sub)
  }
  if(!is.null(group)){
    sub <- all_possible %>% purrr::keep(~ .$group %in% group)
    sub <- unlist(purrr::map(sub, "permutations"), recursive = FALSE)
    return(sub)
  }
}


#' @export
frType_combine <- function(frt){
  if(is_frType(frt)){
    hdt <- frType_hdTypes(frt, chr = TRUE)
  }else if(is_hdType(frt)){
    hdt <- frt
  }
  l <- lapply(powerSet(hdt), paste, collapse = "-")
  lapply(l, frType)
}

hdTypes_permute <- function(hdtypes, nms = NULL){
  if(is_frType(hdtypes)){
    hdt <- frType_hdTypes(hdtypes, chr = TRUE)
  }else if(is_hdType(hdtypes)){
    hdt <- hdtypes
  }else {
    hdt <- hdType(hdtypes)
  }
  nms <- nms %||% names(hdt)
  if(is.null(nms))
    stop("ctypes must have names")
  y <- permuteVector(nms) %>% t()
  names(y) <- 1:ncol(y)
  y <- y %>% tibble::as_tibble(.name_repair = "unique") %>% as.list()
  x <- permuteVector(as.character(hdt)) %>% t()
  names(x) <- 1:ncol(x)
  x <- x %>% tibble::as_tibble(.name_repair = "unique") %>% as.list()
  unname(purrr::map2(x,y, ~ hdType(purrr::set_names(.x, .y))))
}


#' #' @export
#' possibleCtypes <- function(ctypes, castable = FALSE, combine = FALSE){
#'   #ctypes <- c("Cat","Cat","Num","Cat")
#'   if(castable & !combine){
#'     return(castable_list(ctypes))
#'   }
#'   if(combine){
#'     comb <- powerSet(ctypes)
#'     if(!castable){
#'       return(comb)
#'     }else{
#'       l <- purrr::map(comb, castable_list)
#'       names(l) <- purrr::map(comb,paste,collapse = "-")
#'       return(l)
#'     }
#'   }
#'   ctypes
#' }


#' #' @export
#' possibleNamedCtypes <- function(namedCtypes, permute = TRUE, castable = FALSE, ncol = NULL) {
#'   subdata <- powerSet(namedCtypes)
#'   if (!is.null(ncol)) {subdata <- subdata[purrr::map_lgl(subdata, ~length(.x) <= ncol)]}
#'   if(!permute)
#'     l <- subdata
#'   else{
#'     l <- purrr::map(subdata, permuteCtypes)
#'     l <- unlist(l, recursive = FALSE) %>% unname()
#'   }
#'   if(castable){
#'     l <- purrr::map(l, possibleCtypes, castable = TRUE)
#'   }
#'   names(l) <- purrr::map(l,function(x) paste0(names(x),collapse="|"))
#'   l
#' }
#'
#' #' @export
#' possibleNamedCtypesStr <- function(namedCtypes, permute = TRUE, castable = FALSE, ncol = NULL){
#'   l <- possibleNamedCtypes(namedCtypes, permute = TRUE, castable = castable, ncol = ncol)
#'   if(!castable){
#'     ctypesStr <- purrr::map(l, paste, collapse = "-")
#'   }else{
#'     ctypesStr <- purrr::map(l, function(s) map_chr(transpose(s),paste, collapse = "-"))
#'   }
#'   ctypesStr
#' }
#'
#'
#' #' @export
#' whichSubdata <- function(data, outCtypes, castable = FALSE){
#'   ctypes <- guessCtypes(data, named = TRUE)
#'   if(castable && (ncol(data) > 5))
#'     stop("Too many columns for castable combinations")
#'   ctypesStr <- possibleNamedCtypesStr(ctypes, permute = TRUE, castable = castable)
#'   outCtypesStr <- paste(outCtypes, collapse = "-")
#'   #if(is.null(outCtypes)){
#'   #  cnames <- names(ctypesStr)
#'   #}else{
#'     cnames <- ctypesStr %>% keep(function(s){outCtypesStr %in% s}) %>% names
#'   #}
#'   strsplit(cnames,"|", fixed = TRUE)
#' }
#'
#' # #' @export
#' # whichSubCtypes <- function(data, as_string = FALSE){
#' #   l <- possibleSubdata(data, permute = TRUE) # TODO ctype casts
#' #   l <- l %>% map("ctype") %>% map(unname) %>% unique()
#' #   if(as_string) return(map(l, paste, collapse = "-"))
#' #   l
#' # }
#'

#'
#'
#' #' @export
#' belongingCtypesCombinations <- function (dt, vectorOfPosibilities, names = FALSE, numP = TRUE) {
#'   if ("data.frame" %in% class(dt)) {
#'     namedCtypes <- guessCtypes(dt, as_string = FALSE, named = TRUE)
#'     namedCtypes[namedCtypes == "Pct"] <- "Num"
#'   }
#'   if (is.atomic(dt)) {
#'     namedCtypes <- dt
#'   }
#'   pr <- possibleNamedCtypes(namedCtypes)
#'   pr0 <- pr
#'   if (numP) {
#'     pr0 <- purrr::map(pr, function(z) {
#'       w <- which(z %in% "Num")
#'       z0 <- z
#'       if (length(w) > 1) {
#'         z0[w[1]] <- "NumP"
#'         z0 <- z0[-w[2:length(w)]]
#'         names(z0)[w[1]] <- paste(names(z)[w], collapse = "|")
#'       }
#'       z0
#'     })
#'   }
#'   lg <- purrr::map_lgl(pr0, function(s) {
#'     ct <- paste(s, collapse = "-")
#'     ps <- ct %in% vectorOfPosibilities
#'     if (ps) {
#'       ps <- any(map_lgl(vectorOfPosibilities, ~all(ct == .x)))
#'     }
#'     ps
#'   })
#'   if (sum(lg) == 0) {
#'     nmb <- NULL
#'   } else {
#'     nmb <- pr0[lg]
#'     if (!names) {
#'       nmb <- purrr::map(nmb, ~unname(.x))
#'     }
#'   }
#'   nmb
#' }
#'
#'
#' #' @export
#' whichFunction <- function (d, colReturn = "name", meta) {
#'   if ("data.frame" %in% class(d)) {
#'     ctp <- guessCtypes(d, as_string = FALSE)
#'     ctp[ctp == "Pct"] <- "Num"
#'     ctp <- paste0(ctp, collapse = "-")
#'   }
#'   if (is.atomic(d)) {
#'     ctp <- paste0(d, collapse = "-")
#'   }
#'   meta <- meta %>% dplyr::filter(ctypes == ctp)
#'   nms <- meta[[colReturn]]
#'   meta %>% pull(colReturn) %>% set_names(nms)
#' }
#'
