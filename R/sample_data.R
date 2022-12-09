
#' Creates a new Datafringe from a dataframe
#' @name sample_data
#' @rdname sample_data
#' @description Creates a new datapackage from json, data fringe, list of data fringes, list of data fringees (see Datafringe reference class)
#' @param d might be a json string, data fringe or list of data fringes.
#' @return dp
#' @export
#' @examples \dontrun{
#' fringe <- newDatafringeFromDatafringe(mtcars)
#' }
sample_data <- function(frtype, n = 20, loremNames = TRUE,
                       names = NULL,
                       addNA = TRUE, rep = FALSE,...){
  arg <- c(as.list(environment()), list(...))
  arg <- purrr::discard(arg, is.null)
  # str(arg)
  # arg <- list(n = 5, loremNames = FALSE, addNA = FALSE, names = NULL)
  # frtype <- "Cat-Num-Pct-Gnm-Dat"
  # frtype <- "Cat-Num-Pct-Dat"
  if(!is_frType(frtype)){
    frtype <- frType(frtype)
  }

  hdtypes <- frType_hdTypes(frtype)

  sample_funs <- paste0("sample",hdtypes)

  getFunParams <- function(x){
    formals(args(x))
  }

  req_params <- lapply(sample_funs, getFunParams)
  params <- lapply(req_params, function(x){
    modifyList(x, arg)
  })
  #lang <- args$lang %||% "en"
  d <- purrr::invoke_map(sample_funs, params)
  names(d) <- letterNames(length(d))
  if(loremNames){
    ncols <- length(d)
    names(d) <- paste0(loremNames(ncols),rep(" (",ncols),hdtypes,rep(")",ncols))
  }
  if(!is.null(names)){
    names(d) <- names
  }
  tibble::as_tibble(d)
}

#' @export
sample_fringe <- function(frtype, ...){
  d <- sample_data(frtype,...)
  fringe(d)
}


#' @export
sample___ <- function(n,addNA = TRUE, ...){
  rep(NA,n)
}

#' @export
sampleUid <- function(n, addNA = TRUE, ...){
  Uid(paste("uid",1:n))
}

#' @export
sampleCat <- function(n, nlevels = 5, addNA = TRUE,...){
  prefix <- sample(c("Cat","Type","X_","Form","Ilk"),1)
  v <- sample(paste0(prefix,LETTERS[1:nlevels]),n,replace = TRUE)
  if(addNA) v[sample(n,round(n/10))] <- NA
  Cat(v)
}

#' @export
sampleSeq <- function(n, nlevels = 5, addNA = TRUE, ...){
  v <- paste0(loremNames(1),1:nlevels)
  v <- sample2(v,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  Uid(v)
}

#' @export
sampleBin <- function(n, addNA = TRUE,...){
  Bin(sampleCat(n, nlevels = 2, addNA = TRUE,...))
}

#' @export
#' @importFrom dstools %||%
sampleNum <- function(n,gt0 = NULL, addNA = TRUE,...){
  gt0 <- gt0 %||% FALSE
  v <- round(rnorm(n,1000,300)*1)
  if(!gt0) v[sample(n,1)] <- -10
  if(addNA) v[sample(n,round(n/10))] <- NA
  Num(v)
}

#' @export
samplePct <- function(n,format = "decimal",addNA = TRUE,...){
  v <- round(runif(n,0,100),2)/100
  if(addNA) v[sample(n,round(n/10))] <- NA
  Pct(v)
}

#' @export
sampleDst <- function(n,addNA = TRUE,...){
  v <- runif(n,0,1)*100
  if(addNA) v[sample(n,round(n/10))] <- NA
  Dst(v/sum(v))
}


#' @export
sampleDat <- function(n,rep = FALSE, addNA = TRUE,...){
  # type = seq || random
  if(!rep){
    v <- seq(as.Date('2000-01-01'),length.out=n, by="day")
  }else{
    v <- sample2(seq(as.Date('2000-01-01'),length.out=n/5, by="day"),
                 n = n,replace = TRUE)
  }
  if(addNA) v[sample(n,round(n/10))] <- NA
  Dat(v)
}

#' @export
sampleYea<- function(n, rep = FALSE, addNA = TRUE,...){
  if(!rep) {
    v <- seq(1900,length.out = n)
  }else{
    v <- sample(seq(1900,length.out = n/10),n, replace = TRUE)
  }
  if(addNA) v[sample(n,round(n/10))] <- NA
  Yea(v)
}

#' @export
sampleYwe <- function(n, rep = FALSE, addNA = TRUE,...){
  v <- paste0("2001-",sprintf("%02d",sample(1:52,n)))
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

#' @export
sampleMon<- function(n, rep = TRUE, addNA = TRUE,...){
  v <- sample2(1:12,n,replace = rep)
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

#' @export
sampleDay<- function(n, rep = TRUE, addNA = TRUE,...){
  v <- sample2(1:31,n,replace = rep)
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

#' @export
sampleWdy<- function(n, rep = FALSE, lang = "en", locale = NULL, addNA = TRUE,...){
  dates <- sampleDat(n, addNA = addNA)
  weekdaysLangs <- readr::read_csv(system.file("data/weekdays-lang.csv",package = "homodatum"))
  wd_translation <- weekdaysLangs %>%
    filter(language == lang) %>%
    select(-language) %>% as_vector()
  wd <- weekdays(dates)
  wd_translation[match(wd, names(wd_translation))]
}

#' @export
#' @importFrom dstools %||%
sampleDtm <- function(n, start = NULL, end = NULL, addNA = TRUE,...){
  st <- start %||% Sys.Date()- 180
  et <- end %||% Sys.Date()
  st <- as.POSIXct(as.Date(st))
  et <- as.POSIXct(as.Date(et))
  dt <- as.numeric(difftime(et,st,unit="sec"))
  ev <- sort(runif(n, 0, dt))
  v <- st + ev
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

#' @export
sampleHms <- function(n, addNA = TRUE,...){
  st <- as.POSIXct(Sys.Date())
  dti <- sampleDtm(n, start = Sys.Date() -1, end = Sys.Date(), addNA = addNA)
  substring(dti,12,19)
}

#' @export
sampleMin <- function(n, addNA = TRUE,...){
  v <- sample2(0:59,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

#' @export
sampleSec <- sampleMin

#' @export
sampleHie <- sampleCat

#' @export
sampleGrp <- sampleCat

#' @export
sampleTxt <- function(n, addNA = TRUE,...){
  nwords <- sample2(10:20,n)
  v <- purrr::map_chr(nwords,~ paste0(loremNames(.),collapse=" "))
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

#' @export
sampleMny <- sampleNum

availableGeoScops <- function(){
  c("world", "col_departments", "col")
}


geoDataframe <- function(scope){
  scope <- scope %||% "world"
  if(scope == "world")
    countries <- readr::read_csv(system.file("data/world-countries.csv",
                                             package = "homodatum"),
                                 col_types = readr::cols())
  else{
    if (!require("geodata"))
      stop("Please install package geodata")
    if(!scope %in% geodata::availableGeodata())
      stop("Check available scopes with geodata::availableGeodata()")
    geodata::geodataCsv(scope)
  }
}

#' @export
sampleGcd <- function(n, addNA = TRUE, scope = "world", ...){
  df <- geoDataframe(scope)
  v <- sample2(df$id,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  Gcd(v)
}

#' @export
sampleGnm <- function(n,addNA = TRUE, scope = "world", ...){
  df <- geoDataframe(scope)
  v <- sample2(df$name,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  Gnm(v)
}

#' @export
sampleGlt <- function(n,addNA = TRUE, scope = "world", ...){
  df <- geoDataframe(scope)
  v <- sample2(df$lat,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  Glt(v)
}

#' @export
sampleGln <- function(n,addNA = TRUE, scope = "world", ...){
  df <- geoDataframe(scope)
  v <- sample2(df$lon,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  Gln(v)
}

#' @export
sampleImg <- function(n,addNA = TRUE,...){
  v <- sample2(1:50,n)
  v <- paste0("file",v,".png")
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

#' @export
sampleAud <- function(n,addNA = TRUE,...){
  v <- sample2(1:50,n)
  v <- paste0("file",v,".mpg")
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}





