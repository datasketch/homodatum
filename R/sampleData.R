
#' Creates a new Datafringe from a dataframe
#' @name sampleData
#' @description Creates a new datapackage from json, data fringe, list of data fringes, list of data fringees (see Datafringe reference class)
#' @param d might be a json string, data fringe or list of data fringes.
#' @return dp
#' @export
#' @examples \dontrun{
#' fringe <- newDatafringeFromDatafringe(mtcars)
#' }
sampleData <- function(ctypes, nrow = 20, loremNames = TRUE,
                       addNA = TRUE, rep = FALSE,...){
  #nrow <- 100
  #ftype <- "Cat2-Yea2-Num1-Dat1"
  #ftype <- "Cat-Yea-NumP"
  # ftype <- "Cat2-Num1"
  if(length(ctypes)>1)
    warning("Use ctypes as a single string")

  ctypes <- sampleCtypes(ctypes)
  ncols <- length(ctypes)

  #s <- list(Cat = samp, Nu = nu, Da = da, Ye = ye)
  ctids <- availableCtypeIds()
  sampleFunNames <- paste0("sample",ctids) %>% set_names(ctids)
  selFuns <- sampleFunNames[ctypes]
  #args <- list(gt0 = TRUE)
  args <- list(...)
  lang <- args$lang %||% "en"
  rep <- args$rep %||% TRUE
  makeFtypeParams <- function(ftype){
    if(ftype == "Num")
      return(list(n = nrow,gt0 = args$gt0,addNA = addNA))
    if(ftype == "Mon")
      return(list(n = nrow,rep = rep, addNA = addNA))
    if(ftype == "Wdy")
      return(list(n = nrow,lang = lang,addNA = addNA))
    if(ftype %in% c("Gcd","Gnm","Glt","Gln"))
      return(list(n = nrow,lang = lang,addNA = addNA, scope = args$scope))
    list(n = nrow, addNA = addNA, rep = rep)
  }
  params <- purrr::map(ctypes,makeFtypeParams)
  d <- purrr::invoke_map(selFuns, params)

  if(!loremNames){
    names(d) <- letterNames(ncols)
  }else{
    names(d) <- loremNames(ncols)
  }
  out <- as_tibble(d)
  out
}


sample___ <- function(n,addNA = TRUE, ...){
  rep(NA,n)
}

sampleUid <- function(n, addNA = TRUE, ...){
  paste("uid",1:n)
}

sampleCat <- function(n, nlevels = 5, addNA = TRUE,...){
  prefix <- sample(c("Cat","Type","X_","Form","Ilk"),1)
  v <- sample(paste0(prefix,LETTERS[1:nlevels]),n,replace = TRUE)
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleSeq <- function(n, nlevels = 5, addNA = TRUE, ...){
  v <- paste0(loremNames(1),1:nlevels)
  v <- sample2(v,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleBin <- function(n, addNA = TRUE,...){
  sampleCat(n, nlevels = 2, addNA = TRUE,...)
}

sampleNum <- function(n,gt0 = NULL, addNA = TRUE,...){
  gt0 <- gt0 %||% FALSE
  v <- round(rnorm(n,1000,300)*1)
  if(!gt0) v[sample(n,1)] <- -10
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

samplePct <- function(n,format = "decimal",addNA = TRUE,...){
  v <- round(runif(n,0,100),2)/100
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleDst <- function(n,addNA = TRUE,...){
  v <- runif(n,0,1)*100
  if(addNA) v[sample(n,round(n/10))] <- NA
  v/sum(v)
}


sampleDat <- function(n,rep = FALSE, addNA = TRUE,...){
  # type = seq || random
  if(!rep){
    v <- seq(as.Date('2000-01-01'),length.out=n, by="day")
  }else{
    v <- sample2(seq(as.Date('2000-01-01'),length.out=n/5, by="day"),
                 n = n,replace = TRUE)
  }
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleYea<- function(n, rep = FALSE, addNA = TRUE,...){
  if(!rep) {
    v <- seq(1900,length.out = n)
  }else{
    v <- sample(seq(1900,length.out = n/10),n, replace = TRUE)
  }
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleYwe <- function(n, rep = FALSE, addNA = TRUE,...){
  v <- paste0("2001-",sprintf("%02d",sample(1:52,n)))
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleMon<- function(n, rep = TRUE, addNA = TRUE,...){
  v <- sample2(1:12,n,replace = rep)
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleDay<- function(n, rep = TRUE, addNA = TRUE,...){
  v <- sample2(1:31,n,replace = rep)
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleWdy<- function(n, rep = FALSE, lang = "en", locale = NULL, addNA = TRUE,...){
  dates <- sampleDat(n, addNA = addNA)
  weekdaysLangs <- read_csv(system.file("data/weekdays-lang.csv",package = "homodatum"))
  wd_translation <- weekdaysLangs %>%
    filter(language == lang) %>%
    select(-language) %>% as_vector()
  wd <- weekdays(dates)
  wd_translation[match(wd, names(wd_translation))]
}

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

sampleHms <- function(n, addNA = TRUE,...){
  st <- as.POSIXct(Sys.Date())
  dti <- sampleDtm(n, start = Sys.Date() -1, end = Sys.Date(), addNA = addNA)
  substring(dti,12,19)
}

sampleMin <- function(n, addNA = TRUE,...){
  v <- sample2(0:59,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleSec <- sampleMin

sampleHie <- sampleCat

sampleGrp <- sampleCat

sampleTxt <- function(n, addNA = TRUE,...){
  nwords <- sample2(10:20,n)
  v <- purrr::map_chr(nwords,~ paste0(loremNames(.),collapse=" "))
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleMny <- sampleNum

availableGeoScops <- function(){
  c("world", "col_departments", "col")
}


geoDataframe <- function(scope){
  scope <- scope %||% "world"
  if(scope == "world")
    countries <- read_csv(system.file("data/world-countries.csv",package = "homodatum"))
  else{
    if (!require("geodata"))
      stop("Please install package geodata")
    if(!scope %in% geodata::availableGeodata())
      stop("Check available scopes with geodata::availableGeodata()")
    geodata::geodataCsv(scope)
  }
}

sampleGcd <- function(n, addNA = TRUE, scope = "world", ...){
  df <- geoDataframe(scope)
  v <- sample2(df$id,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleGnm <- function(n,addNA = TRUE, scope = "world", ...){
  df <- geoDataframe(scope)
  v <- sample2(df$name,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleGlt <- function(n,addNA = TRUE, scope = "world", ...){
  df <- geoDataframe(scope)
  v <- sample2(df$lat,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleGln <- function(n,addNA = TRUE, scope = "world", ...){
  df <- geoDataframe(scope)
  v <- sample2(df$lon,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleImg <- function(n,addNA = TRUE,...){
  v <- sample2(1:50,n)
  v <- paste0("file",v,".png")
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}

sampleAud <- function(n,addNA = TRUE,...){
  v <- sample2(1:50,n)
  v <- paste0("file",v,".mpg")
  if(addNA) v[sample(n,round(n/10))] <- NA
  v
}





