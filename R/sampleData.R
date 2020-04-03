
#' Creates a new Datafringe from a dataframe
#' @name sampleData
#' @description Creates a new datapackage from json, data fringe, list of data fringes, list of data fringees (see Datafringe reference class)
#' @param d might be a json string, data fringe or list of data fringes.
#' @return dp
#' @export
#' @examples \dontrun{
#' fringe <- newDatafringeFromDatafringe(mtcars)
#' }
sampleData <- function(frtype, n = 20, loremNames = TRUE,
                       addNA = TRUE, rep = FALSE,...){
  arg <- c(as.list(environment()), list(...))
  #arg <- list(n = 5, loremNames = FALSE, addNA = FALSE)
  # frtype <- "Cat-Num-Pct-Gnm-Dat"
  # frtype <- "Cat-Num-Pct-Dat"
  if(!is_frType(frtype)){
    frtype <- frType(frtype)
  }

  hdtypes <- frType_hdTypes(frtype)

  sample_funs <- paste0("sample",hdtypes)

  req_params <- lapply(sample_funs, function(x)formals(args(x)))
  params <- lapply(req_params, function(x){
    modifyList(x, arg)
  })
  #lang <- args$lang %||% "en"
  d <- purrr::invoke_map(sample_funs, params)
  names(d) <- letterNames(length(d))
  if(loremNames){
    names(d) <- loremNames(length(d))
  }
  tibble::as_tibble(d)
}


sample___ <- function(n,addNA = TRUE, ...){
  rep(NA,n)
}

sampleUid <- function(n, addNA = TRUE, ...){
  Uid(paste("uid",1:n))
}

sampleCat <- function(n, nlevels = 5, addNA = TRUE,...){
  prefix <- sample(c("Cat","Type","X_","Form","Ilk"),1)
  v <- sample(paste0(prefix,LETTERS[1:nlevels]),n,replace = TRUE)
  if(addNA) v[sample(n,round(n/10))] <- NA
  Cat(v)
}

sampleSeq <- function(n, nlevels = 5, addNA = TRUE, ...){
  v <- paste0(loremNames(1),1:nlevels)
  v <- sample2(v,n)
  if(addNA) v[sample(n,round(n/10))] <- NA
  Uid(v)
}

sampleBin <- function(n, addNA = TRUE,...){
  Bin(sampleCat(n, nlevels = 2, addNA = TRUE,...))
}

sampleNum <- function(n,gt0 = NULL, addNA = TRUE,...){
  gt0 <- gt0 %||% FALSE
  v <- round(rnorm(n,1000,300)*1)
  if(!gt0) v[sample(n,1)] <- -10
  if(addNA) v[sample(n,round(n/10))] <- NA
  Num(v)
}

samplePct <- function(n,format = "decimal",addNA = TRUE,...){
  v <- round(runif(n,0,100),2)/100
  if(addNA) v[sample(n,round(n/10))] <- NA
  Pct(v)
}

sampleDst <- function(n,addNA = TRUE,...){
  v <- runif(n,0,1)*100
  if(addNA) v[sample(n,round(n/10))] <- NA
  Dst(v/sum(v))
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
  Dat(v)
}

sampleYea<- function(n, rep = FALSE, addNA = TRUE,...){
  if(!rep) {
    v <- seq(1900,length.out = n)
  }else{
    v <- sample(seq(1900,length.out = n/10),n, replace = TRUE)
  }
  if(addNA) v[sample(n,round(n/10))] <- NA
  Yea(v)
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
    countries <- readr::read_csv(system.file("data/world-countries.csv",package = "homodatum"))
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





