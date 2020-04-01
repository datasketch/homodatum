
#' @export
available_hdTypes <- function(as_character = FALSE){
  hds <- tibble::tribble(
    ~id,~label,~weight,
    "___","Null",1,
    "Uid","Uid",1,
    "Cat","Categorical",5,
    "Bin","Binary",3,
    "Seq","Sequential",3,
    "Num","Numeric",5,
    "Pct","Percentage",2,
    "Dst","Distribution",2,
    "Dat","Date",5,
    "Yea","Year",1,
    "Mon","Month",1,
    "Day","Day",1,
    "Wdy","Day of week",1,
    "Ywe","Week in year",1,
    "Dtm","Date time",1,
    "Hms","Time HMS",1,
    "Min","Minutes",1,
    "Sec","Seconds",1,
    "Hie","Hierarchy",1,
    "Grp","Group",1,
    "Txt","Text",1,
    "Mny","Money",1,
    "Gnm","Geo name",1,
    "Gcd","Geo code",1,
    "Glt","Geo latitude",1,
    "Gln","Geo longitude",1,
    "Img","Image",1,
    "Aud","Audio",1,
  )
  if(as_character) return(hds$id)
  hdType(hds$id)

}
