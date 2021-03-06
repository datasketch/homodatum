% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_data.R
\name{sample_data}
\alias{sample_data}
\title{Creates a new Datafringe from a dataframe}
\usage{
sample_data(
  frtype,
  n = 20,
  loremNames = TRUE,
  names = NULL,
  addNA = TRUE,
  rep = FALSE,
  ...
)
}
\arguments{
\item{d}{might be a json string, data fringe or list of data fringes.}
}
\value{
dp
}
\description{
Creates a new datapackage from json, data fringe, list of data fringes, list of data fringees (see Datafringe reference class)
}
\examples{
\dontrun{
fringe <- newDatafringeFromDatafringe(mtcars)
}
}
