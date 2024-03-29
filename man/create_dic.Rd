% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dic.R
\name{create_dic}
\alias{create_dic}
\title{Dictionary}
\usage{
create_dic(d, frtype = NULL)
}
\arguments{
\item{d}{The data set for which the user is creating the dictionary for}

\item{frtype}{pre-defined fringe types (check available_hdTypes() for the complete list)}
}
\value{
a data frame with three columns: id, label and hdType
}
\description{
\code{create_dic()} Creates a data.frame dictionary identifying column id (with cleaned variable names), label and homodatum variable type
}
\examples{
d <- mtcars
new_dic <- create_dic(d)
}
