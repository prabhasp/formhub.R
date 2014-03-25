\name{as.data.frame.formhubData}
\alias{as.data.frame.formhubData}
\title{Produce a data.frame out of a formhubDataObj}
\usage{
\method{as.data.frame}{formhubData}(fhD, ...)
}
\arguments{
  \item{the}{formhub object which will be possibly co-erced
  to a dataframe.}

  \item{the}{formhub object which will be possibly co-erced
  to a SpatialPointsDataFrame object.}
}
\value{
A data.frame represntation of this formhub oject

A SpatialPointsDataFrame representation of this formhub
Object
}
\description{
Produce a data.frame out of a formhubDataObj
}
\examples{
#' Produce a SpatialPointsDataFrame if data has a column of type `gps` or `geopoint`.
Otherwise, return NA.
}

