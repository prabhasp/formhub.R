\name{as.SpatialPointsDataFrame}
\alias{as.SpatialPointsDataFrame}
\title{Produce a SpatialPointsDataFrame if data has a column of type `gps` or `geopoint`.
Otherwise, return NA.}
\usage{
as.SpatialPointsDataFrame(formhubObj)
}
\arguments{
  \item{the}{formhub object which will be possibly co-erced
  to a SpatialPointsDataFrame object.}
}
\value{
A SpatialPointsDataFrame representation of this formhub
Object
}
\description{
Produce a SpatialPointsDataFrame if data has a column of
type `gps` or `geopoint`. Otherwise, return NA.
}
\examples{
good_eats_data <- as.data.frame(formhubDownload("good_eats", "mberg"))
ge_spdf <- as.SpatialPointsDataFrame(good_eats_data)
class(ge_spdf) # "SpatialPointsDataFrame"
}

