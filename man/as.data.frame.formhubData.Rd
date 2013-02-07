\name{as.data.frame.formhubData}
\alias{as.data.frame.formhubData}
\title{Get just the data out of a formhub object. as.data.frame(obj) equivalent to obj}
\usage{
  as.data.frame.formhubData(fD)
}
\arguments{
  \item{formhubDataObj}{is the formhub data object whose
  data slot will be returned}
}
\value{
  the data inside this formhubData object
}
\description{
  Get just the data out of a formhub object.
  as.data.frame(obj) equivalent to obj
}
\examples{
good_eats_data <- as.data.frame(formhubDownload("good_eats", "mberg"))
class(good_eats_data) # is data.frame
head(good_eats_data) # and has all the data
}

