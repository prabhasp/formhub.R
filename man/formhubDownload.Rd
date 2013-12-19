\name{formhubDownload}
\alias{formhubDownload}
\title{Download data from formhub.}
\usage{
  formhubDownload(formName, uname, pass = NA, ...)
}
\arguments{
  \item{formName}{formname on formhub.org for which we
  download the data}

  \item{uname}{formhub.org username}

  \item{pass}{formhub.org password, if the data and/or form
  is private}

  \item{...}{other parameters to pass onto formhubRead}
}
\value{
  formhubDataObj a formhubData Object, with "data" and
  "form" slots
}
\description{
  This function downloads a dataset for the given form and
  username, and produces a formhubData Object.
}
\examples{
good_eats <- formhubDownload("good_eats", "mberg")
good_eats # is a data frame of all the data
good_eats
}

