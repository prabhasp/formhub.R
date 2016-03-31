\name{onaDownload}
\alias{onaDownload}
\title{Download data from ona.}
\usage{
onaDownload(formName, account, uname, pass = NA, ...)
}
\arguments{
  \item{formName}{formname on ona for which we
  download the data}

  \item{account}{ona username/org_name}
  \item{uname}{ona username}

  \item{pass}{ona password, if the data and/or form
  is private}

  \item{...}{other parameters to pass onto onaRead}
}
\value{
onaDataObj a onaData Object, with "data" and "form"
slots
}
\description{
This function downloads a dataset for the given form, account(username/org_name) and
username, and produces a onaData Object.
}
\examples{
good_eats <- onaDownload("good_eats", "mberg","mberg")
good_eats # is a data frame of all the data
good_eats
}

