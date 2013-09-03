\name{formhubRead}
\alias{formhubRead}
\title{Reads data from a passed csv filename and json filename into a formhubData object.}
\usage{
  formhubRead(csvfilename, jsonfilename,
    extraFormDF = data.frame(), dropCols = "",
    na.strings = c("n/a"), convert.dates = TRUE)
}
\arguments{
  \item{csvfilename}{filename (or a connection object) that
  has the formhub data}

  \item{jsonfilename}{filename of a json file (or a
  connection object) that has the form.json form}

  \item{extraFormDF}{override the form (such as by
  providing a type for a calculate, a new label, etc.)}

  \item{dropCols}{a regular expression, any column name
  that matches that regexp will be dropped}

  \item{na.strings}{list of na.strings to be passed onto
  read.csv (default: "n/a")}
}
\value{
  formhubDataObj a formhubData Object, with "data" and
  "form" slots
}
\description{
  This function creates a formhubData object from two
  files: a csv data file, and a json form file. These
  should both be downloaded from formhub.org for the same
  form.
}
\examples{
# will need to download data.csv and form.json for a specific form on formhub, for below, download
http://formhub.org/mberg/forms/good_eats/data.csv http://formhub.org/mberg/forms/good_eats/form.json
good_eats <- formhubRead("~/Downloads/good_eats_2013_05_05.csv", "~/Downloads/good_eats.json")
head(good_eats) # is a data frame of all the data
good_eatsX <- formhubRead("~/Downloads/good_eats_2013_05_05.csv", "~/Downloads/good_eats.json",
             extraFormDF=data.frame(name="imei", type="integer", label="IMEI"))
good_eatsX
}

