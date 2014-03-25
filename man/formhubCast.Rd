\name{formhubCast}
\alias{formhubCast}
\title{Casts a dataframe to the right types based on a form-dataframe.}
\usage{
formhubCast(dataDF, formDF, extraFormDF = data.frame(), dropCols = "",
  convert.dates = TRUE)
}
\arguments{
  \item{dataDF}{data}

  \item{formDF}{form data frame. See format above.}

  \item{extraFormDF}{override the form (such as by
  providing a type for a calculate, a new label, etc.)}

  \item{dropCols}{a regular expression, any column name
  that matches that regexp will be dropped}
}
\value{
formhubDataObj a formhubData Object, with "data" and "form"
slots
}
\description{
This function creates a formhubData object based on a pair
of dataframes: the data and the form that describes the
data. The column names of the data match with the "name"
column of the form, and the "type" column in the form
provide information for type conversion.
}
\examples{
#See examples under formhubRead; this should be used through formhubRead in almost all cases
}

