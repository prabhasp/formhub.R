\name{replaceHeaderNamesWithLabels}
\alias{replaceHeaderNamesWithLabels}
\title{Get a new dataframe, where the header contains the full questions as opposed to slugs.}
\usage{
  replaceHeaderNamesWithLabels(formhubDataObj)
}
\arguments{
  \item{formhubDataObj}{is the formhub data object whose
  data slot will be renamed}
}
\value{
  a new data frames with the column names renamed from
  `name`s (slugs) to `label`s(full questions)
}
\description{
  formhub Objects have some data, as well as the form,
  which documents how the data was obtained through a
  survey. The data, by default, is represented by slugs,
  ie, items in the `name` column in the original xfrom.
  This function replaces slugs in the header with the
  actual question text.
}
\examples{
good_eats <- formhubDownload("good_eats", "mberg")
names(good_eats
}

