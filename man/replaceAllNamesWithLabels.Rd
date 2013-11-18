\name{replaceAllNamesWithLabels}
\alias{replaceAllNamesWithLabels}
\title{Get a new dataframe, where all 'name's are replaced with full labels.}
\usage{
  replaceAllNamesWithLabels(formhubDataObj,
    language = NULL)
}
\arguments{
  \item{formhubDataObj}{is the formhub data object whose
  data slot will be renamed}

  \item{language}{if this is a multi-lingual form, the
  language of choice for the labels should be passed in.
  For single-language forms language=NULL.}
}
\value{
  a new data frames with the column names, as well as
  factor values, renamed from `name`s (slugs) to
  `label`s(full questions)
}
\description{
  formhub Objects have some data, as well as the form,
  which documents how the data was obtained through a
  survey. The data, by default, is represented by slugs,
  ie, items in the `name` column in the original xfrom.
  This function replaces slugs in the header with actual
  question text, and replaces slugs in select one options
  with the actual resposne text.
}
\examples{
good_eats <- formhubDownload("good_eats", "mberg")
names(good_eats) # still slugged names
summary(good_eats$rating)
good_eats_readable <- replaceAllNamesWithLabels(good_eats)
names(good_eats_readable) # not slugged anymore
summary(good_eats_readable$`Risk Factor`) # not slugged anymore.
}

