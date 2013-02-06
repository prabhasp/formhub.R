\name{form_to_df}
\alias{form_to_df}
\title{Converts formhub form.json format to dataframe format. Dataframe has name, type, label columns.}
\usage{
  form_to_df(formJSON)
}
\arguments{
  \item{formJSON}{formJSON that has been freshly read from
  JSON using JSONIO's fromJSON function.}
}
\value{
  formDF
}
\description{
  Converts formhub form.json format to dataframe format.
  Dataframe has name, type, label columns.
}
\examples{
good_eats_form_df <- form_to_df(fromJSON("~/Downloads/good_eats.json"))
}

