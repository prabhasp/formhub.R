\name{remapAllColumns}
\alias{remapAllColumns}
\title{Remap all of the columns of the formhub data object according to the remap_list}
\usage{
remapAllColumns(formhubDataObj, remap, strictness = "all_found")
}
\arguments{
  \item{remap}{A vector. The name is what to map, and the
  value what to map to.  Example: remap = c("yes" = TRUE,
  "no" = FALSE, "dk" = NA) maps all "yes" values to TRUE,
  "no" to FALSE, and "dk" to NA}

  \item{strictness}{One of "exact", "all_found", or
  "any_found"; Default = all.  Defines the strictness of
  finding data. For example, all_found ensures that all
  keys in the data are found in the keys of our remapList,
  anyFound will replace partial matches, whereas exact
  ensures a full 2way match.}
}
\value{
A data.frame with values replaced.
}
\description{
Remap all of the columns of the formhub data object
according to the remap_list
}
\examples{
good_eats <- formhubDownload("good_eats", "mberg")
}

