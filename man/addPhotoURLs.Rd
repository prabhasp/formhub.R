\name{addPhotoURLs}
\alias{addPhotoURLs}
\title{Add columns corresponding to the original, as well as medium and small thumbnails of images
as stored on the formhub server.}
\usage{
addPhotoURLs(formhubDataObj, formhubUsername, type = "url")
}
\arguments{
  \item{the}{formhubData object which to create URLs from.}

  \item{the}{formhub username of the person who owns this
  form.}

  \item{type:}{"url" or "img". URL only puts image url in,
  img puts image tag in.}
}
\value{
a formhubData object, with a few additional URL columns
}
\description{
Add columns corresponding to the original, as well as
medium and small thumbnails of images as stored on the
formhub server.
}
\examples{
good_eats <- as.data.frame(formhubDownload("good_eats", "mberg"))
good_eats_with_photos <- addPhotoURLs(good_eats, "mberg")
grep("URL", names(good_eats_with_photo), value=T) # the new columns
}

