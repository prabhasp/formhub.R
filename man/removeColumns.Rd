\name{removeColumns}
\alias{removeColumns}
\title{Helper function to remove columns from data based on reg-exp matching. Also takes list of strings.}
\usage{
removeColumns(df, columnNameRegExpMatcher)
}
\arguments{
  \item{df}{data}

  \item{columnNameRegExpMatcher}{pattern(s) to match to
  columns; matched columns are dropped.}
}
\value{
a smaller data frame.
}
\description{
Helper function to remove columns from data based on
reg-exp matching. Also takes list of strings.
}
\examples{
good_eats_df <- onaDownload("good_eats", "mberg","mberg")
names(good_eats_form_df) # note it includes submit_date and submit_data both
names(removeColumns(good_eats_form_df, "submit*")) # both of which are gone now
names(removeColumns(good_eats_form_df, c("submit*", "_gps*")) # you can pass a list of regular expressions
}

