---
layout: main
---
formhub.R
=========

formhub.R is a library for making reading data form http://formhub.org into R easier.

At the moment, it has the following features:
 * One command download of formhub.org data -- both public and private
 * Casting of data to the right type, based on the type of the input field in XLSform
   * `select one` fields are converted to factors
   * `integer`/`decimal` converted to numerics
   * `date` fields (including `today`), and `datetime` fields (including `start` and `end`) are converted to lubridate instants [timezone information is discarded at the moment]
   * TRUE / FALSE fields created out of `select multiple` options are converted into booleans
 * removeColumns convenience function that removes columns based on matching the title with a regular expression
 * 'extra-schema' over-ride. This helps provide a type to `calculate` fields if that is desired; automagically calculating the type of a calculate field is not in the short-term feature list

For planned features, go to the [issues](https://github.com/prabhasp/formhub.R/issues) page.


