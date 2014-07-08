<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>
formhub.R
=========

formhub.R is a library for making reading data form [formhub.org](http://formhub.org) into R easier.
<a href="https://github.com/SEL-Columbia/formhub.R"><img style="position: absolute; top: 0; left: 0; border: 0;" src="https://s3.amazonaws.com/github/ribbons/forkme_left_darkblue_121621.png" alt="Fork me on GitHub"></a>

Examples
--------

 * Getting started -- [Basics of formhub.R](http://SEL-Columbia.github.com/formhub.R/demo/Basics_of_formhub.R.html)
 * Quick example -- [Charting good_eats submission over time](http://SEL-Columbia.github.com/formhub.R/demo/Good_Eats_Example.html)
 * Making maps -- [Making maps with North Ghana data](http://SEL-Columbia.github.com/formhub.R/demo/Water_Points_Example.html)
 * Quality control -- [How long did it take to process a survey](http://SEL-Columbia.github.com/formhub.R/demo/How_Long_Example.html)
 * Quality control II -- [A basic tutorial on removing outliers from your data](http://SEL-Columbia.github.com/formhub.R/demo/RemoveOutliers.html)
 * Making Readable Data -- [Replacing "slugs" in data output with survey text](http://SEL-Columbia.github.io/formhub.R/demo/ReadableData.html)
 * Analyzing Responses to Don't Know Questions -- [Analyzing Responses to Don't Know](http://SEL-Columbia.github.io/formhub.R/demo/DontKnowResponses.html)

For most of the examples, I use the [ggplot2](http://ggplot2.org) library, which is an amazing data visualization library worth every minute of your time spent learning it.

Installing formhub.R
--------------------
The package for formhub.R in R is simply called `formhub`. For now, it cannot be installed from CRAN, but using Hadley Wickam's excellent `devtools` package, you can install it directly from github. To install (or upgrade), run the following commands from your R terminal:

    install.packages('devtools') 
    library(devtools)
    install_github("formhub.R", username="SEL-Columbia")
    library(formhub)

And voila! The `formhub` package is installed in your R system! You can check by running:

    help(formhubDownload)
 
Features
--------

At the moment, it has the following features:

 * One command download of formhub.org data -- both public and private
 * (Automatic) casting of data to the right type, based on the type of the input field in XLSform
   * `select one` fields are converted to factors
   * `integer`/`decimal` converted to numerics
   * `date` fields (including `today`), and `datetime` fields (including `start` and `end`) are converted to [lubridate](http://cran.r-project.org/package=lubridate) instants [timezone information is discarded at the moment]
   * TRUE / FALSE fields created out of `select multiple` options are converted into booleans
 * removeColumns convenience function that removes columns based on matching the title with a regular expression
 * 'extra-schema' over-ride. 

For planned features, go to the [issues](https://github.com/SEL-Columbia/formhub.R/issues) page.


