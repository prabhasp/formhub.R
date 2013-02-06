<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>
An introduction to formhub.R
========================================================

Installation
------------
formhub.R makes is easy to download and work with datasets on [formhub](http://formhub.org). After downloading, formhub.R post-processes your dataset to convert the different columns to the correct type, which it derives from the `type` you specified during the creation of your XLSform. It is distributed as an R package called `formhub` which is not in CRAN yet, and can be installed in the following way:

```
  install.packages(devtools)
  library(devtools)
  install_github("formhub.R", username="modilabs")
```

The `install_github` line will need to be re-run every time you need to update the package, which will be frequent for now, as the package is in early testing. After installation, it can be loaded like you load any other R package:


```r
# library(formhub)
library(devtools)
load_all("~/Code/formhub.R/")
```


Download your first dataset
---------------------------
At this point, we should be ready to get started, and use some of the formhub functions. Likely the most useful, and the most basic, one is called `formhubDownload`. Try typing in `help(formhubDownload)` in your R terminal to see what it does. We'll use it to download the `good_eats` form from mberg's account in formhub, which is a public dataset and doesn't require a password. (To download data from an account with a password, simply pass it along as the third parameter).


```r
good_eats <- formhubDownload("good_eats", "mberg")
```


The formhubData Object
----------------------

Question: what kind of beast did we just download?

```r
str(good_eats)
```

```
## Formal class 'formhubData' [package "formhub"] with 2 slots
##   ..@ data:'data.frame':	38 obs. of  17 variables:
##   .. ..$ submit_data    : POSIXct[1:38], format: "2011-12-30" ...
##   .. ..$ food_type      : Factor w/ 9 levels "caffeination",..: 9 4 8 8 2 1 6 6 6 3 ...
##   .. ..$ description    : chr [1:38] "Turkish burger " "Shisha! " "Fistikli" "Turkish donuts" ...
##   .. ..$ amount         : num [1:38] 2 20 2 2 2.75 2.5 18 8 20 1600 ...
##   .. ..$ rating         : Factor w/ 4 levels "bad","delectible",..: 2 2 2 2 3 3 3 2 2 3 ...
##   .. ..$ comments       : chr [1:38] NA "Should have brought a board game! " "Great staff" "Pure oil and honey" ...
##   .. ..$ risk_factor    : Factor w/ 3 levels "high_risk","low_risk",..: 2 2 2 2 2 2 2 2 2 2 ...
##   .. ..$ food_photo     : chr [1:38] NA "1325234084489.jpg" "1325233793480.jpg" "1325233641666.jpg" ...
##   .. ..$ location_name  : chr [1:38] NA "Kosebasi Nargile " "Tatlic Safa" "Tahiri Osmanli Lokmaci" ...
##   .. ..$ location_photo : chr [1:38] NA "1325234137252.jpg" "1325233817453.jpg" "1325233694501.jpg" ...
##   .. ..$ gps            : chr [1:38] "41.01452808827162 28.97566007450223 57.4000244140625 30.0" NA "41.016706293448806 28.970129443332553 44.9000244140625 40.0" "41.0182375414297 28.97094827145338 39.0 30.0" ...
##   .. ..$ X_gps_latitude : num [1:38] 41 NA 41 41 41 ...
##   .. ..$ X_gps_longitude: num [1:38] 29 NA 29 29 29 ...
##   .. ..$ X_gps_altitude : num [1:38] 57.4 NA 44.9 39 39.9 ...
##   .. ..$ X_gps_precision: num [1:38] 30 NA 40 30 40 20 35 85 60 75 ...
##   .. ..$ imei           : Factor w/ 8 levels "352249053530058",..: 5 5 5 5 5 5 5 5 5 5 ...
##   .. ..$ submit_date    : POSIXct[1:38], format: "2011-12-30" ...
##   ..@ form:'data.frame':	13 obs. of  3 variables:
##   .. ..$ name : chr [1:13] "submit_data" "food_type" "description" "amount" ...
##   .. ..$ type : Factor w/ 7 levels "decimal","geopoint",..: 7 5 6 1 5 6 5 4 6 4 ...
##   .. ..$ label: chr [1:13] "submit_data" "Type of Eat" "Description" "Amount" ...
```


R tell us it something like `Formal class 'formhubData' [package ".GlobalEnv"] with 2 slots`. What this means is that this is a `formhubData` S4 object, and it contains two "slots", `data` and `form`. Basically, a `formhubData` object has both the dataset that you download from formhub (using the csv export), and a reduced representation of your XLSform. The `form` gives us information about the exact question that was asked, and the type of the question asked (was it `text` or `select one`? or was it a `date`?). This lets the formhub.R library change the types of the values to make them right, which is basically the power of this package.

To get just the data we need, which is what we usually need, we can use the `as.data.frame` method, or access the slot `data` directly.


```r
class(good_eats)
```

```
## [1] "formhubData"
## attr(,"package")
## [1] "formhub"
```

```r
class(as.data.frame(good_eats))
```

```
## [1] "data.frame"
```

```r
class(good_eats@data)
```

```
## [1] "data.frame"
```

```r

good_eats_data <- as.data.frame(good_eats)
```



What formhub.R does for you -- type conversions
-----------------------------------------------

So the part where R downloaded your data for you was pretty cool. But there is more to the `formhubDownload` function than just downloading. In the background, the types of each of the columns is converted according to how the data was collected.

```r
str(good_eats_data)
```

```
## 'data.frame':	38 obs. of  17 variables:
##  $ submit_data    : POSIXct, format: "2011-12-30" "2011-12-30" ...
##  $ food_type      : Factor w/ 9 levels "caffeination",..: 9 4 8 8 2 1 6 6 6 3 ...
##  $ description    : chr  "Turkish burger " "Shisha! " "Fistikli" "Turkish donuts" ...
##  $ amount         : num  2 20 2 2 2.75 2.5 18 8 20 1600 ...
##  $ rating         : Factor w/ 4 levels "bad","delectible",..: 2 2 2 2 3 3 3 2 2 3 ...
##  $ comments       : chr  NA "Should have brought a board game! " "Great staff" "Pure oil and honey" ...
##  $ risk_factor    : Factor w/ 3 levels "high_risk","low_risk",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ food_photo     : chr  NA "1325234084489.jpg" "1325233793480.jpg" "1325233641666.jpg" ...
##  $ location_name  : chr  NA "Kosebasi Nargile " "Tatlic Safa" "Tahiri Osmanli Lokmaci" ...
##  $ location_photo : chr  NA "1325234137252.jpg" "1325233817453.jpg" "1325233694501.jpg" ...
##  $ gps            : chr  "41.01452808827162 28.97566007450223 57.4000244140625 30.0" NA "41.016706293448806 28.970129443332553 44.9000244140625 40.0" "41.0182375414297 28.97094827145338 39.0 30.0" ...
##  $ X_gps_latitude : num  41 NA 41 41 41 ...
##  $ X_gps_longitude: num  29 NA 29 29 29 ...
##  $ X_gps_altitude : num  57.4 NA 44.9 39 39.9 ...
##  $ X_gps_precision: num  30 NA 40 30 40 20 35 85 60 75 ...
##  $ imei           : Factor w/ 8 levels "352249053530058",..: 5 5 5 5 5 5 5 5 5 5 ...
##  $ submit_date    : POSIXct, format: "2011-12-30" "2011-12-30" ...
```


Notice that `submit_data` and `submit_date`, both of which were `today` (ie, date) questions in your form, are converted to `POXIXct`, which is R's date type. What does this mean? That means that we can do date-time calculations, for example, to check how long mberg has been collecting data:

```r
max(good_eats_data$submit_data) - min(good_eats_data$submit_date)
```

```
## Time difference of 403 days
```

Over a year... awesome!

Similiarly, things like `select one`, `imei`, and others are converted to factors, `integers` and `decimals` to numbers. Lets see how this compares with if we had simply just read the file as a csv without any type conversions:

```r
good_eats2 <- read.csv("~/Downloads/good_eats_2013_01_24.csv")
str(good_eats2)
```

```
## 'data.frame':	32 obs. of  17 variables:
##  $ submit_data    : Factor w/ 24 levels "2011-12-30","2011-12-31",..: 1 1 1 1 1 6 4 5 8 10 ...
##  $ food_type      : Factor w/ 9 levels "caffeination",..: 9 4 8 8 2 1 6 6 6 3 ...
##  $ description    : Factor w/ 32 levels "A nice cup of black tea to start the day off",..: 29 22 9 31 3 30 25 7 19 24 ...
##  $ amount         : Factor w/ 25 levels "12.0","1200",..: 7 10 7 7 9 8 6 20 10 5 ...
##  $ rating         : Factor w/ 5 levels "bad","delectible",..: 2 2 2 2 3 3 3 2 2 3 ...
##  $ comments       : Factor w/ 18 levels "Better with Pili Pili ",..: 9 13 6 11 14 9 9 9 5 4 ...
##  $ risk_factor    : Factor w/ 4 levels "high_risk","low_risk",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ food_photo     : Factor w/ 28 levels "1325233641666.jpg",..: 26 4 2 1 3 10 8 9 12 14 ...
##  $ location_name  : Factor w/ 26 levels "Addigas ","Ambience Mall, Gurgaon, India",..: 18 15 23 21 12 18 25 24 9 10 ...
##  $ location_photo : Factor w/ 19 levels "1325233694501.jpg",..: 17 4 2 1 3 9 7 8 11 17 ...
##  $ gps            : Factor w/ 28 levels "-0.1051 34.7566 0 0",..: 17 28 18 20 21 10 12 11 24 4 ...
##  $ X_gps_latitude : Factor w/ 28 levels "-0.1051","-1.2572642",..: 17 28 18 20 21 10 12 11 24 4 ...
##  $ X_gps_longitude: Factor w/ 28 levels "-123.15297940999999",..: 13 28 11 12 15 9 7 8 2 23 ...
##  $ X_gps_altitude : Factor w/ 14 levels "-9.899993896484375",..: 11 14 8 5 7 9 10 3 3 3 ...
##  $ X_gps_precision: Factor w/ 19 levels "0","10.0","15.0",..: 6 19 9 6 9 4 7 18 14 17 ...
##  $ imei           : Factor w/ 6 levels "352249053530058",..: 4 4 4 4 4 4 4 4 4 4 ...
##  $ submit_date    : Factor w/ 24 levels "2011-12-30","2011-12-31",..: 1 1 1 1 1 6 4 5 8 10 ...
```


Everything is a factor! Why is that bad? Well, see the plot below for yourself:

```r
library(ggplot2)
qplot(data = good_eats2, x = amount)  # from data read in without formhub.R
```

![plot of chunk plot1](figure/plot11.png) 

```r
qplot(data = good_eats_data, x = amount)  # from data read in using formhub.R
```

![plot of chunk plot1](figure/plot12.png) 



Other functions in formhub.R
----------------------------
Okay, hopefully by now, you are sold on the usefulness of formhub.R, and see some value in it. Since this is a "basics of" document, I'll end by describing a couple of other high-level functions in formhub.R (lower-level functions will be documented over time).

  * `formhubDownload` -- download data directly from formhub by passing form name, username, and password for private data
  * `formhubRead` -- create a formhubData object from pre-downloaded files. The first file argument is the csv file, the second is the form.json file (which you can download from the form page on formhub). Note: unexpected things will happen if the files aren't the right ones. See the full documentation by using `help(formhubRead)`.
  * `replaceHeaderNamesWithLabels` -- get a version of the data where the header row is re-written as the actual question asked.
  
And thats really the gist of it!

What if I get an error while running a function?
------------------------------------------------
This is software that has been tested by only a couple of use cases so far, and writing good code in R is pretty tricky, so there are probably bugs! If you encounter one, please go to your form page, and under "Sharing", give the username "prabhasp" "View" privileges, and file an [issue on github](http://github.com/modilabs/formhub.R/issues)
