library(RJSONIO)
library(stringr)
library(plyr)
library(RCurl)
library(lubridate)
library(sp)
library(doBy)


setClass("formhubData", representation("data.frame", form="data.frame"), contains="data.frame")

#' Produce a SpatialPointsDataFrame if data has a column of type `gps` or `geopoint`.
#' Otherwise, return NA.
#'
#' @param the formhub object which will be possibly co-erced to a SpatialPointsDataFrame object.
#' @export
#' @return A SpatialPointsDataFrame representation of this formhub Object
#' @examples
#' good_eats_data <- as.data.frame(formhubDownload("good_eats", "mberg"))
#' ge_spdf <- as.SpatialPointsDataFrame(good_eats_data)
#' class(ge_spdf) # "SpatialPointsDataFrame"
as.SpatialPointsDataFrame <- function(formhubObj) {
  gpsfields = subset(formhubObj@form, type %in% c("gps", "geopoint"))$name
  gpsfields = gpsfields[which(gpsfields %in% names(data.frame(formhubObj)))]
  if(length(gpsfields) == 0) NA
  else {
    #TODO: deal with multiple gps questions?
    gpses <- data.frame(formhubObj)[,gpsfields[1]]
    dropRows <- gpses=="NA" | is.na(gpses)
    if(any(dropRows)) {    
      warning(paste("formhub.R: Dropping",table(dropRows)['TRUE'],"rows because GPS not present."))
      gpses <- gpses[!dropRows]
    }
    gpses_split <- apply(str_split_fixed(gpses, " ", 3)[,c(2,1)], 2, FUN=as.numeric)
    SpatialPointsDataFrame(gpses_split, data.frame(formhubObj)[!dropRows,],
                           proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
  }
}

#' Get a new dataframe, where the header contains the full questions as opposed to slugs.
#'
#' formhub Objects have some data, as well as the form, which documents how
#' the data was obtained through a survey. The data, by default, is represented by
#' slugs, ie, items in the `name` column in the original xfrom. This function
#' replaces slugs in the header with the actual question text.
#'
#' @param formhubDataObj is the formhub data object whose data slot will be renamed
#' @export
#' @return a new data frames with the column names renamed from `name`s (slugs) to `label`s(full questions)
#' @examples
#' good_eats <- formhubDownload("good_eats", "mberg")
#' names(good_eats) # still slugged names
#' summary(good_eats$rating)
#' full_header_good_eats <- replaceHeaderNamesWithLabels(good_eats)
#' names(full_header_good_eats) # not slugged anymore
#' summary(full_header_good_eats$Rating) # but data is the same
replaceHeaderNamesWithLabels <- function(formhubDataObj) {
  newNames <- lapply(names(formhubDataObj), function(nm) {
    index <- which(formhubDataObj@form$name==nm)
    if(length(index) > 0) {
       formhubDataObj@form$label[[index]]
    } else {
      nm
    }
  })
  setNames(formhubDataObj, newNames)
}

#' Get a new dataframe, where all 'name's are replaced with full labels.
#'
#' formhub Objects have some data, as well as the form, which documents how
#' the data was obtained through a survey. The data, by default, is represented by
#' slugs, ie, items in the `name` column in the original xfrom. This function
#' replaces slugs in the header with actual question text, and replaces slugs in
#' select one options with the actual resposne text.
#'
#' @param formhubDataObj is the formhub data object whose data slot will be renamed
#' @export
#' @return a new data frames with the column names, as well as factor values, renamed from `name`s (slugs) to `label`s(full questions)
#' @examples
#' good_eats <- formhubDownload("good_eats", "mberg")
#' names(good_eats) # still slugged names
#' summary(good_eats$rating)
#' good_eats_readable <- replaceAllNamesWithLabels(good_eats)
#' names(good_eats_readable) # not slugged anymore
#' summary(good_eats_readable$`Risk Factor`) # not slugged anymore.
replaceAllNamesWithLabels <- function(formhubDataObj) {
  data <- data.frame(formhubDataObj)
  form <- formhubDataObj@form
  row.names(form) <- form$name
  
  l_ply(form[form$type == 'select one',]$name, function(ln) {
    ol <- fromJSON(form[ln,'options'])
    old <- ldply(ol, rbind)
    row.names(old) <- old$name
    levels(data[,ln]) <<- recodeVar(levels(data[,ln]), 
                as.character(old$name), as.character(old$label), default=NA, keep.na=T)
  })
  replaceHeaderNamesWithLabels(new("formhubData", data, form=form))
}

#' Download data from formhub.
#'
#' This function downloads a dataset for the given form and username, and produces a 
#' formhubData Object.
#'
#' @param formName formname on formhub.org for which we download the data
#' @param uname formhub.org username
#' @param pass formhub.org password, if the data and/or form is private
#' @param ... other parameters to pass onto formhubRead
#' @export
#' @return formhubDataObj a formhubData Object, with "data" and "form" slots
#' @examples
#' good_eats <- formhubDownload("good_eats", "mberg")
#' good_eats # is a data frame of all the data
#' good_eats@form # is the form for that data, encoded as a dataframe
#' privateData <- formhubDownload("Private_Data_For_Testing", uname="formhub_r", pass="t3st~p4ss")
formhubDownload = function(formName, uname, pass=NA, ...) {
  fUrl <- function(formName, uname, form=F) {
    str_c('http://formhub.org/', uname, '/forms/', formName,
          ifelse(form,'/form.json', '/data.csv'))
  }
  dataUrl = fUrl(formName, uname)
  formUrl = fUrl(formName, uname, form=T)
  
  #TODO -- pre-flight check? below doesn't work; expects 200+ status
  #if(!url.exists(datUrl)) { stop("could not find ", dataUrl)}
  #if(!url.exists(formUrl)) { stop("could not find ", formUrl)}
  
  # get the data, depending on public or not
  dataCSVstr <- ifelse(is.na(pass),
                 getURI(dataUrl),
                 getURI(dataUrl, userpwd=str_c(uname,pass,sep=":"), httpauth = 1L))
  
  # get the form, depending on public or not
  # TODO: situations where data is public, form is not
  formJSON <- ifelse(is.na(pass),
                 getURI(formUrl),
                 getURI(formUrl, userpwd=str_c(uname,pass,sep=":"), httpauth = 1L))
  formhubRead(textConnection(dataCSVstr), formJSON, ...)
}

#' Reads data from a passed csv filename and json filename into a formhubData object.
#'
#' This function creates a formhubData object from two files: a csv data file, and a 
#' json form file. These should both be downloaded from formhub.org for the same form.
#'
#' @param csvfilename filename (or a connection object) that has the formhub data
#' @param jsonfilename filename of a json file (or a connection object) that has the form.json form
#' @param extraFormDF override the form (such as by providing a type for a calculate, a new label, etc.)
#' @param dropCols a regular expression, any column name that matches that regexp will be dropped
#' @param na.strings list of na.strings to be passed onto read.csv (default: "n/a")
#' @export
#' @return formhubDataObj a formhubData Object, with "data" and "form" slots
#' @examples
#' # will need to download data.csv and form.json for a specific form on formhub, for below, download
#' http://formhub.org/mberg/forms/good_eats/data.csv http://formhub.org/mberg/forms/good_eats/form.json
#' good_eats <- formhubRead("~/Downloads/good_eats_2013_05_05.csv", "~/Downloads/good_eats.json")
#' head(good_eats) # is a data frame of all the data
#' good_eatsX <- formhubRead("~/Downloads/good_eats_2013_05_05.csv", "~/Downloads/good_eats.json",
#'              extraFormDF=data.frame(name="imei", type="integer", label="IMEI"))
#' good_eatsX@form # note that imei is now slated as type "integer" instead of type "imei"
#' str(good_eatsX$imei) # also notice that it is numeric instead of a factor
#' good_eatsWO <- formhubRead("~/Downloads/good_eats_2013_05_05.csv", "~/Downloads/good_eats.json",
#'              dropCols="submit*")
#' names(good_eatsWO) # notice how submit_date and submit_date are no longer there
#' good_eatsNA <- formhubRead("~/Downloads/good_eats_2013_05_05.csv", "~/Downloads/good_eats.json",
#'              na.strings=c("999"))
#' good_eatsNA$amount # notice that the value that was 999 is now missing. This is helpful when using values such
#'                    # as 999 to indicate no data
formhubRead  = function(csvfilename, jsonfilename, extraFormDF=data.frame(), dropCols="", na.strings=c("n/a"),
                        convert.dates=TRUE) {
  dataframe <- read.csv(csvfilename, stringsAsFactors=FALSE, header=TRUE, na.strings=na.strings)
  
  formhubCast(dataframe, form_to_df(fromJSON(jsonfilename)), extraFormDF=extraFormDF, dropCols=dropCols,
              convert.dates=convert.dates)
}

#' Casts a dataframe to the right types based on a form-dataframe.
#'
#' This function creates a formhubData object based on a pair of dataframes: the data
#' and the form that describes the data. The column names of the data match with the "name" column of 
#' the form, and the "type" column in the form provide information for type conversion.
#'
#' @param dataDF data
#' @param formDF form data frame. See format above.
#' @param extraFormDF override the form (such as by providing a type for a calculate, a new label, etc.)
#' @param dropCols a regular expression, any column name that matches that regexp will be dropped
#' @return formhubDataObj a formhubData Object, with "data" and "form" slots
#' @examples
#' 
#' See examples under formhubRead; this should be used through formhubRead in almost all cases
formhubCast  = function(dataDF, formDF, extraFormDF=data.frame(), dropCols="", convert.dates=TRUE) {
  dataDF <- removeColumns(dataDF, dropCols)

  extraFormDF <- colwise(as.character)(extraFormDF)
  formDF <- rbind.fill(extraFormDF, formDF)
  formDF <- formDF[!duplicated(formDF$name),]
  
  new("formhubData", recastDataFrameBasedOnFormDF(dataDF, formDF, convert.dates=convert.dates),
                     form=formDF)
}

#' Converts formhub form.json format to dataframe format. Dataframe has name, type, label columns.
#'
#' @param formJSON formJSON that has been freshly read from JSON using JSONIO's fromJSON function.
#' @return formDF
#' @examples
#' good_eats_form_df <- form_to_df(fromJSON("~/Downloads/good_eats.json"))
form_to_df = function(formJSON) {
  form_to_df_internal = function(formJSON, prefix="") {
    ldply(formJSON[["children"]], function(child) {
      nom <- if (prefix == "") { child[["name"]] } else { paste(prefix, child[["name"]], sep=".") }
  
      if (child[["type"]] == "group") {
        form_to_df_internal(child, nom)
      } else if (child[["type"]] == "select all that apply") {

        options <- child[["children"]]        
        nameprefix <- ifelse(prefix=="", child[["name"]], str_c(prefix, child[["name"]], sep="."))
        names <- paste(nameprefix, sapply(options, function(o) o['name']), sep=".")
        
        labels <- sapply(options, function(o) { paste( child[["label"]], o['label'], sep=" >> ")})
        data.frame(name=names, label=labels, type="boolean", options=NA, stringsAsFactors=F)
      } else if (child[["type"]] == "select one") {
        data.frame(name=nom, type=child[["type"]], options=toJSON(child$children),
                   label=if("label" %in% names(child)) {child[["label"]]} else {child[["name"]]},
                   stringsAsFactors=F)
      } else {
        data.frame(name=nom, type=child[["type"]], options=NA,
                   label=if("label" %in% names(child)) {child[["label"]]} else {child[["name"]]},
                   stringsAsFactors=F)
      }
    })
  }
  df <- form_to_df_internal(formJSON)
  df
}

#' Casts a dataframe to the right types based on a form-dataframe. Used by formhubCast
#'
#' @param df data
#' @param formdf form data frame. See format on formhubCast.
#' @return df re-casted data frame.
#' @examples
#' 
#' #See examples under formhubRead; this should be used through formhubRead in almost all cases
recastDataFrameBasedOnFormDF = function(df, formdf, convert.dates=TRUE) {
  # do this by type
  #TODO: refactor
  stopifnot(is.character(formdf$name))
  # re-type everything in df of type in types with reTypeFunc
  reTypeColumns <- function(types, reTypeFunc) { 
    cols <- c(subset(formdf, type %in% types)$name)
    colsToReType <- unique(cols[cols %in% names(df)])
    suppressWarnings(suppressMessages(
      df[colsToReType] <<- colwise(reTypeFunc)(df[colsToReType])
    ))
  }
  # lubridate doesn't handle ISO 8601 datetimes yet, so we just chuck the timezone info
  iso8601DateTimeConvert <- function(x) { ymd_hms(str_extract(x, '^[^+Z]*(T| )[^+Z-]*')) }
  
  # some formhub dates come in the format 2011-04-24T00:20:00.000000
  iso8601DateConvert <- function(x) { ymd(str_extract(x, '^[^T]*')) }
  
  reTypeColumns(c("integer", "decimal"), as.numeric)
  reTypeColumns(c("boolean"), as.logical)
  reTypeColumns(c("select one", "imei", "subscriberid", "simserial", "deviceid", "phonenumber"), as.factor)
  if(convert.dates) {
    reTypeColumns(c("date", "today"), iso8601DateConvert)
    reTypeColumns(c("start", "end", "datetime"), iso8601DateTimeConvert)
  }
  df
}

#' Helper function to remove columns from data based on reg-exp matching. Also takes list of strings.
#'
#' @param df data
#' @param columnNameRegExpMatcher pattern(s) to match to columns; matched columns are dropped.
#' @return a smaller data frame.
#' @examples
#' good_eats_df <- formhubDownload("good_eats", "mberg")
#' names(good_eats_form_df) # note it includes submit_date and submit_data both
#' names(removeColumns(good_eats_form_df, "submit*")) # both of which are gone now
#' names(removeColumns(good_eats_form_df, c("submit*", "_gps*")) # you can pass a list of regular expressions
removeColumns <- function(df, columnNameRegExpMatcher) {
  if (columnNameRegExpMatcher=="" || is.na(columnNameRegExpMatcher)) { 
    df 
  } else {
    orMatcher <- paste(columnNameRegExpMatcher, collapse="|")
    df[,-which(str_detect(names(df), orMatcher))]
  }
}


