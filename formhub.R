library(RJSONIO)
library(stringr)
library(plyr)
library(RCurl)
library(lubridate)


setClass("formhubData", representation(data="data.frame", schema="data.frame"))

#' Get a new dataframe, where the header contains the full questions as opposed to slugs.
#'
#' formhub Objects have some data, as well as the schema, which documents how
#' the data was obtained through a survey. The data, by default, is represented by
#' slugs, ie, items in the `name` column in the original xfrom. This function
#' replaces slugs in the header with the actual question text.
#'
#' @param formhubDataObj is the formhub data object whose data slot will be renamed
#' @export
#' @return a new data frames with the column names renamed from `name`s (slugs) to `label`s(full questions)
#' @examples
#' good_eats <- formhubDownload("good_eats", "mberg")
#' names(good_eats@data) # still slugged names
#' summary(good_eats@data$rating)
#' full_header_good_eats <- replaceHeaderNamesWithLabels(good_eats)
#' names(full_header_good_eats) # not slugged anymore
#' summary(full_header_good_eats$Rating) # but data is the same
replaceHeaderNamesWithLabels <- function(formhubDataObj) {
  newNames <- lapply(names(formhubDataObj@data), function(n) {
    trySelectAllReplace <- function(name) {
      index <- which(formhubDataObj@schema$name==n)
    }
    
    index <- which(formhubDataObj@schema$name==n)
    ifelse(length(index) == 0,
           # maybe its a select-all
           trySelectAllReplace(n),
           formhubDataObj@schema$label[[index]])
  })
  setNames(formhubDataObj@data, newNames)
}

#' Download data from formhub.
#'
#' This function downloads a dataset for the given form and username, and produces a 
#' formhubData Object.
#'
#' @param formName formname on formhub.org for which we download the data
#' @param uname formhub.org username
#' @param pass formhub.org password, if the data and/or schema is private
#' @param ... other parameters to pass onto formhubRead
#' @export
#' @return formhubDataObj a formhubData Object, with "data" and "schema" slots
#' @examples
#' good_eats <- formhubDownload("good_eats", "mberg")
#' good_eats@data # is a data frame of all the data
#' good_eats@schema # is the schema for that data, encoded as a dataframe
#' privateData <- formhubDownload("Private_Data_For_Testing", uname="formhub_r", pass="t3st~p4ss")
formhubDownload = function(formName, uname, pass=NA, ...) {
  fUrl <- function(formName, uname, schema=F) {
    str_c('http://formhub.org/', uname, '/forms/', formName,
          ifelse(schema,'/form.json', '/data.csv'))
  }
  dataUrl = fUrl(formName, uname)
  schemaUrl = fUrl(formName, uname, schema=T)
  
  #TODO -- pre-flight check? below doesn't work; expects 200+ status
  #if(!url.exists(datUrl)) { stop("could not find ", dataUrl)}
  #if(!url.exists(schemaUrl)) { stop("could not find ", schemaUrl)}
  
  # get the data, depending on public or not
  dataCSVstr <- ifelse(is.na(pass),
                 getURI(dataUrl),
                 getURI(dataUrl, userpwd=str_c(uname,pass,sep=":"), httpauth = 1L))
  # get the schema, depending on public or not
  # TODO: situations where data is public, schema is not
  schemaJSON <- ifelse(is.na(pass),
                 getURI(schemaUrl),
                 getURI(schemaUrl, userpwd=str_c(uname,pass,sep=":"), httpauth = 1L))
  formhubRead(textConnection(dataCSVstr), schemaJSON)
}

formhubRead  = function(csvfilename, jsonfilename, extraSchema=data.frame(), dropCols="", na.strings=c("n/a")) {
  dataframe <- read.csv(csvfilename, stringsAsFactors=FALSE, header=TRUE, na.strings=na.strings)
  
  formhubCast(dataframe, fromJSON(jsonfilename), extraSchema=extraSchema, dropCols=dropCols)
}

formhubCast  = function(dataDataFrame, schemaJSON, extraSchema=data.frame(), dropCols="") {
  dataDataFrame <- removeColumns(dataDataFrame, dropCols)

  # over-ride select items with "extraSchema" -- note this assumes that first found schema element is used
  extraSchema$name <- as.character(extraSchema$name)
  extraSchema$label <- as.character(extraSchema$label)
  schemadf <- rbind(extraSchema, schema_to_df(schemaJSON))

  new("formhubData", data=recastDataFrameBasedOnSchemaDF(dataDataFrame, schemadf),
                     schema=schemadf)
}


schema_to_df = function(schema) {
  schema_to_df_internal = function(schema, prefix="") {
    ldply(schema[["children"]], function(child) {
      nom <- if (prefix == "") { child[["name"]] } else { paste(prefix, child[["name"]], sep=".") }
  
      if (child[["type"]] == "group") {
        schema_to_df_internal(child, nom)
      } else if (child[["type"]] == "select all that apply") {

        options <- child[["children"]]        
        nameprefix <- ifelse(prefix=="", child[["name"]], str_c(prefix, child[["name"]], sep="."))
        names <- paste(nameprefix, sapply(options, function(o) o['name']), sep=".")
        
        labels <- sapply(options, function(o) { paste( child[["label"]], o['label'], sep=" >> ")})
        data.frame(name=names, label=labels, type="boolean", stringsAsFactors=F)
      } else {
        data.frame(name=nom, type=child[["type"]], 
                   label=if("label" %in% names(child)) {child[["label"]]} else {child[["name"]]},
                   stringsAsFactors=F)
      }
    })
  }
  df <- schema_to_df_internal(schema)
  df$type <- as.factor(df$type)
  df
}

recastDataFrameBasedOnSchemaDF = function(df, schemadf) {
  # do this by type
  #TODO: refactor
  stopifnot(is.character(schemadf$name))
  # re-type everything in df of type in types with reTypeFunc
  reTypeColumns <- function(types, reTypeFunc) { 
    cols <- c(subset(schemadf, type %in% types)$name)
    colsToReType <- unique(cols[cols %in% names(df)])
    df[colsToReType] <<- colwise(reTypeFunc)(df[colsToReType])
  }
  # lubridate doesn't handle ISO 8601 datetimes yet, so we just chuck the timezone info
  iso8601DateTimeConvert <- function(x) { ymd_hms(str_extract(x, '^[^+Z]*'), quiet=TRUE) }
  
  # some formhub dates come in the format 2011-04-24T00:20:00.000000
  iso8601DateConvert <- function(x) { ymd(str_extract(x, '^[^T]*'), quiet=TRUE) }
  
  reTypeColumns(c("integer", "decimal"), as.numeric)
  reTypeColumns(c("boolean"), as.logical)
  reTypeColumns(c("select one", "imei"), as.factor)
  reTypeColumns(c("date", "today"), iso8601DateConvert)
  reTypeColumns(c("start", "end", "datetime"), iso8601DateTimeConvert)
  df
}

# Remove Column names passed in as regex. If list of strings passed in, match any name
removeColumns <- function(df, columnNameRegExpMatcher) {
  if (columnNameRegExpMatcher=="" || is.na(columnNameRegExpMatcher)) { 
    df 
  } else {
    orMatcher <- paste(columnNameRegExpMatcher, collapse="|")
    df[,-which(str_detect(names(df), orMatcher))]
  }
}
