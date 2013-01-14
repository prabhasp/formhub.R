# prabhas -- # setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
library(RJSONIO)
library(stringr)
library(plyr)

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

  recastDataFrameBasedOnSchemaDF(dataDataFrame, schemadf)
}


schema_to_df = function(schema) {
  schema_to_df_internal = function(schema, prefix="") {
    ldply(schema[["children"]], function(child) {
      nom <- if (prefix == "") { child[["name"]] } else { paste(prefix, child[["name"]], sep=".") }
  
      if (child[["type"]] == "group") {
        schema_to_df_internal(child, nom)
      } else if (child[["type"]] == "select all that apply") {
        options <- child[["children"]]
        names <- paste(child[["name"]], sapply(options, function(o) o['name']), sep=".")
        labels <- sapply(options, function(o) o['label'])
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
  colsOfType <- function(df, types) {
    cols <- c(subset(schemadf, type %in% types)$name)
    cols[cols %in% names(df)]
  }
  reType <- function(typeStrings, reTypeFunc) {
    colsToRetype <- colsOfType(df, typeStrings)
    df[colsToRetype] <<- colwise(reTypeFunc)(df[colsToRetype])
  }
  
  reType(c("integer", "decimal"), as.numeric)
  reType(c("boolean"), as.logical)
  reType(c("select one"), as.factor)
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
