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
  schemadf <- rbind(extraSchema, schema_to_df(schemaJSON))
  # over-ride select items with "extraSchema" -- note this assumes that first found schema element is used

  recastDataFrameBasedOnSchemaDF(dataDataFrame, schemadf)
}


schema_to_df = function(schema, prefix="") {
  ldply(schema[["children"]], function(child) {
      #if (child[["name"]] == "generator_funct_yn") { browser() }
      if (child[["type"]] == "group") {
        schema_to_df(child, child[["name"]])
      } else if (child[["type"]] == "select all that apply") {
        options <- child[["children"]]
        names <- paste(child[["name"]], sapply(options, function(o) o['name']), sep=".")
        labels <- sapply(options, function(o) o['label'])
        data.frame(name=names, label=labels, type="boolean")
      } else {
        nom <- if (prefix == "") { child[["name"]] } else { paste(prefix, child[["name"]], sep=".") }
        data.frame(name=nom, type=child[["type"]], 
                   label=if("label" %in% names(child)) {child[["label"]]} else {child[["name"]]})
      }
  })
}

recastDataFrameBasedOnSchemaDF = function(df, schemadf) {
  # do this by type
  #TODO: refactor
  
  subsetdfbytype <- function(df, types) {
    df[,which(names(df) %in% (subset(schemadf, type %in% types)$name))]
  }
  ints <- subsetdfbytype(df, c("integer", "decimal"))
  bools <- subsetdfbytype(df, c("boolean"))
  cats <- subsetdfbytype(df, c("select one"))
  rest <- df[,which(!names(df) %in% c(names(ints), names(bools), names(cats)))]

  newdf <- colwise(as.numeric)(ints)
  newdf <- cbind(newdf, colwise(as.logical)(bools))
  newdf <- cbind(newdf, colwise(as.factor)(cats))
  newdf <- cbind(newdf, colwise(as.character)(rest))
  newdf  
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
