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
  #stopifnot(is.character(schemadf$name) & is.character(schemadf$label) & is.character(extraSchema$name)
  #          & is.character(extraSchema$label))

  recastDataFrameBasedOnSchemaDF(dataDataFrame, schemadf)
}


schema_to_df = function(schema, prefix="") {
  df <- ldply(schema[["children"]], function(child) {
      nom <- if (prefix == "") { child[["name"]] } else { paste(prefix, child[["name"]], sep=".") }
  
      if (child[["type"]] == "group") {
        schema_to_df(child, nom)
      } else if (child[["type"]] == "select all that apply") {
        options <- child[["children"]]
        names <- paste(child[["name"]], sapply(options, function(o) o['name']), sep=".")
        labels <- sapply(options, function(o) o['label'])
        data.frame(name=names, label=labels, type="boolean")
      } else {
        data.frame(name=nom, type=child[["type"]], 
                   label=if("label" %in% names(child)) {child[["label"]]} else {child[["name"]]})
      }
  })
  df$type <- as.factor(df$type)
  df$name <- as.character(df$name)
  df$label <- as.character(df$label)
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
  
  ints <- colsOfType(df, c("integer", "decimal"))
  bools <- colsOfType(df, c("boolean"))
  cats <- colsOfType(df, c("select one"))
  rest <- df[,which(!names(df) %in% c(names(ints), names(bools), names(cats)))]
  
  df[names(ints)] <- colwise(as.numeric)(df[names(ints)])
  df[names(bools)] <- colwise(as.logical)(df[names(bools)])
  df[names(cats)] <- colwise(as.factor)(df[names(cats)])
  df[names(rest)] <- colwise(as.character)(df[names(rest)])
  
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
