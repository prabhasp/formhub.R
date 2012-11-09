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
      if (child[["type"]] == "group") {
        schema_to_df(child, child[["name"]])
      } else if (child[["type"]] == "select all that apply") {
        options <- child[["children"]]
        ldply(options, function(option) { 
          c(name=paste(child[["name"]],option[["name"]], sep="."), 
            label=option[["label"]], type="boolean")
        })   
      } else {
        name <- if (prefix == "") { child[["name"]] } else { paste(prefix, child[["name"]], sep=".") }
        data.frame(name=name, type=child[["type"]], 
                   label=if("label" %in% names(child)) child[["label"]] else child[["name"]])
      }
  })
}

recastRVectorBasedOnFormhubType = function(RVector, FormhubType) {
  if(is.null(FormhubType) || length(FormhubType) == 0) {
    RVector
  } else if (FormhubType == "integer" || FormhubType == "decimal") {
      as.numeric(as.character(RVector))
  } else if (FormhubType == "boolean") {
    as.logical(RVector)
  } else if (as.logical(length(grep("select.one", FormhubType)))) {
      as.factor(RVector)
  } else if (FormhubType == "string" || FormhubType == "text") {
      as.character(RVector)
  } else {
      as.character(RVector)
  }
}

recastDataFrameBasedOnSchemaDF = function(df, schemadf) {
  for (colName in names(df)) {
    #matches <- str_match(colName, '([^.]*)[-.]([^.]*)')
    FormhubType <- subset(schemadf, subset=(name==colName))[[2]]
    #print(paste(colName, " ", FormhubType))
    df[[colName]] <- recastRVectorBasedOnFormhubType(df[[colName]], FormhubType)
  }
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
