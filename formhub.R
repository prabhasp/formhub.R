# prabhas -- # setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
library(RJSONIO)
formhubRead  = function(csvfilename, jsonfilename, extraSchema) {
  dataframe <- read.csv(csvfilename, stringsAsFactors=FALSE, header=TRUE)
  schemadf <- schema_to_df(fromJSON(jsonfilename))
  
  # over-ride select items with "extraSchema" -- note this assumes that first found schema element is used
  schemadf <- rbind(extraSchema, schemadf)
  
  recastDataFrameBasedOnSchemaDF(dataframe, schemadf)
}

schema_to_df = function(schema) 
  setNames(ldply(schema[["children"]], function(child) {  
      c(child[["name"]], child[["type"]], if("label" %in% names(child)) child[["label"]] else child[["name"]])
  }), c("name", "type", "label"))

recastRVectorBasedOnFormhubType = function(RVector, FormhubType) {
  if(is.null(FormhubType) || length(FormhubType) == 0) {
    RVector
  } else if (FormhubType == "integer" || FormhubType == "decimal") {
      as.numeric(RVector)
  } else if (as.logical(length(grep("select.one", FormhubType)))) {
      as.factor(RVector)
  } else {
      RVector
  }
}

recastDataFrameBasedOnSchemaDF = function(df, schemadf) {
  setNames(lapply(names(df), function(colName) {
    FormhubType <- subset(schemadf, subset=(name==colName))[[2]]
    print(paste(colName, " ", FormhubType))
    recastRVectorBasedOnFormhubType(df[[colName]], FormhubType)
  }), names(df))
}

removecolumns <- function(df, columnNameRegExpMatcher) {
  df[,-which(str_detect(names(df), columnNameRegExpMatcher))]
  df[,-which(any(str_detect(names(df), columnNameRegExpMatcher)))]
}
