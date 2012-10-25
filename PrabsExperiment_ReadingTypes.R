# prabhas -- # setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
education <- read.csv("raw_data/Education_05_06_2012_2012_10_05_09_56_27.csv", stringsAsFactors=FALSE, header = TRUE)
library(RJSONIO)
education_schema <- fromJSON("raw_data/json_schemas/Education_05_06_2012.json")
# child <- education_schema$children[[12]]
schema_to_df = function(schema) 
  setNames(ldply(schema[["children"]], function(child) {  
      c(child[["type"]], child[["name"]], if("label" %in% names(child)) child[["label"]] else child[["name"]])  
  }), c("type", "name", "label"))
recastColumnInRBasedOnFormhubType = function(RVector, FormhubType) {
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
recastDataFrameBasedOnFormhubSchema = function(df, FormhubSchema) {
  schemadf <- schema_to_df(FormhubSchema) # to-do: optimize away subset below with a list
  setNames(lapply(names(df), function(colName) {
    FormhubType <- subset(schemadf, subset=(name==colName))[[1]]
    recastColumnInRBasedOnFormhubType(df[[colName]], FormhubType)
  }), names(df))
}

edu <- recastDataFrameBasedOnFormhubSchema(education, education_schema)