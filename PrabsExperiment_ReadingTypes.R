# prabhas -- # setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
education <- read.csv("raw_data/Education_05_06_2012_2012_10_05_09_56_27.csv", header = TRUE)
library(RJSONIO)
education_schema <- fromJSON("raw_data/json_schemas/Education_05_06_2012.json")
schema_to_dataframe = function(schema) ldply(schema[["children"]], function(child) {
  c(child[["type"]], child[["name"]], if("label" %in% names(child)) child[["label"]] else child[["name"]])
})
head(schema_to_dataframe(education_schema))