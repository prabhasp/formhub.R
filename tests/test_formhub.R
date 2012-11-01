library(testthat)

source("~/Code/nga_cleaning_scripts/formhub.R")
# test_dir("~/Code/nga_cleaning_scripts/tests/")
# setwd("~/Code/nga_cleaning_scripts/tests/")


edu_datafile <- "fixtures/edu1.csv"
edu_schemafile <- "fixtures/edu1.json"
extraSchema = setNames(data.frame(rbind(
                    c("mylga", "select one", "LGA"), 
                    c("mylga_state", "select one", "State"))),
                       c("name", "type", "label"))
edu_df <- formhubRead(edu_datafile, edu_schemafile, extraSchema=extraSchema)
edu_rawdf <- read.csv(edu_datafile, na.strings="n/a", stringsAsFactors=FALSE, header=TRUE)

test_that("reCastingRVectors Works as expected", {
  expect_true(is.character(edu_rawdf$mylga))
  expect_true(is.factor(recastRVectorBasedOnFormhubType(edu_rawdf$mylga, "select one")))
  expect_true(is.character(recastRVectorBasedOnFormhubType(edu_rawdf$ward, "text")))
})


test_that("formhubRead converted types properly", {
  expect_equal(1, 1)
  expect_true(is.factor(edu_df$mylga_zone))
  expect_true(is.factor(edu_df$mylga))
  expect_true(is.factor(edu_df$mylga_state))
  expect_true(is.numeric(edu_df$num_students_total_gender.num_students_female))
  expect_true(is.character(edu_df$ward))
  expect_true(is.character(edu_df$community))
})
