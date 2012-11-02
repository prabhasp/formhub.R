library(testthat)

source("~/Code/nga_cleaning_scripts/formhub.R")
# test_dir("~/Code/nga_cleaning_scripts/tests/")
# setwd("~/Code/nga_cleaning_scripts/tests/")

edu_datafile <- "fixtures/edu1.csv"
edu_schemafile <- "fixtures/edu1.json"

edu_rawdf <- read.csv(edu_datafile, na.strings="n/a", stringsAsFactors=FALSE, header=TRUE)
edu_df <- formhubRead(edu_datafile, edu_schemafile)
schema_df <- schema_to_df(fromJSON(edu_schemafile))

test_that("reCastingRVectors Works as expected", {
  expect_true(is.character(edu_rawdf$mylga))
  
  expect_true(is.factor(recastRVectorBasedOnFormhubType(edu_rawdf$mylga, "select one")))
  expect_true(is.character(recastRVectorBasedOnFormhubType(edu_rawdf$ward, "text")))
  expect_true(is.numeric(recastRVectorBasedOnFormhubType(
      edu_rawdf$num_students_total_gender.num_students_female, "integer")))
  expect_true(is.numeric(recastRVectorBasedOnFormhubType(
    edu_rawdf$num_students_total_gender.num_students_female, "integer")))
})


test_that("pre-conversion expectations are correct (if failing, fix dataset)", {
  expect_true(is.character(edu_rawdf$mylga_zone))
  expect_true(is.logical(edu_rawdf$mylga_lga_in_benue)) # all NAs
  expect_true(is.numeric(edu_rawdf$respondent_contact)) # phone numbers
})

test_that("formhubRead converted types properly", {
  expect_true(is.factor(edu_df$mylga_zone)) # type: select one
  expect_true(is.factor(edu_df$mylga_lga_in_benue))

  expect_true(is.numeric(edu_df$num_students_total_gender.num_students_female))
  expect_true(is.numeric(edu_df$num_students_total_gender.num_students_male))

  expect_true(is.character(edu_df$ward)) #type : string / text
  expect_true(is.character(edu_df$community))
  expect_true(is.character(edu_df$respondent_contact)) # type: phone number
  expect_true(is.character(edu_df$photo)) # type: attachment
})

test_that("passing extraSchema in works", {
  extraSchema = setNames(data.frame(rbind(
    c("mylga", "select one", "LGA"), 
    c("mylga_state", "select one", "State"))),
                         c("name", "type", "label"))

  edu_df_with_extra <- formhubRead(edu_datafile, edu_schemafile, extraSchema=extraSchema)
  expect_true(is.factor(edu_df_with_extra$mylga))
  expect_true(is.factor(edu_df_with_extra$mylga_state))
})

test_that("column deletion works", {
  edu_df_dropped <- removecolumns(edu_df, "mylga_.*_in_.*")
  expect_equal(names(edu_df_dropped)[which(str_detect(names(edu_df_dropped), "mylga"))],
               c("mylga_zone", "mylga_state", "mylga"))

  edu_df_dropped <- removecolumns(edu_df, "^num.*")
  expect_false(any(str_detect("num", names(edu_df_dropped))))
  expect_false("num_pry_total_gender.num_pry_female" %in% names(edu_df_dropped))
  expect_true("mylga_lga_in_benue" %in% names(edu_df_dropped))

  edu_df_dropped <- removecolumns(edu_df, c("num.*", "mylga_.*"))
  #expect_false("mylga_lga_in_benue" %in% names(edu_df_dropped))
  expect_false("num_pry_total_gender.num_pry_male" %in% names(edu_df_dropped))
  expect_false("mylga_lga_in_benue" %in% names(edu_df_dropped))
  expect_false("mylga_zone" %in% names(edu_df_dropped))
})

