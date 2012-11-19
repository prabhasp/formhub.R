library(testthat)

source("~/Code/nga_cleaning_scripts/formhub.R")
test_dir = ""
# test_dir = "~/Code/nga_cleaning_scripts/tests/"
# test_dir("~/Code/nga_cleaning_scripts/tests/")

edu_datafile <- paste(test_dir, "fixtures/edu1.csv", sep="")
edu_schemafile <- paste(test_dir, "fixtures/edu1.json", sep="")
hlt_schemafile <- paste(test_dir, "fixtures/healthschema.json", sep="")

edu_rawdf <- read.csv(edu_datafile, na.strings="n/a", stringsAsFactors=FALSE, header=TRUE)
edu_df <- formhubRead(edu_datafile, edu_schemafile)
edu_schema_df <- schema_to_df(fromJSON(edu_schemafile))
hlt_schema_df <- schema_to_df(fromJSON(hlt_schemafile))


test_that("schemadf is read properly", {
  edu_typeofname <- function(nom) { subset(edu_schema_df, name==nom)$type }
  expect_true(edu_typeofname("start") == "start")
  expect_true(edu_typeofname("km_to_catchment_area") == "integer")
  expect_true(edu_typeofname("num_toilet.num_toilet_boy") == "integer")
  expect_true(edu_typeofname("generator_funct_yn") == "select one")
  expect_true(edu_typeofname("uuid") == "calculate")
  expect_true(edu_typeofname("respondent_name") == "text")
  expect_true(edu_typeofname("respondent_contact") == "string")
  expect_true(edu_typeofname("power_sources.generator") == "boolean")
  
  hlt_typeofname <- function(nom) { subset(hlt_schema_df, name==nom)$type }
  expect_true(hlt_typeofname("not_for_private_1.toilets_available.num_flush_or_pour_flush_piped")=="integer")
})

# test_that("reCastingRVectors Works as expected", {
#   expect_true(is.character(edu_rawdf$mylga))
#   
#   expect_true(is.factor(recastRVectorBasedOnFormhubType(edu_rawdf$mylga, "select one")))
#   expect_true(is.character(recastRVectorBasedOnFormhubType(edu_rawdf$ward, "text")))
# 
#   expect_true(is.numeric(recastRVectorBasedOnFormhubType(
#       edu_rawdf$num_students_total_gender.num_students_female, "integer")))
#   expect_true(is.numeric(recastRVectorBasedOnFormhubType(
#     edu_rawdf$num_students_total_gender.num_students_female, "integer")))
# })


test_that("pre-conversion expectations are correct (if failing, fix dataset)", {
  expect_true(is.character(edu_rawdf$mylga_zone))
  expect_true(is.logical(edu_rawdf$mylga_lga_in_benue)) # all NAs
  expect_true(is.numeric(edu_rawdf$respondent_contact)) # phone numbers
})

test_that("groups and select multiples convert correctly", {
  edu_rawdf2 <- read.csv(edu_datafile) #no na.strings, no stringsAsFactors=FALSE
  #again, just to make sure input is perpared
  expect_true(is.factor(edu_rawdf2$num_pry_total_gender.num_pry_female))
  expect_true(is.factor(edu_rawdf2$num_pry_total_gender.num_pry_male))
  expect_true(is.factor(edu_rawdf2$power_sources.generator))
  expect_true(is.factor(edu_rawdf2$power_sources.grid))
  
  edu_df2 <- recastDataFrameBasedOnSchemaDF(edu_rawdf2, edu_schema_df)
  expect_true(is.numeric(edu_df2$num_pry_total_gender.num_pry_female))
  expect_true(is.numeric(edu_df2$num_pry_total_gender.num_pry_male))
  
  expect_true(is.logical(edu_df2$power_sources.generator))
  expect_true(is.logical(edu_df2$power_sources.grid))
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

test_that("passing na.strings works", {
  na.strings = c("southeast")
  edu_df_wo_SE <- formhubRead(edu_datafile, edu_schemafile, na.strings=na.strings)
  expect_equal(levels(edu_df_wo_SE$mylga_zone), c("northwest"))
})


test_that("column deletion works", {
  edu_df_dropped <- removeColumns(edu_df, "")
  edu_df_dropped <- removeColumns(edu_df_dropped, NA)
  expect_equal(names(edu_df), names(edu_df_dropped))
  
  edu_df_dropped <- removeColumns(edu_df, "mylga_.*_in_.*")
  expect_equal(names(edu_df_dropped)[which(str_detect(names(edu_df_dropped), "mylga"))],
               c("mylga_zone", "mylga_state", "mylga"))

  edu_df_dropped <- removeColumns(edu_df, "^num.*")
  expect_false(any(str_detect("num", names(edu_df_dropped))))
  expect_false("num_pry_total_gender.num_pry_female" %in% names(edu_df_dropped))
  expect_true("mylga_lga_in_benue" %in% names(edu_df_dropped))

  edu_df_dropped <- removeColumns(edu_df, c("num.*", "mylga_.*"))
  #expect_false("mylga_lga_in_benue" %in% names(edu_df_dropped))
  expect_false("num_pry_total_gender.num_pry_male" %in% names(edu_df_dropped))
  expect_false("mylga_lga_in_benue" %in% names(edu_df_dropped))
  expect_false("mylga_zone" %in% names(edu_df_dropped))
})

