library(testthat)
library(stringr)
library(formhub)

test_dir = ""
#source("~/Code/formhub.R/R/formhub.R");test_file("~/Code/formhub.R/tests/test_formhub_writes.R")

edu_datafile <- str_c(test_dir, "fixtures/edu1.csv")
edu_formfile <- str_c(test_dir, "fixtures/edu1.json")
good_eats_datafile <- str_c(test_dir, "fixtures/good_eats.csv")
good_eats_formfile <- str_c(test_dir, "fixtures/good_eats.json")


# Spatial Point Data Frame Object
test_that("SpatialPointsDataFrame Object can be produced", {
  edu_formhubObj <- formhubRead(edu_datafile, edu_formfile)
  expect_warning(good_eats <- formhubRead(good_eats_datafile, good_eats_formfile))
  
  edu_spdf <- as.SpatialPointsDataFrame(edu_formhubObj)
  expect_is(edu_spdf, "SpatialPointsDataFrame")
  expect_equal(nrow(edu_spdf), nrow(as.data.frame(edu_formhubObj)))
  
  # good eats: WARNINGS should be emitted; we have NAs in the gps field; we'll drop 3 rows
  expect_warning(good_eats_spdf <- as.SpatialPointsDataFrame(good_eats))
  expect_equal(nrow(good_eats_spdf) + 3, nrow(as.data.frame(good_eats)))
  expect_is(good_eats_spdf, "SpatialPointsDataFrame")
})
 
test_that("SpatialPointsDataFrame Object can be produced even when _lat _long columns missing", {
  edu_formhubObj <- formhubRead(edu_datafile, edu_formfile, dropCols="*tude")
  expect_warning(good_eats <- formhubRead(good_eats_datafile, good_eats_formfile, dropCols="*tude"))
          
  edu_spdf <- as.SpatialPointsDataFrame(edu_formhubObj)
  expect_is(edu_spdf, "SpatialPointsDataFrame")
  expect_equal(nrow(edu_spdf), nrow(as.data.frame(edu_formhubObj)))
  
  # good eats: WARNINGS should be emitted; we have NAs in the gps field; we'll drop 3 rows
  expect_warning(good_eats_spdf <- as.SpatialPointsDataFrame(good_eats))
  expect_equal(nrow(good_eats_spdf) + 3, nrow(as.data.frame(good_eats)))
  expect_is(good_eats_spdf, "SpatialPointsDataFrame")
})

test_that("SpatialPointDataFrame Objec conversion returns NA when gps field missing", {
  edu_formhubObj <- formhubRead(edu_datafile, edu_formfile, dropCols="gps")
  expect_warning(good_eats <- formhubRead(good_eats_datafile, good_eats_formfile, dropCols="gps"))
  
  expect_true(is.na(as.SpatialPointsDataFrame(edu_formhubObj)))
  expect_true(is.na(as.SpatialPointsDataFrame(good_eats)))
})
