library(testthat)

source("~/Code/formhub.R/formhub.R")

test_that("downloading public data (with public schema) works", {
  good_eats <- formhubDownload("good_eats", uname="mberg")
  expect_true("risk_factor" %in% names(good_eats))
  expect_true(nrow(good_eats) > 1)
  
  # public data with a password also works, no matter what the password
  good_eats <- formhubDownload("good_eats", uname="mberg", pass="boguspassword")
  expect_true("risk_factor" %in% names(good_eats))
  expect_true(nrow(good_eats) > 1)
  
})

test_that("downloading private data works", {
  data <- formhubDownload("Simple_Water_Points", uname="prabhasp", pass="testingbamboo")
     #data <- formhubDownload("Private_Data_For_Testing", uname="formhub_r", 
     #                      pass="t3st~p4ss")
  expect_true("submit_date" %in% names(data))
  expect_true(nrow(data) > 0)
  expect_true(is.instant(data$submit_date))
  expect_true(is.factor(data$functional))
  expect_true(is.character(data$wp_id))
})


test_that("downloading public data (with private schema) falls back gracefully", {
  data <- formhubDownload("Public_Data_Private_Schema", uname="formhub_r")
  expect_true(nrow(data) > 0)
  expect_true(is.instant(data$submit_date))
  expect_true(is.factor(data$functional))
  expect_true(is.character(data$wp_id))
})
