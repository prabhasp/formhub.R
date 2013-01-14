library(testthat)

source("~/Code/nga_cleaning_scripts/formhub.R")

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
  #   data <- formhubDownload("Private_Data_For_Testing", uname="formhubR", 
  #                           pass="t3st~p4ss")
  expect_true("submit_date" %in% names(data))
  expect_true(nrow(data) > 1)
})



test_that("downloading public data (with private schema) falls back gracefully", {
  good_eats <- formhubDownload("Public_Data_Private_Schema", uname="formhubR")
  expect_true("start" %in% names(data))
  expect_true(nrow(data) > 1)  
})
