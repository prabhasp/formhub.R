library(testthat)
library(stringr)
library(formhub)

test_dir = ""
#source("~/Code/formhub.R/R/formhub.R");test_file("~/Code/formhub.R/tests/test_formhub_funcs.R")

edu_datafile <- str_c(test_dir, "fixtures/edu1.csv")
edu_formfile <- str_c(test_dir, "fixtures/edu1.json")
good_eats_datafile <- str_c(test_dir, "fixtures/good_eats.csv")
good_eats_formfile <- str_c(test_dir, "fixtures/good_eats.json")


# Header Names with Labels works
test_that("replaceHeaderNamesWithLabels basically works", {
  edu_formhubObj <- formhubRead(edu_datafile, edu_formfile)
  good_eats <- formhubRead(good_eats_datafile, good_eats_formfile)
  
  edu_formhubObj_replaced <- replaceHeaderNamesWithLabels(edu_formhubObj)
  good_eats_replaced <- replaceHeaderNamesWithLabels(good_eats)
  
  expect_false(any(names(edu_formhubObj_replaced) == "NA"))
  expect_false(any(names(good_eats_replaced) == "NA"))
  
  expect_true(all(c("submit_date", "imei", "X_gps_longitude","Rating","Type of Eat", "Food Pic")
              %in% names(good_eats_replaced)))
  expect_true(all(c("subscriberid", "uuid", "2.1 Respondent\031s Name", "X_gps_latitude",
                    "LGA", "mylga", "num_ss_total_calc", "TOTAL students in Senior Secondary 1 thugh 3",
                    "23. What type(s) of power sources are available at this school? >> Generator",
                    "23. What type(s) of power sources are available at this school? >> None")
              %in% names(edu_formhubObj_replaced)))
})

test_that("replaceAllNamesWithLabels works on good_eats", {
  good_eats <- formhubRead(good_eats_datafile, good_eats_formfile)
  good_eats_replaced <- replaceAllNamesWithLabels(good_eats)
  
  expect_false(any(names(good_eats_replaced) == "NA"))
  expect_true(all(c("submit_date", "imei", "X_gps_longitude","Rating","Type of Eat", "Food Pic")
                  %in% names(good_eats_replaced)))
  
  expect_false("bad" %in% good_eats_replaced$Rating)
  expect_true("What was I thinking" %in% good_eats_replaced$Rating)
  expect_equivalent(table(good_eats_replaced$Rating)["What was I thinking"], 2)
  
  expect_true("Low Risk" %in% good_eats_replaced[,"Risk Factor"])
})